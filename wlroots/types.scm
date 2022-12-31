(define-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:use-module (wayland)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-2)
  #:use-module (wayland signal)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign)
                #:select(pointer-address pointer? %null-pointer make-pointer null-pointer? bytevector->pointer))
  #:use-module (bytestructures guile)
  #:export-syntax ( define-wlr-types-class
                    define-wlr-types-class-public)
  #:export (get-pointer
            get-bytestructure
            get-bytevector
            get-event-signal
            .descriptor
            .wrap
            .unwrap))

(define-generic get-event-signal)

(define %bytestructures (make-hash-table 1000))
(define-class <bytestructure-class> (<class>)
  (descriptor #:init-keyword #:descriptor
              #:init-value #f
              #:getter .descriptor)
  (wrap #:init-keyword #:wrap #:getter .wrap)
  (unwrap #:init-keyword #:unwrap #:getter .unwrap))

(define-method (initialize (object <bytestructure-class>) initargs)
  (next-method)
  (and=> (.descriptor object) (cut hash-set! %bytestructures <> object)))

(define (get-field-alist o)
  (cond ((struct-metadata? o) (struct-metadata-field-alist o))
        (else #f)))

(define-inlinable (force-or-nothing o)
  (if (promise? o)
      (force o)
      o))

(define (haneld-pointer-descriptor o)
  (let* ((metadata (bytestructure-descriptor-metadata o))
         (is-pointer? (pointer-metadata? metadata)))
    (values is-pointer? (if is-pointer?
                            (force-or-nothing (pointer-metadata-content-descriptor metadata))
                            o))))
(define-method (compute-get-n-set (class <bytestructure-class>) slot)
  (if (eq? (slot-definition-allocation slot) #:bytestructure)
      (let* ((index (slot-ref class 'nfields))
             (s (slot-definition-options slot))
             (b-name (or (get-keyword #:field-name s #f)
                         (slot-definition-name slot)))
             (descriptor (.descriptor class))
             (metadata (bytestructure-descriptor-metadata descriptor))
             (alist (get-field-alist metadata))
             (field-descriptor
              (assq-ref alist b-name)))
        (unless field-descriptor
          (goops-error "not field name'd `~S' found in class `~S' descriptor " b-name class))
        (slot-set! class 'nfields (+ index 1))
        (let* ((is-pointer? field-descriptor (haneld-pointer-descriptor field-descriptor))
               (handle (if is-pointer? make-pointer identity))
               (b-class (delay (hash-ref %bytestructures field-descriptor #f)))
               (wrap (delay (or (and=> (force b-class) .wrap) identity)))
               (unwrap (delay (or (and=> (force b-class) .unwrap) identity))))
          (list (lambda (o)
                  (let ((f (force wrap))
                        (out (bytestructure-ref (get-bytestructure o) b-name)))
                    (f (handle
                        (cond ((bytestructure? out)
                               (bytestructure->pointer out))
                              (else out))))))
                (lambda (o v)
                  (let ((f (force unwrap)))
                    (bytestructure-set!
                     (get-bytestructure o) b-name
                     (f v)))))))
      (next-method)))

(define-class <wlr-type> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer)
  #:metaclass <bytestructure-class>)

(define-method (initialize (object <wlr-type>) initargs)
  (let ((descriptor(.descriptor (class-of object))))
    (if (get-keyword #:pointer initargs #f)
        (next-method)
        (if descriptor
            (next-method
             object (append
                     (list #:pointer (bytevector->pointer
                                      (make-bytevector
                                       (bytestructure-descriptor-size descriptor))))
                     initargs))
            (goops-error (string-append
                          "You must set a #:descriptor with `define-class' "
                          "or provie #:pointer when `make'"))))))
(define-method (= (f <wlr-type>) (l <wlr-type>))
  (= (.pointer f)
     (.pointer l)))
(define-method (get-bytestructure (obj <wlr-type>))
  (and-let* ((class (class-of obj))
             (descriptor (.descriptor class))
             (unwrap (.unwrap class)))
    (pointer->bytestructure (unwrap obj) descriptor)))
(define-method (get-bytevector (obj <wlr-type>))
  (and=> (get-bytestructure obj) bytestructure-bytevector))
(define-generic get-pointer)
(define-syntax define-wlr-types-class
  (lambda (x)
    (syntax-case x ()
      ((_ name (supers ...) slots ...
          )
       (let ((symbol (syntax->datum #'name))
             (identifier (cut datum->syntax #'name <>)))
         (with-syntax ((rtd (identifier (symbol-append '< symbol '>)))
                       (wrap (identifier (symbol-append 'wrap- symbol )))
                       (unwrap (identifier (symbol-append 'unwrap- symbol)))
                       (is? (identifier (symbol-append symbol '?))))
           #`(begin
               (define wrap
                 (let ((ptr->obj (make-weak-value-hash-table 3000)))
                   (lambda (ptr)
                     (unless (pointer? ptr)
                       (goops-error "In ~S,~S is not a pointer" wrap ptr))
                     (if (null-pointer? ptr)
                         #f
                         (or (hash-ref ptr->obj ptr)
                             (let ((o (make rtd #:pointer ptr)))
                               (hash-set! ptr->obj ptr o)
                               o))))))
               (define (unwrap o)
                 (if o
                     (begin (unless (is? o)
                              (error (string-append
                                      "not a "
                                      (symbol->string (class-name rtd))
                                      " or #f")
                                     o))
                            (.pointer o))
                     %null-pointer))
               (define-class rtd (supers ... <wlr-type>)
                 slots ...
                 #:wrap wrap
                 #:unwrap unwrap)
               (when (.descriptor rtd)
                 (when (assq 'events (struct-metadata-field-alist
                                      (bytestructure-descriptor-metadata
                                       (.descriptor rtd))))
                   (define-method (get-event-signal (b rtd) (signal-name <symbol>))
                     (let* ((bs (get-bytestructure b))
                            (o (bytestructure-ref bs 'events signal-name))
                            (p (bytestructure->pointer o)))
                       (wrap-wl-signal p)))))
               (define-method (get-pointer (o rtd))
                 (let ((u (unwrap o)))
                   (cond ((pointer? u) u)
                         ((bytestructure? u) (bytestructure->pointer u)))))
               (define (is? o) (is-a? o rtd)))))))))

(define-syntax define-wlr-types-class-public
  (lambda (x)
    (syntax-case x ()
      ((_ name others ...)
       (let ((symbol (syntax->datum #'name))
             (identifier (cut datum->syntax #'name <>)))
         (with-syntax ((rtd (identifier (symbol-append '< symbol '>)))
                       (wrap (identifier (symbol-append 'wrap- symbol )))
                       (unwrap (identifier (symbol-append 'unwrap- symbol)))
                       (is? (identifier (symbol-append symbol '?))))
           #`(begin
               (define-wlr-types-class name others ...)
               (export wrap)
               (export unwrap)
               (export is? ))))))))


(define-public %timespec-struct
  (bs:struct
   `((tv-sec ,int64) ;; __time_t
     (tv-nsec ,long) ;; __syscall_slong_t
     )))

(define-public %pixman-box32-struct
  (bs:struct `((x1 ,int32)
               (y1 ,int32)
               (x2 ,int32)
               (y2 ,int32))))
(define-public %pixman-region32-t-struct
  (bs:struct `((extents ,%pixman-box32-struct)
               (data ,(bs:pointer 'void)))))

(define-public %wlr-addon-set-struct
  (bs:struct `((addons ,%wl-list-struct))))

(define-public %wlr-buffer-struct
  (bs:struct `((width ,int)
               (height ,int)
               (dropped ,bool)
               (n-locks ,size_t)
               (accessing-data-ptr ,bool)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (release ,%wl-signal-struct))))
               (addons ,%wlr-addon-set-struct))))

(define-public %wlr-output-mode-struct
  (bs:struct `((width ,int32)
               (height ,int32)
               (refresh ,int32)
               (preferred ,int)
               (link ,%wl-list-struct))))
(define-public %wlr-output-state-struct
  (bs:struct `((committed ,uint32)
               (damage ,%pixman-region32-t-struct)
               (enabled ,bool)
               (scale ,float)
               (transform ,int)
               (adaptive-sync-enabled ,bool)
               (render-format ,uint32)
               (buffer ,(bs:pointer %wlr-buffer-struct))
               (mode-type ,int)
               (mode ,(bs:pointer %wlr-output-mode-struct))
               (custom-mode ,(bs:struct
                              `((width ,int32)
                                (height ,int32)
                                (refresh ,int32))))
               (gamma-lut ,(bs:pointer uint16))
               (gamma-lut-size ,size_t))))


(define-public %wlr-box-struct
  (bs:struct `((x ,int) (y ,int) (width ,int) (height ,int))))

(define-public %wlr-fbox-struct
  (bs:struct `((x ,double)
               (y ,double)
               (width ,double)
               (height ,double))))

(define-public %wlr-surface-state-struct
  (bs:struct `((committed ,uint32)
               (seq ,uint32)
               (buffer ,(bs:pointer %wlr-buffer-struct))
               (dx ,int32)
               (dy ,int32)
               (surface-damage ,%pixman-region32-t-struct)
               (buffer-damage ,%pixman-region32-t-struct)
               (opaque ,%pixman-region32-t-struct)
               (input ,%pixman-region32-t-struct)
               (transform ,int) ;; enum wl_output_transform
               (scale ,int32)
               (frame-callback-list ,%wl-list-struct)
               (width ,int)
               (height ,int)
               (buffer-width ,int)
               (buffer-height ,int)
               (subsurfaces-below ,%wl-list-struct)
               (subsurfaces-above ,%wl-list-struct)
               (viewport ,(bs:struct `((has-src ,bool)
                                       (has-dst ,bool)
                                       (src ,%wlr-fbox-struct)
                                       (dst-width ,int)
                                       (dst-height ,int))))
               (cached-state-locks ,size_t)
               (cached-state-link ,%wl-list-struct))))

(define-public %wlr-surface-struct
  (bs:struct
   `((resource ,(bs:pointer '*))
     (renderer ,(bs:pointer '*))
     (buffer ,(bs:pointer '*))
     (sx ,int)
     (sy ,int)
     (buffer-damage ,%pixman-region32-t-struct)
     (external-damage ,%pixman-region32-t-struct)
     (opaque-region ,%pixman-region32-t-struct)
     (input-region ,%pixman-region32-t-struct)
     (current ,%wlr-surface-state-struct)
     (pending ,%wlr-surface-state-struct)
     (cached ,%wl-list-struct)
     (role ,(bs:pointer '*))
     (role-data ,(bs:pointer 'void))
     (events ,(bs:struct `((commit ,%wl-signal-struct)
                           (new-subsurface ,%wl-signal-struct)
                           (destroy ,%wl-signal-struct))))
     (current-outputs ,%wl-list-struct)
     (addons ,%wlr-addon-set-struct)
     (data ,(bs:pointer 'void))
     (renderer-destroy ,%wl-listener-struct)
     (previous
      ,(bs:struct `((scale ,int32)
                    (transform ,int)
                    (width ,int)
                    (height ,int)
                    (buffer-width ,int)
                    (buffer-height ,int)))))))


(define-public %wlr-output-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (backend ,(bs:pointer (delay %wlr-backend-struct)))
               (display ,(bs:pointer '*))
               (gloabl ,(bs:pointer '*))
               (resources ,%wl-list-struct)
               (name ,cstring-pointer)
               (description ,cstring-pointer)
               (make ,(bs:vector 56 int8))
               (model ,(bs:vector 16 int8))
               (serial ,(bs:vector 16 int8))
               (phys-width ,int32)
               (phys-height ,int32)
               (modes ,%wl-list-struct)
               (current-mode ,(bs:pointer %wlr-output-mode-struct))
               (width ,int32)
               (height ,int32)
               (refresh ,int32)
               (enabled ,bool)
               (scale ,float)
               (subpixel ,int32)
               (transform ,int32)
               (adaptive-sync-status ,int32)
               (render-format ,uint32)
               (needs-frame ,bool)
               (frame-pending ,bool)
               (transform-matrix ,(bs:vector 9 float))
               (non-desktop ,bool)
               (pending ,%wlr-output-state-struct)
               (commit-seq ,uint32)
               (events ,(bs:struct (map (lambda (a)(list a %wl-signal-struct))
                                        `(frame
                                          damage
                                          needs-frame precommit
                                          commit present bind
                                          enable mode description destroy ))))
               (idle-frame ,(bs:pointer '*))
               (idle-done ,(bs:pointer '*))
               (attach-render-locks ,int)
               (cursors ,%wl-list-struct)
               (hardware-cursor ,(bs:pointer '*))
               (cursor-swapchain ,(bs:pointer '*))
               (cursor-front-buffer ,(bs:pointer %wlr-buffer-struct))
               (software-cursor-locks ,int)
               (allocator ,(bs:pointer %wlr-allocator-struct))
               (renderer ,(bs:pointer (delay %wlr-renderer-struct)))
               (swapchain ,(bs:pointer '*))
               (back-buffer ,(bs:pointer %wlr-buffer-struct))
               (display-destroy ,%wl-listener-struct)
               (addons ,%wlr-addon-set-struct)
               (data ,(bs:pointer 'void)))))

(define-public %wlr-xcursor-manager-struct
  (bs:struct `((name ,(bs:pointer *))
               (size ,uint32)
               (scaled-themes ,%wl-list-struct))))

(define-public %wlr-xdg-surface-state-struct
  (bs:struct `((configure-serial ,uint32)
               (geometry ,%wlr-box-struct))))

(define-public %wlr-output-manager-v1-struct
  (bs:struct `((display ,(bs:pointer '*))
               (global ,(bs:pointer '*))
               (resources ,%wl-list-struct)
               (heads ,%wl-list-struct)
               (serial ,uint32)
               (current-configuration-dirty ,int8) ;; bool
               (events ,(bs:struct `((apply ,%wl-signal-struct)
                                     (test ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (display-destroy ,%wl-listener-struct)
               (data ,(bs:pointer 'void)))))
(define-public %wlr-output-layout-struct
  (bs:struct
   `((outputs ,%wl-list-struct)
     (state ,(bs:pointer '*))
     (events ,(bs:struct
               `((add ,%wl-signal-struct)
                 (change ,%wl-signal-struct)
                 (destroy ,%wl-signal-struct))))
     (data ,(bs:pointer 'void)))))

(define-public %wlr-xdg-activation-v1-struct
  (bs:struct `((token-timeout-msec ,uint32)
               (tokens ,%wl-list-struct)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (request-activate ,%wl-signal-struct))))
               (display ,(bs:pointer '*))
               (global ,(bs:pointer '*))
               (display-destroy ,%wl-listener-struct))))

(define-public %wlr-input-device-type-enum
  (bs:enum '((WLR_INPUT_DEVICE_KEYBOARD 0)
             (WLR_INPUT_DEVICE_POINTER 1)
             (WLR_INPUT_DEVICE_TOUCH 2)
             (WLR_INPUT_DEVICE_TABLET_TOOL 3)
             (WLR_INPUT_DEVICE_TABLET_PAD 4)
             (WLR_INPUT_DEVICE_SWITCH 5))))
(define-public %wlr-input-device-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (type ,%wlr-input-device-type-enum)
               (vendor ,unsigned-int)
               (product ,unsigned-int)
               (name ,cstring-pointer)
               (width-mm ,double)
               (height-mm ,double)
               (output-name ,cstring-pointer)
               (union ,(bs:union `((_device ,(bs:pointer 'void))
                                   (keyboard ,(bs:pointer '*))
                                   (pointer ,(bs:pointer '*))
                                   (switch-device ,(bs:pointer '*))
                                   (touch ,(bs:pointer '*))
                                   (tablet ,(bs:pointer '*))
                                   (tablet-pad ,(bs:pointer '*)))))
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))

(define-public %wlr-button-state-enum
  (bs:enum '((WLR_BUTTON_RELEASED 0)
             (WLR_BUTTON_PRESSED 1))))

(define-public %wlr-event-pointer-button-struct
  (bs:struct `((device ,(bs:pointer %wlr-input-device-struct))
               (time-msec ,uint32)
               (button ,uint32)
               (state ,%wlr-button-state-enum))))

(define-public %wlr-event-pointer-motion-struct
  (bs:struct `((device ,(bs:pointer %wlr-input-device-struct))
               (time-msec ,uint32)
               (delta-x ,double)
               (delta-y ,double)
               (unaccel-dx ,double)
               (unaccel-dy ,double))))

(define-public %wlr-event-pointer-motion-absolute-struct
  (bs:struct `((device ,(bs:pointer %wlr-input-device-struct))
               (time-msec ,uint32)
               (x ,double)
               (y ,double))))

(define-public %wlr-event-pointer-axis-struct
  (bs:struct `((device ,(bs:pointer %wlr-input-device-struct))
               (time-msec ,uint32)
               (source ,int)
               (orientation ,int)
               (delta ,double)
               (delta-discrete ,int32))))

(define-public %wlr-backend-struct
  (bs:struct
   `((wlr-backend-impl ,(bs:pointer '*))
     (events ,(bs:struct
               `((destroy ,%wl-signal-struct)
                 (new-input ,%wl-signal-struct)
                 (new-output ,%wl-signal-struct)))))))

(define-public %wlr-renderer-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (rendering ,bool)
               (rendering-with-buffer ,bool)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))
(define-public %wlr-scene-node-state-struct
  (bs:struct `((link ,%wl-list-struct)
               (children ,%wl-list-struct)
               (enabled ,bool)
               (x ,int)
               (y ,int))))
(define-public %wlr-scene-node-type-enum
  (bs:enum '((WLR_SCENE_NODE_ROOT 0)
             (WLR_SCENE_NODE_TREE 1)
             (WLR_SCENE_NODE_SURFACE 2)
             (WLR_SCENE_NODE_RECT 3)
             (WLR_SCENE_NODE_BUFFER 4))))
(define-public %wlr-scene-node-struct
  (bs:struct `((type ,%wlr-scene-node-type-enum)
               (parent ,(bs:pointer (delay %wlr-scene-node-struct)))
               (state ,%wlr-scene-node-state-struct)
               (events
                ,(bs:struct `((destroy ,%wl-signal-struct)))))))
(define-public %wlr-scene-tree-struct
  (bs:struct `((node ,%wlr-scene-node-struct))))

(define-public %wlr-scene-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (outputs ,%wl-list-struct)
               (presentation ,(bs:pointer '*))
               (presentation-destroy ,%wl-listener-struct)
               (peeding-buffers ,%wl-list-struct))))
(define-public %wlr-scene-rect-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (width ,int)
               (height ,int)
               (color ,%color-struct))))

(define-public %wlr-scene-buffer-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (buffer ,(bs:pointer '*))
               (texture  ,(bs:pointer '*))
               (src-box ,%wlr-fbox-struct)
               (dst-width ,int)
               (dst-height ,int)
               (transform ,int) ;; enum wl_output_transform
               (pending-link ,%wl-list-struct))))

(define-public %wlr-presentation-struct
  (bs:struct `((global ,(bs:pointer '*))
               (clock ,int32) ;; clockid_t
               (events ,(bs:struct `((destroy ,%wl-listener-struct))))
               (display-destroy ,%wl-listener-struct))))
(define-public %wlr-presentation-feedback-struct
  (bs:struct `((resources ,%wl-list-struct)
               (output ,%wlr-output-struct)
               (output-committed ,bool)
               (output-commit-seq ,uint32)
               (output-commit ,%wl-listener-struct)
               (output-present ,%wl-listener-struct)
               (output-destroy ,%wl-listener-struct))))
(define-public %wlr-presentation-event-struct
  (bs:struct `((output ,%wlr-output-struct)
               (tv-sec ,uint64)
               (tv-nsec ,uint32)
               (refresh ,uint32)
               (seq ,uint64)
               (flags ,uint32))))

(define-public %wlr-layer-shell-v1-struct
  (bs:struct `((global ,(bs:pointer '*))
               (display-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))
(define-public %wlr-layer-surface-v1-status-struct
  (bs:struct `((committed ,uint32)
               (anchor ,uint32)
               (exclusive-zone ,int32)
               (margin ,(bs:struct `((top ,uint32)
                                     (right ,uint32)
                                     (bottom ,uint32)
                                     (left ,uint32))))
               (keyboard-interactive ,int)
               (desired-width ,uint32)
               (desired-height ,uint32)
               (layer ,int)
               (configure-serial ,uint32)
               (actual-width ,uint32)
               (actual-height ,uint32))))

(define-public %wlr-layer-surface-v1-struct
  (bs:struct `((surface ,(bs:pointer %wlr-surface-struct))
               (output ,(bs:pointer %wlr-output-struct))
               (resource ,(bs:pointer '*))
               (shell ,(bs:pointer %wlr-layer-shell-v1-struct))
               (popups ,%wl-list-struct)
               (namespace ,cstring-pointer)
               (added ,bool)
               (configured ,bool)
               (mapped ,bool)
               (configure-list ,%wl-list-struct)
               (current ,%wlr-layer-surface-v1-status-struct)
               (pending ,%wlr-layer-surface-v1-status-struct)
               (surface-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (map ,%wl-signal-struct)
                                     (unmap ,%wl-signal-struct)
                                     (new-popup ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-xdg-shell-struct
  (bs:struct `((global ,(bs:pointer '*))
               (clients ,%wl-list-struct)
               (popup-grabs ,%wl-list-struct)
               (ping-timeout ,uint32)
               (display-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))
(define-public %wlr-xdg-surface-struct
  (bs:struct `((client ,(bs:pointer '*))
               (resource ,(bs:pointer '*))
               (surface ,(bs:pointer '*))
               (link ,%wl-list-struct)
               (role ,int)
               (union ,(bs:union
                        `((toplevel
                           ,(bs:pointer
                             (delay %wlr-xdg-toplevel-struct)))
                          (popup
                           ,(bs:pointer
                             (delay %wlr-xdg-popup-struct))))))
               (popups ,%wl-list-struct)
               (added ,bool)
               (configured ,bool)
               (mapped ,bool)
               (configure-idle ,(bs:pointer '*))
               (scheduled-serial ,uint32)
               (configure-list ,%wl-list-struct)
               (current ,%wlr-xdg-surface-state-struct)
               (pending ,%wlr-xdg-surface-state-struct)
               (surface-destroy ,%wl-listener-struct)
               (surface-commit ,%wl-listener-struct)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (ping-timeout ,%wl-signal-struct)
                                     (new-popup ,%wl-signal-struct)
                                     (map ,%wl-signal-struct)
                                     (unmap ,%wl-signal-struct)
                                     (configure ,%wl-signal-struct)
                                     (ack-configure ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-xdg-surface-configure-struct
  (bs:struct `((surface ,(bs:pointer %wlr-xdg-surface-struct))
               (link ,%wl-list-struct)
               (serial ,uint32))))

(define-public %wlr-xdg-toplevel-configure-struct
  (bs:struct `(,@(map (lambda (a) (list a int8))
                      '(maximized fullscreen resizing activated))
               ,@(map (lambda (a) (list a uint32))
                      '(titled width height)))))
(define-public %wlr-xdg-toplevel-requested-struct
  (bs:struct `(,@(map (lambda (a) (list a int8))
                      '(maximized minimized fullscreen))
               (fullscreen-output ,(bs:pointer '*))
               (fullscreen-output-destroy ,%wl-listener-struct))))

(define-public %wlr-xdg-positioner-struct
  (bs:struct `((anchor-rect ,%wlr-box-struct)
               (anchor ,int8)
               (gravity ,int8)
               (constraint-adjustment ,int8)
               (size ,(bs:struct `((width ,int32)
                                   (height ,int32))))
               (offset ,(bs:struct `((x ,int32)
                                     (y ,int32)))))))
(define-public %wlr-xdg-popup-struct
  (bs:struct `((base ,(bs:pointer %wlr-xdg-surface-struct))
               (link ,%wl-list-struct)
               (resource ,(bs:pointer '*))
               (committed ,bool)
               (parent ,(bs:pointer %wlr-surface-struct))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (geometry ,%wlr-box-struct)
               (positioner ,%wlr-xdg-positioner-struct)
               (grab-link ,%wl-list-struct))))

(define-public %wlr-xdg-toplevel-state-struct
  (bs:struct `(,@(map (lambda (o) `(,o ,bool))
                      '(maximized fullscreen resizing activated))
               ,@(map (lambda (o) `(,o ,uint32))
                      '(tiled width height max-width max-height min-width min-height)))))
(define-public %wlr-xdg-toplevel-struct
  (bs:struct `((resource ,(bs:pointer '*))
               (base ,(bs:pointer %wlr-xdg-surface-struct))
               (added ,bool)
               (parent ,(bs:pointer %wlr-xdg-surface-struct))
               (parent-unmap ,%wl-listener-struct)
               (current ,%wlr-xdg-toplevel-state-struct)
               (pending ,%wlr-xdg-toplevel-state-struct)
               (scheduled ,%wlr-xdg-toplevel-configure-struct)
               (requested ,%wlr-xdg-toplevel-requested-struct)
               (title ,cstring-pointer)
               (app-id ,cstring-pointer)
               (events ,(bs:struct
                         (map (lambda (a) (list a %wl-signal-struct))
                              '(request-maximize
                                request-fullscreen
                                request-minimize
                                request-move
                                request-resize
                                request-show-window-menu
                                set-parent
                                set-title
                                set-app-id
                                )))))))
(define-public %wlr-xdg-toplevel-resize-event-struct
  (bs:struct `((surface ,(bs:pointer %wlr-xdg-surface-struct))
               (seat ,(bs:pointer (delay %wlr-seat-client-struct)))
               (serial ,uint32)
               (edges ,uint32))))
(define-public %wlr-xdg-toplevel-set-fullscreen-event
  (bs:struct `((surface ,(bs:pointer %wlr-xdg-surface-struct))
               (fullscreen ,bool)
               (output ,(bs:pointer '*)))))

(define-public %wlr-cursor-struct
  (bs:struct `((state ,(bs:pointer '*))
               (x ,double)
               (y ,double)
               (events ,(bs:struct (map (cut cons <> (list %wl-signal-struct))
                                        '(motion
                                          motion-absolute
                                          button
                                          axis
                                          frame
                                          swipe-begin
                                          swipe-update
                                          swipe-end
                                          pinch-begin
                                          pinch-update
                                          pinch-end
                                          hold-begin
                                          hold-end
                                          touch-up
                                          touch-down
                                          touch-motion
                                          touch-cancel
                                          touch-frame
                                          tablet-tool-axis
                                          tablet-tool-proximity
                                          tablet-tool-tip
                                          tablet-tool-button))))
               (data ,(bs:pointer 'void)))))

(define-public WLR_SERIAL_RINGSET_SIZE 128)
(define-public %wlr-serial-range-struct
  (bs:struct `((min-incl ,uint32)
               (max-incl ,uint32))))
(define-public %wlr-serial-ringset-struct
  (bs:struct `((data ,(bs:vector WLR_SERIAL_RINGSET_SIZE %wlr-serial-range-struct))
               (end ,int)
               (count ,int))))
(define-public %wlr-seat-client-struct
  (bs:struct `((client ,(bs:pointer '*))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (link ,%wl-list-struct)

               (resources ,%wl-list-struct)
               (pointers ,%wl-list-struct)
               (keyboards ,%wl-list-struct)
               (touches ,%wl-list-struct)
               (data-devices ,%wl-list-struct)

               (events ,(bs:struct `((destroy ,%wl-signal-struct))))
               (serials ,%wlr-serial-ringset-struct)
               (needs-touch-frame ,bool))))

(define-public WLR_POINTER_BUTTONS_CAP 16)

(define-public %wlr-seat-pointer-state-struct
  (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
               (focused-client ,(bs:pointer %wlr-seat-client-struct))
               (focused-surface ,(bs:pointer %wlr-surface-struct))
               (sx ,double)
               (sy ,double)
               (grab ,(bs:pointer (delay %wlr-seat-pointer-grab-struct)))
               (default-grab ,(bs:pointer (delay %wlr-seat-pointer-grab-struct)))
               (sent-axis-source ,bool)
               (cached-axis-source ,int32)
               (buttons ,(bs:vector WLR_POINTER_BUTTONS_CAP uint32))
               (button-count ,size_t)
               (grab-button ,uint32)
               (grab-serial ,uint32)
               (grab-time ,uint32)
               (surface-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((focus-change ,%wl-signal-struct)))))))
(define-public %wlr-seat-keyboard-state-struct
  (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
               (keyboard ,(bs:pointer '*))
               (focused-client ,(bs:pointer %wlr-seat-client-struct))
               (focused-surface ,(bs:pointer %wlr-surface-struct))
               (keyboard-destroy ,%wl-listener-struct)
               (keyboard-keymap ,%wl-listener-struct)
               (keyboard-repeat-info ,%wl-listener-struct)
               (surface-destroy ,%wl-listener-struct)
               (grab ,(bs:pointer (delay %wlr-seat-keyboard-grab-struct)))
               (default-grab ,(bs:pointer (delay %wlr-seat-keyboard-grab-struct)))
               (events ,(bs:struct `((focus-change ,%wl-signal-struct)))))))
(define-public %wlr-seat-touch-state-struct
  (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
               (touch-points ,%wl-list-struct)
               (grab-serial ,uint32)
               (grab-id ,uint32)
               (grab ,(bs:pointer '*))
               (default-grab ,(bs:pointer '*)))))
(define-public %wlr-seat-keyboard-grab-struct
  (bs:struct `((interface ,(bs:pointer '*))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (data ,(bs:pointer 'void)))))
(define-public %wlr-seat-pointer-grab-struct
  (bs:struct `((interface ,(bs:pointer '*))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (data ,(bs:pointer 'void)))))
(define-public %wlr-seat-touch-grab-struct
  (bs:struct `((interface ,(bs:pointer '*))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (data ,(bs:pointer 'void)))))
(define-public %wlr-seat-request-set-cursor-event-struct
  (bs:struct `((seat-client ,(bs:pointer %wlr-seat-client-struct))
               (surface ,(bs:pointer %wlr-surface-struct))
               (serial ,uint32)
               (hostpot-x ,int32)
               (hostpot-y ,int32))))
(define-public %wlr-seat-struct
  (bs:struct `((global ,(bs:pointer '*))
               (display ,(bs:pointer '*))
               (clients ,%wl-list-struct)
               (name ,cstring-pointer)
               (capabilities ,uint32)
               (accumulated-capabilities ,uint32)
               (last-event ,%timespec-struct)
               (selection-source ,(bs:pointer (delay %wlr-data-source-struct)))
               (selection-serial ,uint32)
               (selection-offers ,%wl-list-struct)
               (primary-selection-source ,(bs:pointer '*))
               (primary-selection-serial ,uint32)

               (drag ,(bs:pointer (delay %wlr-drag-struct)))
               (drag-source ,(bs:pointer (delay %wlr-data-source-struct)))
               (drag-serial ,uint32)
               (drag-offers ,%wl-list-struct)

               (pointer-state ,%wlr-seat-pointer-state-struct)
               (keyboard-state ,%wlr-seat-keyboard-state-struct)
               (touch-state ,%wlr-seat-touch-state-struct)

               (display-destroy ,%wl-listener-struct)
               (selection-source-destroy ,%wl-listener-struct)
               (primary-selection-source-destroy ,%wl-listener-struct)
               (drag-source-destroy ,%wl-listener-struct)

               (events ,(bs:struct (map (cut cons <> (list %wl-signal-struct))
                                        '(pointer-grab-begin
                                          pointer-grab-end
                                          keyboard-grab-begin
                                          keyboard-grab-end
                                          touch-grab-begin
                                          touch-grab-end
                                          request-set-cursor
                                          request-set-selection
                                          set-selection
                                          request-set-primary-selection
                                          set-primary-selection
                                          request-start-drag
                                          start-drag
                                          destroy))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-seat-request-set-selection-event-struct
  (bs:struct `((source ,(bs:pointer (delay %wlr-data-source-struct)))
               (serial ,uint32))))

(define-public %wlr-data-source-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (mime-types ,%wl-array)
               (actions ,int32)
               (accepted ,bool)
               (current-dnd-action ,int32)
               (compositor-action ,uint32)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))
(define-public %wlr-drap-icon-struct
  (bs:struct `((drag ,(bs:pointer (delay %wlr-drag-struct)))
               (surface ,(bs:pointer %wlr-surface-struct))
               (mapped ,bool)
               (events ,(bs:struct `((map ,%wl-signal-struct)
                                     (unmap ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (surface-destroy ,%wl-listener-struct)
               (data ,(bs:pointer 'void)))))
(define-public %wlr-drag-struct
  (bs:struct `((grab-type ,int32)
               (keyboard-grab ,%wlr-seat-keyboard-grab-struct)
               (pointer-grab ,%wlr-seat-pointer-grab-struct)
               (touch-grab ,%wlr-seat-touch-grab-struct)
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (seat-client ,(bs:pointer (delay %wlr-seat-client-struct)))
               (focus-client ,(bs:pointer (delay %wlr-seat-client-struct)))
               (icon ,(bs:pointer %wlr-drap-icon-struct))
               (focus ,(bs:pointer %wlr-surface-struct))
               (source ,(bs:pointer %wlr-data-source-struct))
               (started ,bool)
               (dropped ,bool)
               (cancelling ,bool)
               (grab-touch-id ,int32)
               (touch-id ,int32)
               (events ,(bs:struct `((focus ,%wl-signal-struct)
                                     (motion ,%wl-signal-struct)
                                     (drop ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (source-destroy ,%wl-listener-struct)
               (seat-client-destroy ,%wl-listener-struct)
               (icon-destroy ,%wl-listener-struct)
               (data ,(bs:pointer 'void)))))
