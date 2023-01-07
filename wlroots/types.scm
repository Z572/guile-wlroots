(define-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (wayland global)
  #:use-module (wayland client)
  #:use-module (wayland event-loop)
  #:use-module (wayland resource)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-2)
  #:use-module (wayland signal)
  #:use-module (rnrs bytevectors)
  #:use-module (bytestructure-class)
  #:use-module ((system foreign)
                #:select(pointer-address pointer? %null-pointer make-pointer null-pointer? bytevector->pointer))
  #:use-module (bytestructures guile)
  #:export-syntax ( define-wlr-types-class
                    define-wlr-types-class-public)
  #:re-export (get-pointer
               get-bytestructure
               get-bytevector
               define-bytestructure-class)
  #:export (get-event-signal))

(define-generic get-event-signal)

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

               (define-bytestructure-class rtd (supers ...)
                 #f wrap unwrap is?
                 slots ...)
               (if (.descriptor rtd)
                   (when (assq 'events (struct-metadata-field-alist
                                        (bytestructure-descriptor-metadata
                                         (.descriptor rtd))))
                     (define-method (get-event-signal (b rtd) (signal-name <symbol>))
                       (let* ((bs (get-bytestructure b))
                              (o (bytestructure-ref bs 'events signal-name))
                              (p (bytestructure->pointer o)))
                         (wrap-wl-signal p))))
                   (pk 'rtd rtd))
               )))))))

(define-syntax define-wlr-types-class-public
  (lambda (x)
    (syntax-case x ()
      ((_ name others ...)
       #'(define-wlr-types-class name others ...)))))


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

(define-public %wlr-texture-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (width ,uint32)
               (height ,uint32))))

(define-public %wlr-addon-set-struct
  (bs:struct `((addons ,%wl-list-struct))))

(define-public %wlr-addon-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (owner ,(bs:pointer 'void))
               (link ,%wl-list-struct))))

(define-public %wlr-buffer-struct
  (bs:struct `((width ,int)
               (height ,int)
               (dropped ,stdbool)
               (n-locks ,size_t)
               (accessing-data-ptr ,stdbool)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (release ,%wl-signal-struct))))
               (addons ,%wlr-addon-set-struct))))

(define-public %wlr-client-buffer-struct
  (bs:struct `((base ,%wlr-buffer-struct)
               (texture ,(bs:pointer %wlr-texture-struct))
               (source ,(bs:pointer %wlr-buffer-struct))
               (source-destroy ,%wl-listener-struct)
               (shm-source-format ,uint32))))

(define-public %wlr-output-mode-struct
  (bs:struct `((width ,int32)
               (height ,int32)
               (refresh ,int32)
               (preferred ,int)
               (link ,%wl-list-struct))))
(define-public %wlr-output-state-struct
  (bs:struct `((committed ,uint32)
               (damage ,%pixman-region32-t-struct)
               (enabled ,stdbool)
               (scale ,float)
               (transform ,int)
               (adaptive-sync-enabled ,stdbool)
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

(define-public %wlr-output-cursor-struct
  (bs:struct `((output ,(bs:pointer (delay %wlr-output-struct)))
               (x ,double)
               (y ,double)
               (enabled ,stdbool)
               (visible ,stdbool)
               (width ,uint32)
               (height ,uint32)
               (hostpot-x ,int32)
               (hostpot-y ,int32)
               (link ,%wl-list-struct)
               (texture ,(bs:pointer %wlr-texture-struct))
               (surface ,(bs:pointer (delay %wlr-surface-struct)))
               (surface-commit ,%wl-listener-struct)
               (surface-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))


(define-public %wlr-edges-enum
  (bs:enum '((WLR_EDGE_NONE 0)
             (WLR_EDGE_TOP 1)
             (WLR_EDGE_BOTTOM 2)
             (WLR_EDGE_LEFT 4)
             (WLR_EDGE_RIGHT 8))))

(define-public %wlr-box-struct
  (bs:struct `((x ,int) (y ,int) (width ,int) (height ,int))))

(define-public %wlr-fbox-struct
  (bs:struct `((x ,double)
               (y ,double)
               (width ,double)
               (height ,double))))

(define-public %wlr-allocator-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (buffer-caps ,uint32)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))

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
               (viewport ,(bs:struct `((has-src ,stdbool)
                                       (has-dst ,stdbool)
                                       (src ,%wlr-fbox-struct)
                                       (dst-width ,int)
                                       (dst-height ,int))))
               (cached-state-locks ,size_t)
               (cached-state-link ,%wl-list-struct))))

(define-public %wlr-surface-struct
  (bs:struct
   `((resource ,(bs:pointer %wl-resource-struct))
     (renderer ,(bs:pointer (delay %wlr-renderer-struct)))
     (buffer ,(bs:pointer %wlr-client-buffer-struct))
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
               (display ,(bs:pointer (delay %wl-display-struct)))
               (global ,(bs:pointer %wl-global-struct))
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
               (enabled ,stdbool)
               (scale ,float)
               (subpixel ,int32)
               (transform ,int32)
               (adaptive-sync-status ,int32)
               (render-format ,uint32)
               (needs-frame ,stdbool)
               (frame-pending ,stdbool)
               (transform-matrix ,(bs:vector 9 float))
               (non-desktop ,stdbool)
               (pending ,%wlr-output-state-struct)
               (commit-seq ,uint32)
               (events ,(bs:struct (map (lambda (a)(list a %wl-signal-struct))
                                        `(frame
                                          damage
                                          needs-frame precommit
                                          commit present bind
                                          enable mode description destroy ))))
               (idle-frame ,(bs:pointer %wl-event-source-struct))
               (idle-done ,(bs:pointer %wl-event-source-struct))
               (attach-render-locks ,int)
               (cursors ,%wl-list-struct)
               (hardware-cursor ,(bs:pointer %wlr-output-cursor-struct))
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
  (bs:struct `((name ,(bs:pointer '*))
               (size ,uint32)
               (scaled-themes ,%wl-list-struct))))

(define-public %wlr-xdg-surface-state-struct
  (bs:struct `((configure-serial ,uint32)
               (geometry ,%wlr-box-struct))))

(define-public %wlr-output-manager-v1-struct
  (bs:struct `((display ,(bs:pointer (delay %wl-display-struct)))
               (global ,(bs:pointer (delay %wl-global-struct)))
               (resources ,%wl-list-struct)
               (heads ,%wl-list-struct)
               (serial ,uint32)
               (current-configuration-dirty ,int8) ;; stdbool
               (events ,(bs:struct `((apply ,%wl-signal-struct)
                                     (test ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (display-destroy ,%wl-listener-struct)
               (data ,(bs:pointer 'void)))))

(define-public %wlr-output-configuration-v1-struct
  (bs:struct `((heads ,%wl-list-struct)
               (manager ,(bs:pointer %wlr-output-manager-v1-struct))
               (serial ,uint32)
               (finalized ,stdbool)
               (finished ,stdbool)
               (resource ,(bs:pointer %wl-resource-struct)))))

(define-public %wlr-output-head-v1-state-struct
  (bs:struct `((output ,(bs:pointer %wlr-output-struct))
               (enabled ,stdbool)
               (mode ,(bs:pointer %wlr-output-mode-struct))
               (custom-mode ,(bs:struct `((width ,int32)
                                          (height ,int32)
                                          (refresh ,int32))))
               (x ,int32)
               (y ,int32)
               (transform ,int32) ;; enum wl_output_transform;
               (scale ,float))))

(define-public %wlr-output-head-v1-struct
  (bs:struct `((state ,%wlr-output-head-v1-state-struct)
               (manager ,(bs:pointer %wlr-output-manager-v1-struct))
               (link ,%wl-list-struct)
               (resources ,%wl-list-struct)
               (mode-resources ,%wl-list-struct)
               (output-destroy ,%wl-listener-struct))))

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
               (display ,(bs:pointer (delay %wl-display-struct)))
               (global ,(bs:pointer %wl-global-struct))
               (display-destroy ,%wl-listener-struct))))

(define-public %wlr-xdg-activation-token-v1-struct
  (bs:struct `((activation ,(bs:pointer %wlr-xdg-activation-v1-struct))
               (surface ,(bs:pointer %wlr-surface-struct) )
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (serial ,uint32)
               (app-id ,cstring-pointer)
               (link ,%wl-list-struct)
               (data ,(bs:pointer 'void))
               (events ,(bs:struct `((destroy ,%wl-signal-struct))))
               (token ,cstring-pointer)
               (resource ,(bs:pointer %wl-resource-struct))
               (timeout ,(bs:pointer %wl-event-source-struct))
               (seat-destroy ,%wl-listener-struct)
               (surface-destroy ,%wl-listener-struct))))

(define-public %wlr-xdg-activation-v1-request-activate-event-struct
  (bs:struct `((activation ,(bs:pointer %wlr-xdg-activation-v1-struct))
               (token ,(bs:pointer %wlr-xdg-activation-token-v1-struct))
               (surface ,(bs:pointer %wlr-surface-struct)))))

(define-public WLR_LED_COUNT 3)
(define-public WLR_MODIFIER_COUNT 8)
(define-public WLR_KEYBOARD_KEYS_CAP 32)

(define-public xkb_led_index_t uint32)
(define-public xkb_mod_index_t uint32)
(define-public xkb_mod_mask_t uint32)

(define-public %wlr-keyboard-modifiers-struct
  (bs:struct `((depressed ,xkb_mod_mask_t)
               (latched ,xkb_mod_mask_t)
               (locked ,xkb_mod_mask_t)
               (group ,xkb_mod_mask_t))))
(define-public %wlr-keyboard-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (group ,(bs:pointer '*))
               (keymap-string ,cstring-pointer)
               (keymap-size ,size_t)
               (keymap-fd ,int)
               (keymap ,(bs:pointer '*))
               (xkb-state ,(bs:pointer '*))
               (led-indexes ,(bs:vector WLR_LED_COUNT xkb_led_index_t))
               (mod-indexes ,(bs:vector WLR_MODIFIER_COUNT xkb_mod_index_t))
               (keycodes ,(bs:vector WLR_KEYBOARD_KEYS_CAP uint32))
               (num-keycodes ,size_t)
               (modifiers ,%wlr-keyboard-modifiers-struct)
               (repeat-info ,(bs:struct `((rate ,int32)
                                          (delay ,int32))))
               (events ,(bs:struct `((key ,%wl-signal-struct)
                                     (modifiers ,%wl-signal-struct)
                                     (keymap ,%wl-signal-struct)
                                     (repeat-info ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-pointer-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (events
                ,(bs:struct
                  `((motion ,%wl-signal-struct)
                    (motion-absolute ,%wl-signal-struct)
                    (button ,%wl-signal-struct)
                    (axis ,%wl-signal-struct)
                    (frame ,%wl-signal-struct)

                    (swipe-begin ,%wl-signal-struct)
                    (swipe-update ,%wl-signal-struct)
                    (swite-end ,%wl-signal-struct)

                    (pinch-begin ,%wl-signal-struct)
                    (pinch-update ,%wl-signal-struct)
                    (pinch-end ,%wl-signal-struct)

                    (hold-begin ,%wl-signal-struct)
                    (hold-end ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-switch-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (events
                ,(bs:struct
                  `((toggle ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-touch-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (events
                ,(bs:struct
                  `((down ,%wl-signal-struct)
                    (up ,%wl-signal-struct)
                    (motion ,%wl-signal-struct)
                    (cancel ,%wl-signal-struct)
                    (frame ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-tablet-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (events
                ,(bs:struct
                  `((axis ,%wl-signal-struct)
                    (proximity ,%wl-signal-struct)
                    (tip ,%wl-signal-struct)
                    (button ,%wl-signal-struct))))
               (name ,cstring-pointer)
               (paths ,%wl-array-struct)
               (data ,(bs:pointer 'void)))))

(define-public %wlr-tablet-pad-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (events
                ,(bs:struct
                  `((button ,%wl-signal-struct)
                    (ring ,%wl-signal-struct)
                    (strip ,%wl-signal-struct)
                    (attach-tablet ,%wl-signal-struct))))
               (button-count ,size_t)
               (ring-count ,size_t)
               (strip-count ,size_t)
               (groups ,%wl-list-struct)
               (paths ,%wl-array-struct)
               (data ,(bs:pointer 'void)))))

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
                                   (keyboard ,(bs:pointer %wlr-keyboard-struct))
                                   (pointer ,(bs:pointer %wlr-pointer-struct))
                                   (switch-device ,(bs:pointer %wlr-switch-struct))
                                   (touch ,(bs:pointer %wlr-touch-struct))
                                   (tablet ,(bs:pointer %wlr-tablet-struct))
                                   (tablet-pad ,(bs:pointer %wlr-tablet-pad-struct)))))
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
               (rendering ,stdbool)
               (rendering-with-buffer ,stdbool)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))

(define-public %wlr-subcompositor-struct
  (bs:struct `((global ,(bs:pointer %wl-global-struct)))))
(define-public %wlr-compositor-struct
  (bs:struct `((global ,(bs:pointer %wl-global-struct))
               (renderer ,(bs:pointer %wlr-renderer-struct))
               (subcompositor ,%wlr-subcompositor-struct)
               (display-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct)))))))

(define-public %wlr-event-keyboard-key-struct
  (bs:struct `((time-msec ,uint32)
               (keycode ,uint32)
               (update-state ,stdbool)
               (state ,int32) ;; enum wl_keyboard_key_state
               )))

(define-public %wlr-scene-node-state-struct
  (bs:struct `((link ,%wl-list-struct)
               (children ,%wl-list-struct)
               (enabled ,stdbool)
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
               (presentation ,(bs:pointer (delay %wlr-presentation-struct)))
               (presentation-destroy ,%wl-listener-struct)
               (peeding-buffers ,%wl-list-struct))))
(define-public %wlr-scene-rect-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (width ,int)
               (height ,int)
               (color ,%color-struct))))

(define-public %wlr-scene-buffer-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (buffer ,(bs:pointer %wlr-buffer-struct))
               (texture  ,(bs:pointer %wlr-texture-struct))
               (src-box ,%wlr-fbox-struct)
               (dst-width ,int)
               (dst-height ,int)
               (transform ,int) ;; enum wl_output_transform
               (pending-link ,%wl-list-struct))))

(define-public %wlr-scene-output-struct
  (bs:struct `((output ,(bs:pointer %wlr-output-struct))
               (link ,%wl-list-struct)
               (scene ,(bs:pointer %wlr-scene-struct))
               (addon ,%wlr-addon-struct)
               (damage ,(bs:pointer '*))
               (x ,int)
               (y ,int)
               (prev-scanout ,stdbool))))

(define-public %wlr-presentation-struct
  (bs:struct `((global ,(bs:pointer %wl-global-struct))
               (clock ,int32) ;; clockid_t
               (events ,(bs:struct `((destroy ,%wl-listener-struct))))
               (display-destroy ,%wl-listener-struct))))
(define-public %wlr-presentation-feedback-struct
  (bs:struct `((resources ,%wl-list-struct)
               (output ,%wlr-output-struct)
               (output-committed ,stdbool)
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
  (bs:struct `((global ,(bs:pointer %wl-global-struct))
               (display-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))
(define-public %wlr-layer-surface-v1-state-struct
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
               (resource ,(bs:pointer %wl-resource-struct))
               (shell ,(bs:pointer %wlr-layer-shell-v1-struct))
               (popups ,%wl-list-struct)
               (namespace ,cstring-pointer)
               (added ,stdbool)
               (configured ,stdbool)
               (mapped ,stdbool)
               (configure-list ,%wl-list-struct)
               (current ,%wlr-layer-surface-v1-state-struct)
               (pending ,%wlr-layer-surface-v1-state-struct)
               (surface-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (map ,%wl-signal-struct)
                                     (unmap ,%wl-signal-struct)
                                     (new-popup ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-xdg-shell-struct
  (bs:struct `((global ,(bs:pointer %wl-global-struct))
               (clients ,%wl-list-struct)
               (popup-grabs ,%wl-list-struct)
               (ping-timeout ,uint32)
               (display-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))
(define-public %wlr-xdg-client-struct
  (bs:struct `((shell ,(bs:pointer %wlr-xdg-shell-struct))
               (resource ,(bs:pointer %wl-resource-struct))
               (client ,(bs:pointer %wl-client-struct))
               (surfaces ,%wl-list-struct)
               (link ,%wl-list-struct)
               (ping-serial ,uint32)
               (ping-timer ,(bs:pointer %wl-event-source-struct)))))
(define-public %wlr-xdg-surface-struct
  (bs:struct `((client ,(bs:pointer %wlr-xdg-client-struct))
               (resource ,(bs:pointer %wl-resource-struct))
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
               (added ,stdbool)
               (configured ,stdbool)
               (mapped ,stdbool)
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
               (fullscreen-output ,(bs:pointer %wlr-output-struct))
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
               (resource ,(bs:pointer %wl-resource-struct))
               (committed ,stdbool)
               (parent ,(bs:pointer %wlr-surface-struct))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (geometry ,%wlr-box-struct)
               (positioner ,%wlr-xdg-positioner-struct)
               (grab-link ,%wl-list-struct))))

(define-public %wlr-xdg-toplevel-state-struct
  (bs:struct `(,@(map (lambda (o) `(,o ,stdbool))
                      '(maximized fullscreen resizing activated))
               ,@(map (lambda (o) `(,o ,uint32))
                      '(tiled width height max-width max-height min-width min-height)))))
(define-public %wlr-xdg-toplevel-struct
  (bs:struct `((resource ,(bs:pointer %wl-resource-struct))
               (base ,(bs:pointer %wlr-xdg-surface-struct))
               (added ,stdbool)
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
               (fullscreen ,stdbool)
               (output ,(bs:pointer %wlr-output-struct)))))

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
  (bs:struct `((client ,(bs:pointer %wl-client-struct))
               (seat ,(bs:pointer (delay %wlr-seat-struct)))
               (link ,%wl-list-struct)

               (resources ,%wl-list-struct)
               (pointers ,%wl-list-struct)
               (keyboards ,%wl-list-struct)
               (touches ,%wl-list-struct)
               (data-devices ,%wl-list-struct)

               (events ,(bs:struct `((destroy ,%wl-signal-struct))))
               (serials ,%wlr-serial-ringset-struct)
               (needs-touch-frame ,stdbool))))

(define-public WLR_POINTER_BUTTONS_CAP 16)

(define-public %wlr-seat-pointer-state-struct
  (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
               (focused-client ,(bs:pointer %wlr-seat-client-struct))
               (focused-surface ,(bs:pointer %wlr-surface-struct))
               (sx ,double)
               (sy ,double)
               (grab ,(bs:pointer (delay %wlr-seat-pointer-grab-struct)))
               (default-grab ,(bs:pointer (delay %wlr-seat-pointer-grab-struct)))
               (sent-axis-source ,stdbool)
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
               (grab ,(bs:pointer (delay %wlr-seat-touch-grab-struct)))
               (default-grab ,(bs:pointer (delay %wlr-seat-touch-grab-struct))))))
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
(define-public %wlr-seat-pointer-request-set-cursor-event-struct
  (bs:struct `((seat-client ,(bs:pointer %wlr-seat-client-struct))
               (surface ,(bs:pointer %wlr-surface-struct))
               (serial ,uint32)
               (hostpot-x ,int32)
               (hostpot-y ,int32))))
(define-public %wlr-seat-struct
  (bs:struct `((global ,(bs:pointer %wl-global-struct))
               (display ,(bs:pointer (delay %wl-display-struct)))
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

(define-public %wlr-idle-struct
  (bs:struct `((global ,(bs:pointer %wl-global-struct))
               (idle-timers ,%wl-list-struct)
               (event-loop ,(bs:pointer %wl-event-loop-struct))
               (enabled ,stdbool)
               (display-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((activity-notify ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-data-device-manager-struct
  (bs:struct `((global ,(bs:pointer %wl-global-struct))
               (data-sources ,%wl-list-struct)
               (display-destroy ,%wl-listener-struct)
               (events ,(bs:struct `((destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-public %wlr-data-source-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (mime-types ,%wl-array-struct)
               (actions ,int32)
               (accepted ,stdbool)
               (current-dnd-action ,int32)
               (compositor-action ,uint32)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))
(define-public %wlr-drap-icon-struct
  (bs:struct `((drag ,(bs:pointer (delay %wlr-drag-struct)))
               (surface ,(bs:pointer %wlr-surface-struct))
               (mapped ,stdbool)
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
               (started ,stdbool)
               (dropped ,stdbool)
               (cancelling ,stdbool)
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
