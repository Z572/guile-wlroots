(define-module (wlroots xwayland)
  #:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:use-module (wlroots types)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types compositor)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (wayland display)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module ((system foreign ) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:export (%wlr-xwayland-surface-struct
            wrap-wlr-xwayland-surface
            unwrap-wlr-xwayland-surface
            wlr-xwayland-display-name
            wlr-xwayland-surface-y
            wlr-xwayland-surface-x
            wlr-xwayland-surface-width
            wlr-xwayland-surface-height
            wlr-xwayland-surface-close
            wlr-xwayland-surface-class
            wlr-xwayland-surface-override-redirect
            wlr-xwayland-surface-title
            wlr-xwayland-surface-role
            wlr-xwayland-surface-instance
            wlr-xwayland-surface-surface
            wlr-xwayland-surface-set-fullscreen
            wlr-xwayland-surface-mapped?
            wlr-xwayland-surface-from-wlr-surface
            wlr-xwayland-surface-configure
            wlr-xwayland-create))

(eval-when (expand load eval)
  (define %wlr-xwayland-struct
    (bs:struct `((server ,(bs:pointer '*))
                 (xwm ,(bs:pointer '*))
                 (cursor ,(bs:pointer '*))
                 (display-name ,cstring-pointer)
                 (wl-display ,(bs:pointer '*))
                 (compositor ,(bs:pointer '*))
                 (seat ,(bs:pointer '*))
                 (events ,(bs:struct
                           (map (lambda (a) (list a %wl-signal-struct))
                                '(ready
                                  new-surface
                                  remove-startup-info))))
                 (user-event-handler ,(bs:pointer '*))
                 (server-ready ,%wl-listener-struct)
                 (server-destroy ,%wl-listener-struct)
                 (seat-destroy ,%wl-listener-struct)
                 (data ,(bs:pointer 'void)))))
  (define %wlr-xwayland-surface-struct
    (bs:struct `((window-id ,uint32)
                 (xwm ,(bs:pointer '*))
                 (surface-id ,uint32)
                 (link ,%wl-list-struct)
                 (stack-link ,%wl-list-struct)
                 (unpaired-link ,%wl-list-struct)
                 (surface ,(bs:pointer '*))
                 (x ,int16)
                 (y ,int16)
                 (width ,uint16)
                 (height ,uint16)
                 (override-redirect ,int8) ;; bool
                 (mapped ,int8)            ;; bool
                 (title ,(bs:pointer int8))
                 (class ,(bs:pointer int8))
                 (instance ,(bs:pointer int8))
                 (role ,(bs:pointer int8))
                 (startup-id ,(bs:pointer int8))
                 (pid ,int)                   ;; pid_t
                 (has-utf8-title ,int8)       ;; bool
                 (children ,%wl-list-struct)
                 (parent ,(bs:pointer (delay %wlr-xwayland-surface-struct)))
                 (parent-link ,%wl-list-struct)
                 (window-type ,(bs:pointer '*))
                 (window-type-len ,size_t)
                 (protocols ,(bs:pointer '*))
                 (protocols-len ,size_t)
                 (decorations ,uint32)
                 (hints ,(bs:pointer '*))
                 (hints-urgency ,uint32)
                 (size-hints ,(bs:pointer '*))
                 (pinging ,int8)          ;; boo;
                 (ping-timer ,(bs:pointer '*))
                 ,@(map (lambda (a) (list a int8))
                        '(modal fullscreen maximized-vert maximized-horz minimized has-alpha))
                 (events ,(bs:struct
                           (map (lambda (a) (list a %wl-signal-struct))
                                '(destroy
                                  request-configure
                                  request-move
                                  request-resize
                                  request-minimize
                                  request-maximize
                                  request-fullscreen
                                  request-activate

                                  map
                                  unmap
                                  set-title
                                  set-class
                                  set-role
                                  set-parent
                                  set-pid
                                  set-startup-id
                                  set-window-type
                                  set-hints
                                  set-decorations
                                  set-override-redirect
                                  set-geometry
                                  ping-timeout


                                  ))))
                 (surface-destroy ,%wl-listener-struct)
                 (data ,(bs:pointer 'void)))))
  (define-bytestructure-accessors %wlr-xwayland-surface-struct
    xwayland-surface-unwrap xwayland-surface-ref xwayland-surface-set!))

(define-wlr-types-class-public wlr-xwayland ()
  #:descriptor %wlr-xwayland-struct)

(define-wlr-types-class wlr-xwayland-surface ()
  #:descriptor %wlr-xwayland-surface-struct)

(define-bytestructure-accessors %wlr-xwayland-struct
  xwayland-unwrap xwayland-ref xwayland-set!)

(define dsize (bytestructure-descriptor-size %wlr-xwayland-surface-struct))
(define-syntax-rule (ref surface o)
  (xwayland-surface-ref
   (ffi:pointer->bytevector
    (unwrap-wlr-xwayland-surface surface)
    dsize) o))
(define xwayland-size (bytestructure-descriptor-size %wlr-xwayland-struct))
(define-syntax-rule (x-ref x o)
  (xwayland-ref
   (ffi:pointer->bytevector
    (unwrap-wlr-xwayland x)
    xwayland-size) o))

(define (wlr-xwayland-display-name x)
  (x-ref x display-name))
(define (wlr-xwayland-surface-surface x)
  (let ((s (ffi:make-pointer
            (ref x surface))))
    (if (ffi:null-pointer? s)
        #f
        (wrap-wlr-surface s))))

(define (wlr-xwayland-surface-class x)
  (let ((s (ffi:make-pointer
            (ref x class))))
    (if (ffi:null-pointer? s)
        #f
        (ffi:pointer->string s))))

(define (wlr-xwayland-surface-override-redirect x)
  (not (zero? (ref x override-redirect))))

(define (wlr-xwayland-surface-x s)
  (ref s x))
(define (wlr-xwayland-surface-y s)
  (ref s y))
(define (wlr-xwayland-surface-width s)
  (ref s width))
(define (wlr-xwayland-surface-height s)
  (ref s height))

(define (wlr-xwayland-surface-mapped? x)
  (let ((s (ffi:make-pointer
            (ref x mapped))))
    (not (zero? s))))

(define (wlr-xwayland-surface-title x)
  (let ((s (ffi:make-pointer
            (ref x title))))
    (if (ffi:null-pointer? s)
        #f
        (ffi:pointer->string s))))


(define (wlr-xwayland-surface-instance x)
  (let ((s (ffi:make-pointer
            (ref x instance))))
    (if (ffi:null-pointer? s)
        #f
        (ffi:pointer->string s))))

(define (wlr-xwayland-surface-role x)
  (let ((s (ffi:make-pointer
            (ref x role))))
    (if (ffi:null-pointer? s)
        #f
        (ffi:pointer->string s))))
;; wlr_xwayland_surface_set_fullscreen
(define-wlr-procedure (wlr-xwayland-create display compositor lazy?)
  ('* "wlr_xwayland_create" (list '* '* ffi:int))
  (wrap-wlr-xwayland (% (unwrap-wl-display display)
                        (unwrap-wlr-compositor compositor)
                        (if lazy? 1 0))))

(define-wlr-procedure (wlr-xwayland-surface-close surface)
  (ffi:void "wlr_xwayland_surface_close" (list '*))
  "Request that this xdg toplevel closes."
  (% (unwrap-wlr-xwayland-surface surface)))
(define-wlr-procedure (wlr-xwayland-surface-set-fullscreen surface fullscreen)
  (ffi:void "wlr_xwayland_surface_set_fullscreen" (list '* ffi:int))
  (% (unwrap-wlr-xwayland-surface surface) (if fullscreen 1 0)))

(define-wlr-procedure (wlr-xwayland-surface-from-wlr-surface surface)
  ('* "wlr_xwayland_surface_from_wlr_surface" '(*))
  (wrap-wlr-xwayland-surface (% (unwrap-wlr-surface surface))))

(define-wlr-procedure (wlr-xwayland-surface-configure surface x y width height)
  (ffi:void "wlr_xwayland_surface_configure" (list '*
                                                   ffi:int16
                                                   ffi:int16
                                                   ffi:uint16
                                                   ffi:uint16))
  (% (unwrap-wlr-xwayland-surface surface) x y width height))
