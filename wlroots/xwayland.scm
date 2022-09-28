(define-module (wlroots xwayland)
  #:use-module (wlroots types)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module ((system foreign ) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:export (%wlr-xwayland-surface-struct
            wrap-wlr-xwayland-surface
            unwrap-wlr-xwayland-surface
            wlr-xwayland-surface-close
            wlr-xwayland-surface-class
            wlr-xwayland-surface-title
            wlr-xwayland-surface-set-fullscreen))

(define-wlr-types-class wlr-xwayland-surface)

(define %wlr-xwayland-surface-struct
  (bs:struct `((window-id ,uint32)
               (xwm ,(bs:pointer '*))
               (surface-id ,uint32)
               (link ,%wl-list)
               (stack-link ,%wl-list)
               (unpaired-link ,%wl-list)
               (surface ,(bs:pointer '*))
               (x ,int16)
               (y ,int16)
               (width ,uint16)
               (height ,uint16)
               (override-redirect ,int8) ;; bool
               (mapped ,int8)            ;; bool
               (title ,cstring-pointer)
               (class ,cstring-pointer)
               (instance ,cstring-pointer)
               (role ,cstring-pointer)
               (startup-id ,cstring-pointer)
               (pid ,int)                   ;; pid_t
               (has-utf8-title ,int8)       ;; bool
               (children ,%wl-list)
               (parent ,(bs:pointer (delay %wlr-xwayland-surface-struct)))
               (parent-link ,%wl-list)
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
                              '(destroy;
                                request-configure;
                                request-move;
                                request-resize;
                                request-minimize;
                                request-maximize;
                                request-fullscreen;
                                request-activate;

                                map;
                                unmap;
                                set-title;
                                set-class;
                                set-role;
                                set-parent;
                                set-pid;
                                set-startup-id;
                                set-window-type;
                                set-hints;
                                set-decorations;
                                set-override-redirect;
                                set-geometry;
                                ping-timeout;


                                ))))
               (surface-destroy ,%wl-listener)
               (data ,(bs:pointer 'void)))))

(define (wlr-xwayland-surface-class x)
  (bytestructure-ref
   (pointer->bytestructure
    (unwrap-wlr-xwayland-surface x)
    %wlr-xwayland-surface-struct) 'class))

(define (wlr-xwayland-surface-title x)
  (bytestructure-ref
   (pointer->bytestructure
    (unwrap-wlr-xwayland-surface x)
    %wlr-xwayland-surface-struct) 'title))
;; wlr_xwayland_surface_set_fullscreen
(define-wlr-procedure (wlr-xwayland-surface-close surface)
  (ffi:void "wlr_xwayland_surface_close" (list '*))
  "Request that this xdg toplevel closes."
  (% (unwrap-wlr-xwayland-surface surface)))
(define-wlr-procedure (wlr-xwayland-surface-set-fullscreen surface fullscreen)
  (ffi:void "wlr_xwayland_surface_set_fullscreen" (list '* ffi:int))
  (% (unwrap-wlr-xwayland-surface surface) surface (if fullscreen 1 0)))
