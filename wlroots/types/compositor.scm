(define-module (wlroots types compositor)
  #:use-module (wayland display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types)
  #:use-module (wlroots types surface)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:export (wrap-wlr-compositor
            unwrap-wlr-compositor
            wlr-compositor-create
            wlr-surface-is-subsurface))

(define-wlr-types-class wlr-compositor ()
  #:descriptor %wlr-compositor-struct)

(define-wlr-procedure (wlr-compositor-create display renderer)
  ('* "wlr_compositor_create" '(* *))
  (wrap-wlr-compositor
   (% (unwrap-wl-display display)
      (unwrap-wlr-renderer renderer))))

(define-wlr-procedure (wlr-surface-is-subsurface surface)
  (ffi:int8 "wlr_surface_is_subsurface" '(*))
  (not (zero? (% (unwrap-wlr-surface surface)))))
