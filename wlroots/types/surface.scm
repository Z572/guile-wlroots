(define-module (wlroots types surface)
  #:use-module (wlroots util box)
  #:use-module (wlroots util addon)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wlroots types output)
  #:use-module (wayland util)
  #:use-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:re-export (%wlr-surface-state-struct
               %wlr-surface-struct)
  #:export (wrap-wlr-surface
            unwrap-wlr-surface
            wlr-surface-has-buffer
            wlr-surface-send-enter
            wlr-surface-get-root-surface
            .resource
            .renderer
            .buffer
            .sx
            .sy))


(define-wlr-types-class-public wlr-surface ()
  (resource #:accessor .resource)
  (renderer #:accessor .renderer)
  (buffer #:accessor .buffer)
  (sx #:accessor .sx)
  (sy #:accessor .sy)
  #:descriptor %wlr-surface-struct)

(define-wlr-procedure (wlr-surface-has-buffer surface)
  (ffi:int "wlr_surface_has_buffer" '(*))
  (= (% (unwrap-wlr-surface surface)) 1))

(define-wlr-procedure (wlr-surface-get-root-surface surface)
  ('* "wlr_surface_get_root_surface" '(*))
  (let ((o (% (unwrap-wlr-surface surface))))
    (and (not (ffi:null-pointer? o))
         (wrap-wlr-surface o))))

(define-wlr-procedure (wlr-surface-send-enter surface output)
  (ffi:void "wlr_surface_send_enter" '(* *))
  (% (unwrap-wlr-surface surface) (unwrap-wlr-output output)))
