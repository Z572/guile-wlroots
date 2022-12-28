(define-module (wlroots types xdg-activation)
  #:use-module (oop goops)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland display)
  #:use-module (wayland util)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:re-export (%wlr-xdg-activation-v1-struct)
  #:export (wlr-xdg-activation-v1-create
            <wlr-xdg-activation-v1>
            wrap-wlr-xdg-activation-v1
            unwrap-wlr-xdg-activation-v1))

(define-wlr-types-class-public wlr-xdg-activation-v1 ()
  #:descriptor %wlr-xdg-activation-v1-struct)

(define-wlr-procedure (wlr-xdg-activation-v1-create display)
  ('* "wlr_xdg_activation_v1_create" '(*))
  (wrap-wlr-xdg-activation-v1 (% (unwrap-wl-display display)) ))
