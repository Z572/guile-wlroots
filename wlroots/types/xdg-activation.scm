(define-module (wlroots types xdg-activation)
  #:use-module (wayland display)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:export (wlr-xdg-activation-v1-create
            <wlr-xdg-activation-v1>
            wrap-wlr-xdg-activation-v1
            unwrap-wlr-xdg-activation-v1))

(define-wlr-types-class-public wlr-xdg-activation-v1)

(define-wlr-procedure (wlr-xdg-activation-v1-create display)
  ('* "wlr_xdg_activation_v1_create" '(*))
  (wrap-wlr-xdg-activation-v1 (% (unwrap-wl-display display)) ))
