(define-module (wlroots types transform)
  #:use-module (wayland protocol)
  #:use-module (wlroots types)
  #:use-module (wlroots utils))

(define-wlr-procedure (wlr-output-transform-invert tr)
  ('* "wlr_output_transform_invert" '(*))
  (% (bs:enum->integer %wl-output-transform-enum tr)))

(define-wlr-procedure (wlr-output-transform-compose tr_a tr_b)
  ('* "wlr_output_transform_compose" '(* *))
  (% (bs:enum->integer %wl-output-transform-enum tr_a)
     (bs:enum->integer %wl-output-transform-enum tr_b)))
