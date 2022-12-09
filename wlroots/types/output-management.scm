(define-module (wlroots types output-management)
  #:use-module (wayland display)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:export (<wlr-output-manager-v1>
            wrap-wlr-output-manager-v1
            unwrap-wlr-output-manager-v1

            <wlr-output-configuration-v1>
            wrap-wlr-output-configuration-v1
            unwrap-wlr-output-configuration-v1

            wlr-output-manager-v1-create))

(define-wlr-types-class wlr-output-manager-v1)
(define-wlr-types-class wlr-output-configuration-v1)

(define-wlr-procedure (wlr-output-manager-v1-create display)
  ('* "wlr_output_manager_v1_create" (list '*))
  (wrap-wlr-output-manager-v1 (% (unwrap-wl-display display))))
