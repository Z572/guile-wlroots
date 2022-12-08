(define-module (wlroots types output-management)
  #:use-module (wlroots types)
  #:export (<wlr-output-manager-v1>
            wrap-wlr-output-manager-v1
            unwrap-wlr-output-manager-v1

            <wlr-output-configuration-v1>
            wrap-wlr-output-configuration-v1
            unwrap-wlr-output-configuration-v1))

(define-wlr-types-class wlr-output-manager-v1)
(define-wlr-types-class wlr-output-configuration-v1)
