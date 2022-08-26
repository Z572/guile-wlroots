(define-module (wlroots types idle)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types seat)
  #:use-module (wlroots types)
  #:export (<wlr-idle>
            wlr-idle-notify-activity
            wrap-wlr-idle
            unwrap-wlr-idle))

(define-wlr-types-class wlr-idle)

(define-wlr-procedure (wlr-idle-notify-activity idle seat)
  (ffi:void "wlr_idle_notify_activity" '(* *))
  (% (unwrap-wlr-idle idle)
     (unwrap-wlr-seat  seat)))
