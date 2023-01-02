(define-module (wlroots types idle)
  #:use-module (wlroots utils)
  #:use-module (wayland display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types seat)
  #:use-module (wlroots types)
  #:export (<wlr-idle>
            wlr-idle-create
            wlr-idle-notify-activity
            wrap-wlr-idle
            unwrap-wlr-idle
            .global
            .idle-timers
            .event-loop
            .enabled
            .display-destroy
            .data))

(define-wlr-types-class wlr-idle ()
  (global #:accessor .global)
  (idle-timers #:accessor .idle-timers)
  (event-loop #:accessor .event-loop)
  (enabled #:accessor .enabled)
  (display-destroy #:accessor .display-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-idle-struct)

(define-wlr-procedure (wlr-idle-create display)
  ('* "wlr_idle_create" '(*))
  (wrap-wlr-idle (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-idle-notify-activity idle seat)
  (ffi:void "wlr_idle_notify_activity" '(* *))
  (% (unwrap-wlr-idle idle)
     (unwrap-wlr-seat  seat)))
