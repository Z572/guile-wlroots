(define-module (wlroots types idle)
  #:use-module (wlroots utils)
  #:use-module (wayland server display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types seat)
  #:use-module (wlroots types)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (<wlr-idle>
            wlr-idle-create
            wlr-idle-notify-activity
            wlr-idle-set-enabled
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
(define-wlr-procedure (wlr-idle-set-enabled idle seat enabled)
  (ffi:void "wlr_idle_set_enabled" `(* * ,ffi:int8))
  (% (unwrap-wlr-idle idle)
     (unwrap-wlr-seat seat)
     (if enabled 1 0)))
