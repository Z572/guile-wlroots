(define-module (wlroots types idle-notify)
  #:use-module (wlroots utils)
  #:use-module (wayland server display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types)
  #:use-module (wlroots types seat)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wlr-idle-notifier-v1-create
            wlr-idle-notifier-v1-set-inhibited
            wlr-idle-notifier-v1-notify-activity))

(define-wlr-types-class wlr-idle-notifier-v1 ()
  #:descriptor %wlr-idle-notifier-v1-struct)

(define-wlr-procedure (wlr-idle-notifier-v1-create display)
  ('* "wlr_idle_notifier_v1_create" '(*))
  (wrap-wlr-idle-notifier-v1 (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-idle-notifier-v1-set-inhibited notifier inhibited)
  (ffi:void "wlr_idle_notifier_v1_set_inhibited" (list '* ffi:int8))
  (% (unwrap-wlr-idle-notifier-v1 notifier) (if inhibited 1 0)))

(define-wlr-procedure (wlr-idle-notifier-v1-notify-activity notifier seat)
  (ffi:void "wlr_idle_notifier_v1_notify_activity" '(* *))
  (% (unwrap-wlr-idle-notifier-v1 notifier) (unwrap-wlr-seat seat)))
