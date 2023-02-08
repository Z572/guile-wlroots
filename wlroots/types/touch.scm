(define-module (wlroots types touch)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module (wlroots types)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots utils)
  #:export (.data
            .time-msec
            .touch-id
            .x
            .y
            .touch))

(define-wlr-types-class wlr-touch ()
  (data #:accessor .data)
  #:descriptor %wlr-touch-struct)

(define-wlr-types-class wlr-touch-down-event ()
  (touch #:accessor .touch)
  (time-msec #:accessor .time-msec)
  (touch-id #:accessor .touch-id)
  (x #:accessor .x)
  (y #:accessor .y)
  #:descriptor %wlr-touch-down-event-struct)

(define-wlr-types-class wlr-touch-up-event ()
  (touch #:accessor .touch)
  (time-msec #:accessor .time-msec)
  (touch-id #:accessor .touch-id)
  #:descriptor %wlr-touch-up-event-struct)

(define-wlr-types-class wlr-touch-motion-event ()
  (touch #:accessor .touch)
  (time-msec #:accessor .time-msec)
  (touch-id #:accessor .touch-id)
  (x #:accessor .x)
  (y #:accessor .y)
  #:descriptor %wlr-touch-motion-event-struct)

(define-wlr-types-class wlr-touch-cancel-event ()
  (touch #:accessor .touch)
  (time-msec #:accessor .time-msec)
  (touch-id #:accessor .touch-id)
  #:descriptor %wlr-touch-cancel-event-struct)

(define-wlr-procedure (wlr-touch-from-input-device input-device)
  ('* "wlr_touch_from_input_device" '(*))
  (wrap-wlr-touch (% (unwrap-wlr-input-device input-device))))
