(define-module (wlroots types pointer)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module (wlroots types)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:export (.time-msec
            .source
            .delta
            .delta-discrete
            .orientation
            .delta-x
            .delta-y
            .x
            .y
            .base
            .output-name
            .data
            .unaccel-dx
            .unaccel-dy
            .button
            .state
            .cancelled
            .pointer
            .fingers))

(define-wlr-types-class wlr-pointer ()
  (base #:accessor .base)
  (output-name #:accessor .output-name)
  (data #:accessor .data)
  #:descriptor %wlr-pointer-struct)

(define-wlr-types-class wlr-pointer-motion-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (delta-x #:accessor .delta-x)
  (delta-y #:accessor .delta-y)
  (unaccel-dx #:accessor .unaccel-dx)
  (unaccel-dy #:accessor .unaccel-dy)
  #:descriptor %wlr-pointer-motion-event-struct)


(define-wlr-types-class wlr-pointer-motion-absolute-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (x #:accessor .x)
  (y #:accessor .y)
  #:descriptor %wlr-pointer-motion-absolute-event-struct)


(define-wlr-types-class wlr-pointer-button-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (button #:accessor .button)
  (state #:accessor .state)
  #:descriptor %wlr-pointer-button-event-struct)

(define-wlr-types-class wlr-pointer-axis-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (source #:accessor .source)
  (orientation #:accessor .orientation)
  (delta #:accessor .delta)
  (delta-discrete #:accessor .delta-discrete)
  #:descriptor %wlr-pointer-axis-event-struct)

(define-wlr-types-class wlr-pointer-swipe-begin-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (fingers #:accessor .fingers)
  #:descriptor %wlr-pointer-swipe-begin-event-struct)

(define-wlr-types-class wlr-pointer-swipe-update-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (fingers #:accessor .fingers)
  (dx #:accessor .dx)
  (dy #:accessor .dy)
  #:descriptor %wlr-pointer-swipe-update-event-struct)

(define-wlr-types-class wlr-pointer-swipe-end-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (cancelled #:accessor .cancelled)
  #:descriptor %wlr-pointer-swipe-end-event-struct)

(define-wlr-types-class wlr-pointer-pinch-begin-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (fingers #:accessor .fingers)
  #:descriptor   %wlr-pointer-pinch-begin-event-struct)

(define-wlr-types-class wlr-pointer-pinch-update-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (fingers #:accessor .fingers)
  (dx #:accessor .dx)
  (dy #:accessor .dy)
  (scale #:accessor .scale)
  (rotation #:accessor .rotation)
  #:descriptor %wlr-pointer-pinch-update-event-struct)

(define-wlr-types-class wlr-pointer-pinch-end-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (cancelled #:accessor .cancelled)
  #:descriptor %wlr-pointer-pinch-end-event-struct)

(define-wlr-types-class wlr-pointer-hold-begin-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (fingers #:accessor .fingers)
  #:descriptor %wlr-pointer-hold-begin-event-struct)

(define-wlr-types-class wlr-pointer-hold-end-event ()
  (pointer #:accessor .pointer)
  (time-msec #:accessor .time-msec)
  (cancelled #:accessor .cancelled)
  #:descriptor %wlr-pointer-hold-end-event-struct)

(define-wlr-procedure (wlr-pointer-from-input-device input-device)
  ('* "wlr_pointer_from_input_device" (list '*))
  (wrap-wlr-pointer (% (unwrap-wlr-input-device input-device))))
