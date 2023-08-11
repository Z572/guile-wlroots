(define-module (wlroots types pointer-gestures)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module (wayland server display)
  #:use-module (wlroots types)
  #:use-module (wlroots types seat)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:export ())

(define-wlr-types-class wlr-pointer-gestures-v1 ()
  (global #:accessor .global)
  (swipes #:accessor .swipes)
  (pinches #:accessor .pinches)
  (holds #:accessor .holds)
  (display-destroy #:accessor .display-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-pointer-gestures-v1-struct)

(define-wlr-procedure (wlr-pointer-gestures-v1-create display)
  ('* "wlr_pointer_gestures_v1_create" (list '*))
  (wrap-wlr-pointer-gestures-v1 (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-swipe-begin
                       gestures seat
                       time-msec fingers)
  (ffi:void "wlr_pointer_gestures_v1_send_swipe_begin" (list '* '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     fingers))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-swipe-update
                       gestures seat
                       time-msec dx dy)
  (ffi:void "wlr_pointer_gestures_v1_send_swipe_update" (list '* '* ffi:uint32 ffi:double ffi:double))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     dx
     dy))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-swipe-end
                       gestures seat
                       time-msec cancelled)
  (ffi:void "wlr_pointer_gestures_v1_send_swipe_end" (list '* '* ffi:uint32 ffi:uint8))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     (if cancelled 1 0)))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-pinch-begin
                       gestures seat
                       time-msec fingers)
  (ffi:void "wlr_pointer_gestures_v1_send_pinch_begin" (list '* '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     fingers))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-pinch-update
                       gestures seat
                       time-msec dx dy
                       scale
                       rotation)
  (ffi:void "wlr_pointer_gestures_v1_send_pinch_update"
            (list '* '* ffi:uint32 ffi:double ffi:double ffi:double ffi:double))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     dx
     dy
     scale
     rotation))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-pinch-end
                       gestures seat
                       time-msec cancelled)
  (ffi:void "wlr_pointer_gestures_v1_send_pinch_end" (list '* '* ffi:uint32 ffi:uint8))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     (if cancelled 1 0)))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-hold-begin
                       gestures seat
                       time-msec fingers)
  (ffi:void "wlr_pointer_gestures_v1_send_hold_begin" (list '* '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     fingers))

(define-wlr-procedure (wlr-pointer-gestures-v1-send-hold-end
                       gestures seat
                       time-msec cancelled)
  (ffi:void "wlr_pointer_gestures_v1_send_hold_end" (list '* '* ffi:uint32 ffi:uint8))
  (% (unwrap-wlr-pointer-gestures-v1 gestures)
     (unwrap-wlr-seat seat)
     time-msec
     (if cancelled 1 0)))
