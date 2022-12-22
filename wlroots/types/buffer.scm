(define-module (wlroots types buffer)
  #:use-module (wlroots types)
  #:use-module (oop goops)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (wrap-wlr-buffer
            unwrap-wlr-buffer
            wlr-buffer-drop
            wlr-buffer-lock
            wlr-buffer-unlock))

(define-wlr-types-class wlr-buffer ())

(define-wlr-procedure (wlr-buffer-drop buffer)
  (ffi:void "wlr_buffer_drop" '(*))
  (% (unwrap-wlr-buffer buffer)))

(define-wlr-procedure (wlr-buffer-lock buffer)
  ('* "wlr_buffer_lock" '(*))
  (wrap-wlr-buffer (% (unwrap-wlr-buffer buffer))))
(define-wlr-procedure (wlr-buffer-unlock buffer)
  (ffi:void "wlr_buffer_unlock" '(*))
  (% (unwrap-wlr-buffer buffer)))
