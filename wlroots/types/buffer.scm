(define-module (wlroots types buffer)
  #:use-module (wlroots types)
  #:use-module (oop goops)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (wrap-wlr-buffer
            unwrap-wlr-buffer))

(define-wlr-types-class wlr-buffer)
