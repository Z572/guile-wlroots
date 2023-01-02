(define-module (wlroots render texture)
  #:use-module (wlroots types))

(define-bytestructure-class <wlr-texture> ()
  %wlr-texture-struct
  wrap-wlr-texture unwrap-wlr-texture wlr-texture?)
