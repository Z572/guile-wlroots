(define-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:re-export (%wlr-box-struct %wlr-fbox-struct)
  #:export (<wlr-box> make-wlr-box list->wlr-box))


(define-bytestructure-class <wlr-box> (<box>)
  %wlr-box-struct wrap-wlr-box unwrap-wlr-box wlr-box?
  (x #:init-keyword #:x #:accessor box-x)
  (y #:init-keyword #:y #:accessor box-y)
  (width #:init-keyword #:width #:accessor box-width)
  (height #:init-keyword #:height #:accessor box-height))

(define (make-wlr-box x y width height)
  (make <wlr-box>
    #:x x
    #:y y
    #:width width
    #:height height))

(define-method (shallow-clone (box <wlr-box>))
  (make-wlr-box (box-x box) (box-y box) (box-width box) (box-height box)))

(define (list->wlr-box l)
  (apply make-wlr-box l))
