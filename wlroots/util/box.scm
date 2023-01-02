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
  (x #:accessor box-x)
  (y #:accessor box-y)
  (height #:accessor box-height)
  (width #:accessor box-width))

(define (make-wlr-box x y width height)
  (wrap-wlr-box (bytestructure
                 %wlr-box-struct
                 `((x ,x)
                   (y ,y)
                   (width ,width)
                   (height ,height)))))

(define-method (shallow-clone (box <wlr-box>))
  (make-wlr-box (box-x box) (box-y box) (box-width box) (box-height box)))
(define (list->wlr-box l)
  (apply make-wlr-box l))

(define-method (->bytestructure (box <wlr-box>))
  (pointer->bytestructure (get-pointer box) %wlr-box-struct))
