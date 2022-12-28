(define-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:re-export (%wlr-box-struct %wlr-fbox-struct)
  #:export (<wlr-box> make-wlr-box list->wlr-box))


(define-wlr-types-class-public wlr-box (<box>)
  (x #:allocation #:bytestructure #:accessor box-x)
  (y #:allocation #:bytestructure #:accessor box-y)
  (height #:allocation #:bytestructure #:accessor box-height)
  (width #:allocation #:bytestructure #:accessor box-width)
  #:descriptor %wlr-box-struct)

(define (make-wlr-box x y width height)
  (wrap-wlr-box (bytestructure->pointer
                 (bytestructure
                  %wlr-box-struct
                  `((x ,x)
                    (y ,y)
                    (width ,width)
                    (height ,height))))))

(define-method (shallow-clone (box <wlr-box>))
  (make-wlr-box (box-x box) (box-y box) (box-width box) (box-height box)))
(define (list->wlr-box l)
  (apply make-wlr-box l))

(define-method (->bytestructure (box <wlr-box>))
  (pointer->bytestructure (get-pointer box) %wlr-box-struct))
