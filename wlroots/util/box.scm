(define-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:export (%wlr-box-struct %wlr-fbox-struct <wlr-box> make-wlr-box list->wlr-box))
(define %wlr-box-struct
  (bs:struct `((x ,int) (y ,int) (width ,int) (height ,int))))

(define %wlr-fbox-struct
  (bs:struct `((x ,double)
               (y ,double)
               (width ,double)
               (height ,double))))

(define-wlr-types-class-public wlr-box (<box>))
(define (make-wlr-box x y width height)
  (wrap-wlr-box (bytestructure->pointer
                 (bytestructure
                  %wlr-box-struct
                  `((x ,x)
                    (y ,y)
                    (width ,width)
                    (height ,height))))))
(define (list->wlr-box l)
  (apply make-wlr-box l))

(define-method (->bytestructure (box <wlr-box>))
  (pointer->bytestructure (get-pointer box) %wlr-box-struct))

(define-method (box-x (box <wlr-box>))
  (bytestructure-ref (->bytestructure box) 'x))
(define-method (box-y (box <wlr-box>))
  (bytestructure-ref (->bytestructure box) 'y))
(define-method (box-width (box <wlr-box>))
  (bytestructure-ref (->bytestructure box) 'width))
(define-method (box-height (box <wlr-box>))
  (bytestructure-ref (->bytestructure box) 'height))

(define-method ((setter box-x) (box <wlr-box>) (n <integer>))
  (bytestructure-set! (->bytestructure box) 'x n))
(define-method ((setter box-y) (box <wlr-box>) (n <integer>))
  (bytestructure-set! (->bytestructure box) 'y n))
(define-method ((setter box-width) (box <wlr-box>) (n <integer>))
  (bytestructure-set! (->bytestructure box) 'width n))
(define-method ((setter box-height) (box <wlr-box>) (n <integer>))
  (bytestructure-set! (->bytestructure box) 'height n))
