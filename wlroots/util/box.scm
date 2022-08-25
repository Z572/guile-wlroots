(define-module (wlroots util box)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:export (%wlr-box-struct %wlr-fbox-struct <wlr-box> make-wlr-box list->wlr-box))
(define %wlr-box-struct
  (bs:struct `((x ,int) (y ,int) (width ,int) (height ,int))))

(define %wlr-fbox-struct
  (bs:struct `((x ,double)
               (y ,double)
               (width ,double)
               (height ,double))))

(define-wlr-types-class-public
  wlr-box)
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
