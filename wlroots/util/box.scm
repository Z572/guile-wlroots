(define-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module (wlroots types)
  #:use-module (wayland protocol)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:re-export (%wlr-box-struct %wlr-fbox-struct)
  #:export (<wlr-box> <wlr-fbox> make-wlr-box list->wlr-box))


(define-bytestructure-class <wlr-box> (<box>)
  %wlr-box-struct wrap-wlr-box unwrap-wlr-box wlr-box?
  (x #:init-keyword #:x #:accessor box-x)
  (y #:init-keyword #:y #:accessor box-y)
  (width #:init-keyword #:width #:accessor box-width)
  (height #:init-keyword #:height #:accessor box-height))

(define-wlr-types-class wlr-fbox (<wlr-box>)
  #:descriptor %wlr-fbox-struct)

(define (make-wlr-box x y width height)
  (make <wlr-box>
    #:x x
    #:y y
    #:width width
    #:height height))

(define-method (shallow-clone (box <wlr-box>))
  (make (class-of box)
    #:x (box-x box)
    #:y (box-y box)
    #:width (box-width box)
    #:height (box-height box)))

(define (list->wlr-box l)
  (apply make-wlr-box l))

(define-method (box-empty? (box <wlr-fbox>)) (wlr-box-empty box))
(define-method (box-empty? (box <wlr-fbox>)) (wlr-fbox-empty box))

(define-wlr-procedure (wlr-box-empty box)
  (ffi:int8 "wlr_box_empty" (list '*))
  (not (zero? (% (unwrap-wlr-box box)))))

(define-wlr-procedure (wlr-fbox-empty box)
  (ffi:int8 "wlr_fbox_empty" (list '*))
  (not (zero? (% (unwrap-wlr-fbox box)))))

(define-wlr-procedure (wlr-box-transform dest box transform width height)
  (ffi:void "wlr_box_transform" (list '* '* ffi:int32 ffi:int ffi:int))
  (% (unwrap-wlr-box dest)
     (unwrap-wlr-box box)
     (bs:enum->integer %wl-output-transform-enum transform)
     width
     height))

(define-wlr-procedure (wlr-fbox-transform dest box transform width height)
  (ffi:void "wlr_fbox_transform" (list '* '* ffi:int32 ffi:double ffi:double))
  (% (unwrap-wlr-fbox dest)
     (unwrap-wlr-fbox box)
     (bs:enum->integer %wl-output-transform-enum transform)
     width
     height))
