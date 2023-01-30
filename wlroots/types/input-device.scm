(define-module (wlroots types input-device)
  #:use-module (wayland signal)
  #:use-module (wlroots types)
  #:use-module (wlroots types keyboard)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:re-export (%wlr-input-device-struct)
  #:export (wrap-wlr-input-device
            unwrap-wlr-input-device
            .type
            .vendor
            .product
            .name
            .data))


(define-wlr-types-class wlr-input-device ()
  (type #:accessor .type)
  (vendor #:accessor .vendor)
  (product #:accessor .product)
  (name #:accessor .name)
  (data #:accessor .data)
  #:descriptor %wlr-input-device-struct)
