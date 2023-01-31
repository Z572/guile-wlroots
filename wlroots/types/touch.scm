(define-module (wlroots types touch)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:use-module (wlroots types))

(define-wlr-types-class wlr-touch ()
  #:descriptor %wlr-touch-struct)
