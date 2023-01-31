(define-module (wlroots types tablet-tool)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:use-module (wlroots types))

(define-wlr-types-class wlr-tablet ()
  #:descriptor %wlr-tablet-struct)
