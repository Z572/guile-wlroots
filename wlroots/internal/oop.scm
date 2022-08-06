(define-module (wlroots internal oop)
  #:use-module (system foreign)
  #:use-module (wlroots config)
  #:use-module (wayland util)
  #:use-module ((bytestructures guile)
                #:select
                (bytestructure
                 bs:pointer
                 bytestructure-offset))
  #:use-module (oop goops)
  #:export (wlr->pointer wlr->procedure bytestructure+offset->pointer)
  #:export-syntax (define-wlr-procedure define-enumeration))
