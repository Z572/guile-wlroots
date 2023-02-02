(define-module (wlroots render dmabuf)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module (wlroots types)
  #:export (wlr-dmabuf-attributes-finish
            wlr-dmabuf-attributes-copy
            .width .height .format .modifier .n-planes .offset .stride .fd))
(define-wlr-types-class wlr-dmabuf-attributes ()
  (width #:accessor .width)
  (height #:accessor .height)
  (format #:accessor .format)
  (modifier #:accessor .modifier)
  (n-planes #:accessor .n-planes)
  (offset #:accessor .offset)
  (stride #:accessor .stride)
  (fd #:accessor .fd)
  #:descriptor %wlr-dmabuf-attributes-struct)

(define-wlr-procedure (wlr-dmabuf-attributes-finish attribs)
  (ffi:void "wlr_dmabuf_attributes_finish" (list '*))
  (% (unwrap-wlr-dmabuf-attributes attribs)))
(define-wlr-procedure (wlr-dmabuf-attributes-copy dst src)
  (ffi:int8 "wlr_dmabuf_attributes_copy" (list '* '*))
  (not (zero? (% (unwrap-wlr-dmabuf-attributes dst)
                 (unwrap-wlr-dmabuf-attributes src)))))