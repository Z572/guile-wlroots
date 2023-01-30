(define-module (wlroots render texture)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots render dmabuf)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (wlroots types)
  #:export (wlr-texture-from-pixels
            wlr-texture-destroy
            wlr-texture-from-buffer
            wlr-texture-from-dmabuf))

(define-wlr-types-class wlr-texture ()
  (width #:accessor .width)
  (height #:accessor .height)
  #:descriptor %wlr-texture-struct)

(define-wlr-procedure (wlr-texture-from-pixels renderer fmt stride width height
                                               data)
  ('* "wlr_texture_from_pixels" (list '* ffi:uint32 ffi:uint32 ffi:uint32
                                      ffi:uint32 '*))
  (wrap-wlr-texture (% (unwrap-wlr-renderer renderer)
                       fmt stride width height data)))

(define-wlr-procedure (wlr-texture-from-dmabuf renderer attribs)
  ('* "wlr_texture_from_dmabuf" (list '* '*))
  (wrap-wlr-texture
   (% (unwrap-wlr-renderer renderer) (unwrap-wlr-dmabuf-attributes attribs))))

(define-wlr-procedure (wlr-texture-destroy texture)
  (ffi:void "wlr_texture_destroy" (list '*))
  (% (unwrap-wlr-texture texture)))

(define-wlr-procedure (wlr-texture-from-buffer renderer buffer)
  ('* "wlr_texture_from_buffer" (list '* '*))
  (wrap-wlr-texture (% (unwrap-wlr-renderer renderer)
                       (unwrap-wlr-buffer buffer))))
