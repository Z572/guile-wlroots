(define-module (wlroots render texture)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots render renderer)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (wlroots types)
  #:export (wlr-texture-from-pixels
            wlr-texture-is-opaque
            wlr-texture-write-pixels
            wlr-texture-destroy
            wlr-texture-from-buffer))

(define-bytestructure-class <wlr-texture> ()
  %wlr-texture-struct
  wrap-wlr-texture unwrap-wlr-texture wlr-texture?)

(define-wlr-procedure (wlr-texture-from-pixels renderer fmt stride width height
                                               data)
  ('* "wlr_texture_from_pixels" (list '* ffi:uint32 ffi:uint32 ffi:uint32
                                      ffi:uint32 '*))
  (wrap-wlr-texture (% (unwrap-wlr-renderer renderer)
                       fmt stride width height data)))

(define-wlr-procedure (wlr-texture-is-opaque texture)
  (ffi:int8 "wlr_texture_is_opaque" (list '*))
  (not (zero? (% (unwrap-wlr-texture texture)))))

(define-wlr-procedure (wlr-texture-write-pixels
                       texture stride
                       width height
                       src_x src_y
                       dst_x dst_y data)
  (ffi:int8 "wlr_texture_write_pixels"
            (list '* ffi:uint32 ffi:uint32 ffi:uint32 ffi:uint32 ffi:uint32
                  ffi:uint32 ffi:uint32 '*))
  (not (zero?
        (% (unwrap-wlr-texture texture)
           stride width height src_x src_y
           dst_x dst_y (cond ((ffi:pointer? data) data)
                             ((bytevector? data)
                              (ffi:bytevector->pointer data)))))))

(define-wlr-procedure (wlr-texture-destroy texture)
  (ffi:void "wlr_texture_destroy" (list '*))
  (% (unwrap-wlr-texture texture)))

(define-wlr-procedure (wlr-texture-from-buffer renderer buffer)
  ('* "wlr_texture_from_buffer" (list '* '*))
  (wrap-wlr-texture (% (unwrap-wlr-renderer renderer)
                       (unwrap-wlr-buffer buffer))))
