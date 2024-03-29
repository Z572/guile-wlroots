(define-module (wlroots render renderer)
  #:use-module (wayland server display)
  #:use-module (wayland signal)
  #:use-module (wlroots utils)
  #:use-module (wlroots backend)
  #:use-module (wlroots render drm-format-set)
  #:use-module (wlroots render texture)
  #:use-module (wlroots types)
  #:use-module (wlroots types matrix)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructure-class)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wrap-wlr-renderer
            unwrap-wlr-renderer wlr-renderer-autocreate
            wlr-renderer-init-wl-display
            .rendering
            .rendering-with-buffer
            call-with-renderer))


(define-wlr-types-class wlr-renderer ()
  (rendering #:getter .rendering #:allocation #:bytestructure)
  (rendering-with-buffer #:getter .rendering-with-buffer #:allocation #:bytestructure)
  #:descriptor %wlr-renderer-struct)

(define-wlr-procedure (wlr-renderer-autocreate backend)
  ('* "wlr_renderer_autocreate" (list '*))
  (wrap-wlr-renderer (% (unwrap-wlr-backend backend))))

(define-wlr-procedure (wlr-renderer-begin renderer width height)
  (ffi:int "wlr_renderer_begin" `(* ,ffi:uint32 ,ffi:uint32))
  (% (unwrap-wlr-renderer renderer) width height))

(define-wlr-procedure (wlr-renderer-end renderer)
  (ffi:int "wlr_renderer_end" `(*))
  (% (unwrap-wlr-renderer renderer)))

(define call-with-renderer
  (case-lambda
    ((renderer width height proc)
     (dynamic-wind
       (lambda () (wlr-renderer-begin renderer width height))
       (lambda () (proc renderer width height))
       (lambda () (wlr-renderer-end renderer))))
    ((renderer width+height proc)
     (call-with-renderer renderer (car width+height) (cdr width+height) proc ))))

(define-wlr-procedure (wlr-renderer-clear renderer color)
  (ffi:int "wlr_renderer_clear" `(* *))
  (% (unwrap-wlr-renderer renderer) (color->pointer color)))

(define-wlr-procedure (wlr-render-texture r texture projection x y alpha)
  (ffi:int8 "wlr_render_texture" (list '* '* '* ffi:int ffi:int ffi:float))
  (not (zero? (% (unwrap-wlr-renderer r)
                 (unwrap-wlr-texture texture)
                 (9-vecotr-or-list->pointer projection)
                 x
                 y
                 alpha))))

(define-wlr-procedure (wlr-render-texture-with-matrix r texture matrix alpha)
  (ffi:int8 "wlr_render_texture_with_matrix" (list '* '* '* ffi:float))
  (not (zero? (% (unwrap-wlr-renderer r)
                 (unwrap-wlr-texture texture)
                 (9-vecotr-or-list->pointer matrix)
                 alpha))))

(define-wlr-procedure (wlr-renderer-get-dmabuf-texture-formats renderer)
  ('* "wlr_renderer_get_dmabuf_texture_formats" (list '*))
  (wrap-wlr-drm-format-set (% (unwrap-wlr-renderer renderer))))

(define-wlr-procedure (wlr-renderer-init-wl-shm renderer display)
  (ffi:int "wlr_renderer_init_wl_shm" '(* *))
  (% (unwrap-wlr-renderer renderer) (unwrap-wl-display display)))

(define-wlr-procedure (wlr-renderer-init-wl-display renderer display)
  (ffi:int "wlr_renderer_init_wl_display" '(* *))
  (% (unwrap-wlr-renderer renderer) (unwrap-wl-display display)))

(define-wlr-procedure (wlr-renderer-destroy renderer)
  (ffi:void "wlr_renderer_destroy" '(*))
  (% (unwrap-wlr-renderer renderer)))
