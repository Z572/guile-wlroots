(define-module (wlroots render renderer)
  #:use-module (wayland display)
  #:use-module (wayland signal)
  #:use-module (wlroots utils)
  #:use-module (wlroots backend)
  #:use-module (wlroots types)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wrap-wlr-renderer unwrap-wlr-renderer wlr-renderer-autocreate
                              wlr-renderer-init-wl-display
                              .rendering
                              .rendering-with-buffer))


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

(define-wlr-procedure (wlr-renderer-clear renderer color)
  (ffi:int "wlr_renderer_clear" `(* *))
  (% (unwrap-wlr-renderer renderer) (color->pointer color)))

(define-wlr-procedure (wlr-renderer-init-wl-display renderer display)
  (ffi:int "wlr_renderer_init_wl_display" '(* *))
  (% (unwrap-wlr-renderer renderer) (unwrap-wl-display display)))
