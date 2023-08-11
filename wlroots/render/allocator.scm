(define-module (wlroots render allocator)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module (wayland server display)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wlroots backend)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots render drm-format-set)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:export (wrap-wlr-allocator
            unwrap-wlr-allocator
            wlr-allocator-autocreate
            wlr-allocator-destroy
            .buffer-caps))

(define-wlr-types-class wlr-allocator ()
  (buffer-caps #:allocation #:bytestructure #:accessor .buffer-caps)
  #:descriptor %wlr-allocator-struct)

(define-wlr-procedure (wlr-allocator-autocreate backend renderer)
  ('* "wlr_allocator_autocreate" '(* *))
  (wrap-wlr-allocator (% (unwrap-wlr-backend backend)
                         (unwrap-wlr-renderer renderer))))

(define-wlr-procedure (wlr-allocator-destroy allocator)
  (ffi:void "wlr_allocator_destroy" '(*))
  (% (unwrap-wlr-allocator allocator)))

(define-wlr-procedure (wlr-allocator-create-buffer alloc width height format)
  ('* "wlr_allocator_create_buffer" (list '* ffi:int ffi:int '*))
  (wrap-wlr-buffer
   (% (unwrap-wlr-allocator alloc)
      width
      height
      (unwrap-wlr-drm-format format))))
