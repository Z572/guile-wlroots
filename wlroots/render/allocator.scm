(define-module (wlroots render allocator)
  #:use-module (wayland display)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wlroots backend)
  #:use-module (wlroots render renderer)
  #:use-module (system foreign)
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
  (void "wlr_allocator_destroy" '(*))
  (% (unwrap-wlr-allocator allocator)))
