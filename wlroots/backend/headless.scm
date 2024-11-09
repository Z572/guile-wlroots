(define-module (wlroots backend headless)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:use-module (wayland server display)
  #:use-module (wlroots backend)
  #:use-module (wlroots utils)
  #:use-module (wlroots types output)
  #:use-module (wlroots types input-device)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (wlr-headless-backend-create
            wlr-headless-add-output
            wlr-backend-is-headless
            wlr-output-is-headless))

(define not-zero? (negate zero?))

(define-wlr-procedure (wlr-headless-backend-create display)
  ('* "wlr_headless_backend_create" (list '*))
  (wrap-wlr-backend (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-headless-add-output backend width height)
  ('* "wlr_headless_add_output" (list '* ffi:unsigned-int ffi:unsigned-int))
  (assert (wlr-backend-is-headless backend))
  (wrap-wlr-output (% (unwrap-wlr-backend backend) width height)))

(define-wlr-procedure (wlr-backend-is-headless backend)
  (ffi:int8 "wlr_backend_is_headless" (list '*))
  (not-zero? (% (unwrap-wlr-backend backend))))

(define-wlr-procedure (wlr-output-is-headless output)
  (ffi:int8 "wlr_output_is_headless" (list '*))
  (not-zero? (% (unwrap-wlr-output output))))
