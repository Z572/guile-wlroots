(define-module (wlroots backend)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (rnrs bytevectors)
  #:autoload (wlroots backend session) (wrap-wlr-session)
  #:autoload (wlroots types input-device) (wrap-wlr-input-device)
  #:autoload (wlroots types output) (wrap-wlr-output)
  #:use-module (wlroots types)
  #:use-module (wlroots config)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland server display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:export (wlr-backend-autocreate
            unwrap-wlr-backend
            wrap-wlr-backend
            wlr-backend-start
            wlr-backend-destroy))

(define-wlr-types-class wlr-backend ()
  (events (new-input wrap-wlr-input-device)
          (new-output wrap-wlr-output))
  #:descriptor %wlr-backend-struct)

(define-wlr-procedure (wlr-backend-autocreate display)
  ('* "wlr_backend_autocreate" (list '* '*))
  (let* ((session-ptr (ffi:bytevector->pointer
                       (make-bytevector (ffi:sizeof '*))))
         (out (% (unwrap-wl-display display) session-ptr)))
    (if (ffi:null-pointer? out)
        (values #f #f)
        (values (wrap-wlr-backend out)
                (wrap-wlr-session (ffi:dereference-pointer session-ptr))))))

(define-wlr-procedure (wlr-backend-start backend)
  (ffi:int8 "wlr_backend_start" (list '*))
  (assert (wlr-backend? backend))
  (not (zero? (% (unwrap-wlr-backend backend)))))

(define-wlr-procedure (wlr-backend-destroy backend)
  (ffi:void "wlr_backend_destroy" (list '*))
  (assert (wlr-backend? backend))
  (% (unwrap-wlr-backend backend)))
