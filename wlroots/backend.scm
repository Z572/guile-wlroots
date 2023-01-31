(define-module (wlroots backend)
  #:autoload (wlroots backend session) (wrap-wlr-session)
  #:use-module (wlroots types)
  #:use-module (wlroots config)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:export (wlr-backend-autocreate
            unwrap-wlr-backend
            wrap-wlr-backend
            wlr-backend-start
            wlr-backend-destroy
            wlr-backend-get-session))

(define-wlr-types-class wlr-backend ()
  #:descriptor %wlr-backend-struct)

(define-wlr-procedure (wlr-backend-autocreate display)
  ('* "wlr_backend_autocreate" (list '*))
  (wrap-wlr-backend (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-backend-start backend)
  (ffi:int8 "wlr_backend_start" (list '*))
  (not (zero? (% (unwrap-wlr-backend backend)))))

(define-wlr-procedure (wlr-backend-destroy backend)
  (ffi:void "wlr_backend_destroy" (list '*))
  (% (unwrap-wlr-backend backend)))

(define-wlr-procedure (wlr-backend-get-session backend)
  ('* "wlr_backend_get_session" (list '*))
  (wrap-wlr-session (% (unwrap-wlr-backend backend))))
