(define-module (wlroots backend)
  #:use-module (wlroots types)
  #:use-module (wlroots config)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:re-export (%wlr-backend-struct)
  #:export (wlr-backend-autocreate
            unwrap-wlr-backend
            wrap-wlr-backend
            wlr-backend-start
            wlr-backend-destroy))

(define-wlr-types-class wlr-backend ()
  #:descriptor %wlr-backend-struct)

(define-wlr-procedure (wlr-backend-autocreate display)
  ('* "wlr_backend_autocreate" (list '*))
  (wrap-wlr-backend (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-backend-start backend)
  (ffi:int "wlr_backend_start" (list '*))
  (% (unwrap-wlr-backend backend)))

(define-wlr-procedure (wlr-backend-destroy backend)
  (ffi:void "wlr_backend_destroy" (list '*))
  (% (unwrap-wlr-backend backend)))
