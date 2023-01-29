(define-module (wlroots backend multi)
  #:use-module (wayland display)
  #:use-module (wlroots backend)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (wlr-multi-backend-create
            wlr-multi-backend-add
            wlr-multi-backend-remove
            wlr-backend-is-multi
            wlr-multi-is-empty
            wlr-multi-for-each-backend))
(define-wlr-procedure (wlr-multi-backend-create display)
  ('* "wlr_multi_backend_create" (list '*))
  (wrap-wlr-backend (% (unwrap-wl-display display))))
(define-wlr-procedure (wlr-multi-backend-add multi backend)
  (ffi:int8 "wlr_multi_backend_add" (list '* '*))
  (not (zero? (% (unwrap-wlr-backend multi) (unwrap-wlr-backend backend)))))
(define-wlr-procedure (wlr-multi-backend-remove multi backend)
  (ffi:void "wlr_multi_backend_remove" (list '* '*))
  (% (unwrap-wlr-backend multi) (unwrap-wlr-backend backend)))
(define-wlr-procedure (wlr-backend-is-multi backend)
  (ffi:int8 "wlr_backend_is_multi" (list '*))
  (not (zero? (% (unwrap-wlr-backend backend)))))
(define-wlr-procedure (wlr-multi-is-empty backend)
  (ffi:int8 "wlr_multi_is_empty" (list '*))
  (not (zero? (% (unwrap-wlr-backend backend)))))

(define-wlr-procedure (wlr-multi-for-each-backend backend callback)
  (ffi:void "wlr_multi_for_each_backend" (list '* '* '*))
  (% (unwrap-wlr-backend backend)
     (ffi:procedure->pointer
      ffi:void
      (lambda (backend data)
        (callback (wrap-wlr-backend backend)))
      '(* *))
     ffi:%null-pointer))
