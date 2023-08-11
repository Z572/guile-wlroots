(define-module (wlroots backend x11)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:use-module (wayland server display)
  #:use-module (wlroots backend)
  #:use-module (wlroots utils)
  #:use-module (wlroots types output)
  #:use-module (wlroots types input-device)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (
            wlr-x11-backend-create
            wlr-x11-output-create
            wlr-backend-is-x11
            wlr-input-device-is-x11
            wlr-output-is-x11
            wlr-x11-output-set-title
            ))

(define not-zero? (negate zero?))
(define-wlr-procedure (wlr-x11-backend-create display x11-display)
  ('* "wlr_x11_backend_create" (list '* '*))
  (wrap-wlr-backend (% (unwrap-wl-display display)
                       (if x11-display
                           (ffi:string->pointer x11-display)
                           ffi:%null-pointer))))

(define-wlr-procedure (wlr-x11-output-create backend)
  ('* "wlr_x11_output_create" (list '*))
  (wrap-wlr-output (% (unwrap-wlr-backend backend))))

(define-wlr-procedure (wlr-backend-is-x11 backend)
  (ffi:int8 "wlr_backend_is_x11" (list '*))
  (not-zero? (% (unwrap-wlr-backend backend))))

(define-wlr-procedure (wlr-input-device-is-x11 device)
  (ffi:int8 "wlr_input_device_is_x11" (list '*))
  (not-zero? (% (unwrap-wlr-input-device device))))

(define-wlr-procedure (wlr-output-is-x11 output)
  (ffi:int8 "wlr_output_is_x11" (list '*))
  (not-zero? (% (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-x11-output-set-title output title)
  (ffi:void "wlr_x11_output_set_title" (list '* '*))
  (% (unwrap-wlr-output output) (ffi:string->pointer title)))
