(define-module (wlroots backend wayland)
  #:use-module (wayland display)
  #:use-module (wlroots backend)
  #:use-module (wlroots utils)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types output)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (wlr-wl-backend-create
            wlr-wl-backend-get-remote-display
            wlr-wl-output-create
            wlr-backend-is-wl
            wlr-input-device-is-wl
            wlr-output-is-wl
            wlr-wl-output-set-title
            ;; wlr-wl-output-get-surface
            ;; wlr-wl-input-device-get-seat
            ))

(define not-zero? (negate zero?))

(define-wlr-procedure (wlr-wl-backend-create display remote)
  ('* "wlr_wl_backend_create" (list '* '*))
  (wrap-wlr-backend (% (unwrap-wl-display display) (if remote
                                                       (ffi:string->pointer remote)
                                                       ffi:%null-pointer))))

(define-wlr-procedure (wlr-wl-backend-get-remote-display backend)
  ('* "wlr_wl_backend_get_remote_display" (list '*))
  (wrap-wl-display (% (unwrap-wlr-backend backend))))

(define-wlr-procedure (wlr-wl-output-create backend)
  ('* "wlr_wl_output_create" (list '*))
  (wrap-wlr-output (% (unwrap-wlr-backend backend))))

(define-wlr-procedure (wlr-backend-is-wl backend)
  (ffi:int8 "wlr_backend_is_wl" (list '*))
  (not-zero? (% (unwrap-wlr-backend backend))))

(define-wlr-procedure (wlr-input-device-is-wl device)
  (ffi:int8 "wlr_input_device_is_wl" (list '*))
  (not-zero? (% (unwrap-wlr-input-device device))))

(define-wlr-procedure (wlr-output-is-wl output)
  (ffi:int8 "wlr_output_is_wl" (list '*))
  (not-zero? (% (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-wl-output-set-title output title)
  (ffi:void "wlr_wl_output_set_title" (list '* '*))
  (% (unwrap-wlr-output output) (if title (ffi:string->pointer title)
                                    ffi:%null-pointer)))


#| TODO
(define-wlr-procedure (wlr-wl-output-get-surface output)
  ('* "wlr_wl_output_get_surface" (list '*))
  (wrap-wl-surface (% (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-wl-input-device-get-seat dev)
  ('* "wlr_wl_input_device_get_seat" (list '*))
  (wrap-wl-seat (% (unwrap-wlr-input-device dev))))
|#
