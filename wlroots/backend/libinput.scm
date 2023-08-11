(define-module (wlroots backend libinput)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:use-module (libinput)
  #:use-module (wayland server display)
  #:use-module (wlroots backend)
  #:use-module (wlroots backend session)
  #:use-module (wlroots utils)
  #:use-module (wlroots types output)
  #:use-module (wlroots types input-device)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (wlr-libinput-backend-create
            wlr-libinput-get-device-handle
            wlr-backend-is-libinput
            wlr-input-device-is-libinput))

(define-wlr-procedure (wlr-libinput-backend-create display session)
  ('* "wlr_libinput_backend_create" (list '* '*))
  (wrap-wlr-backend (% (unwrap-wl-display display) (unwrap-wlr-session session))))

(define-wlr-procedure (wlr-libinput-get-device-handle dev)
  ('* "wlr_libinput_get_device_handle" (list '*))
  (wrap-libinput-device (% (unwrap-wlr-input-device dev))))

(define-wlr-procedure (wlr-backend-is-libinput backend)
  (ffi:int8 "wlr_backend_is_libinput" '(*))
  (not (zero? (% (unwrap-wlr-backend backend)))))
(define-wlr-procedure (wlr-input-device-is-libinput device)
  (ffi:int8 "wlr_input_device_is_libinput" '(*))
  (not (zero? (% (unwrap-wlr-input-device device)))))
