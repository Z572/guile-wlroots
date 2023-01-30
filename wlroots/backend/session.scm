(define-module (wlroots backend session)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots backend)
  #:export (;; wlr-backend-get-session
            wlr-session-change-vt
            wrap-wlr-session
            unwrap-wlr-session
            .active
            .vtnr
            .seat
            .udev
            .mon
            .udev-event
            .seat-handle
            .libseat-event
            .devices
            .display
            .display-destroy
            .events))

(define-wlr-types-class wlr-device ()
  (fd #:accessor .fd)
  (device-id #:accessor .device-id)
  (dev #:accessor .dev)
  (link #:accessor .link)
  #:descriptor %wlr-device-struct)

(define-wlr-types-class wlr-session ()
  (active #:accessor .active)
  (vtnr #:accessor .vtnr)
  (seat #:accessor .seat)
  (udev #:accessor .udev)
  (mon #:accessor .mon)
  (udev-event #:accessor .udev-event)
  (seat-handle #:accessor .seat-handle)
  (libseat-event #:accessor .libseat-event)
  (devices #:accessor .devices)
  (display #:accessor .display)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-session-struct)

(define-wlr-types-class wlr-session-add-event ()
  (path #:accessor .path)
  #:descriptor  %wlr-session-add-event-struct)

(define-wlr-types-class wlr-device-hotplug-event ()
  (connector-id #:accessor .connector-id)
  (prop-id #:accessor .prop-id)
  #:descriptor %wlr-device-hotplug-event-struct)

;; (define-wlr-procedure (wlr-backend-get-session backend)
;;   ('* "wlr_backend_get_session" (list '*))
;;   (wrap-wlr-session (% (unwrap-wlr-backend backend))))

(define-wlr-procedure
  (wlr-session-create disp)
  ('* "wlr_session_create" (list '*))
  (wrap-wlr-session (% disp)))
(define-wlr-procedure
  (wlr-session-destroy session)
  (ffi:void "wlr_session_destroy" (list '*))
  (% (unwrap-wlr-session session)))
(define-wlr-procedure
  (wlr-session-open-file session path)
  ('* "wlr_session_open_file" (list '* '*))
  (wrap-wlr-device
   (% (unwrap-wlr-session session) (ffi:string->pointer path))))
(define-wlr-procedure
  (wlr-session-close-file session device)
  (ffi:void "wlr_session_close_file" (list '* '*))
  (% (unwrap-wlr-session session) (unwrap-wlr-device device)))
(define-wlr-procedure (wlr-session-change-vt session vt)
  (ffi:int "wlr_session_change_vt" (list '* ffi:unsigned-int))
  (not (zero? (% (unwrap-wlr-session session ) vt))))

;; (define-wlr-procedure
;;   (wlr-session-find-gpus session ret_len ret)
;;   (#f "wlr_session_find_gpus" (list '* ffi:size_t '*))
;;   (% (unwrap-wlr-session session) ret_len ret))
