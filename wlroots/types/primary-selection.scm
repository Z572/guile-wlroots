(define-module (wlroots types primary-selection)
  #:use-module (wayland display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module (wlroots types seat)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wlr-primary-selection-source-destroy
            wlr-primary-selection-source-send
            wlr-seat-request-set-primary-selection
            wlr-seat-set-primary-selection
            .data
            .devices
            .display-destroy
            .global
            .link
            .manager
            .mime-types
            .offers
            .resources
            .seat
            .seat-destroy
            .seat-focus-change
            .seat-set-primary-selection))

(define-wlr-types-class wlr-primary-selection-source ()
  (mime-types #:accessor .mime-types)
  (data #:accessor .data)
  #:descriptor %wlr-primary-selection-source-struct)

(define-wlr-procedure (wlr-primary-selection-source-destroy source)
  (ffi:void "wlr_primary_selection_source_destroy" (list '*))
  (% (unwrap-wlr-primary-selection-source source)))
(define-wlr-procedure (wlr-primary-selection-source-send source mime-type fd)
  (ffi:void "wlr_primary_selection_source_send" (list '* '* ffi:int))
  (% (unwrap-wlr-primary-selection-source source)
     (ffi:string->pointer mime-type) fd))
(define-wlr-procedure (wlr-seat-request-set-primary-selection
                       seat client source serial)
  (ffi:void "wlr_seat_request_set_primary_selection" (list '* '* '* ffi:uint32))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-seat-client client)
     (unwrap-wlr-primary-selection-source source) serial))
(define-wlr-procedure (wlr-seat-set-primary-selection seat source serial)
  (ffi:void "wlr_seat_set_primary_selection" (list '* '* ffi:uint32))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-primary-selection-source source) serial))

;; wlr/types/wlr_primary_selection_v1.h

(define-wlr-types-class wlr-primary-selection-v1-device-manager ()
  (global #:accessor .global)
  (devices #:accessor .devices)
  (display-destroy #:accessor .display-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-primary-selection-v1-device-manager-struct)

(define-wlr-types-class wlr-primary-selection-v1-device ()
  (manager #:accessor .manager)
  (seat #:accessor .seat)
  (link #:accessor .link)
  (resources #:accessor .resources)
  (offers #:accessor .offers)
  (seat-destroy #:accessor .seat-destroy)
  (seat-focus-change #:accessor .seat-focus-change)
  (seat-set-primary-selection #:accessor .seat-set-primary-selection)
  (data #:accessor .data)
  #:descriptor %wlr-primary-selection-v1-device-struct)

(define-wlr-procedure (wlr-primary-selection-v1-device-manager-create display)
  ('* "wlr_primary_selection_v1_device_manager_create" (list '*))
  (wrap-wlr-primary-selection-v1-device-manager
   (% (unwrap-wl-display display))))
