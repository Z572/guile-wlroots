(define-module (wlroots types data-control)
  #:use-module (wayland server display)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (.devices
            .display-destroy
            .link
            .manager
            .primary-selection-offer-resource
            .resource
            .seat
            .seat-destroy
            .seat-set-primary-selection
            .seat-set-selection
            .selection-offer-resource
            .global))

(define-wlr-types-class wlr-data-control-manager-v1 ()
  (events (new-device wrap-wlr-data-control-device-v1))
  (global #:accessor .global)
  (devices #:accessor .devices)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-data-control-manager-v1-struct)

(define-wlr-types-class wlr-data-control-device-v1 ()
  (resource #:accessor .resource)
  (manager #:accessor .manager)
  (link #:accessor .link)
  (seat #:accessor .seat)
  (selection-offer-resource #:accessor .selection-offer-resource)
  (primary-selection-offer-resource
   #:accessor .primary-selection-offer-resource)
  (seat-destroy #:accessor .seat-destroy)
  (seat-set-selection #:accessor .seat-set-selection)
  (seat-set-primary-selection #:accessor .seat-set-primary-selection)
  #:descriptor %wlr-data-control-device-v1-struct)

(define-wlr-procedure (wlr-data-control-manager-v1-create display)
  ('* "wlr_data_control_manager_v1_create" (list '*))
  (wrap-wlr-data-control-manager-v1 (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-data-control-device-v1-destroy device)
  (ffi:void "wlr_data_control_device_v1_destroy" (list '*))
  (% (unwrap-wlr-data-control-device-v1 device)))
