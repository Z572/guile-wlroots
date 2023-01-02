(define-module (wlroots types output-management)
  #:use-module (oop goops)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland util)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:re-export (%wlr-output-manager-v1-struct)
  #:export (<wlr-output-manager-v1>
            wrap-wlr-output-manager-v1
            unwrap-wlr-output-manager-v1

            <wlr-output-configuration-v1>
            wrap-wlr-output-configuration-v1
            unwrap-wlr-output-configuration-v1

            wlr-output-manager-v1-create
            wlr-output-configuration-v1-create
            wlr-output-manager-v1-set-configuration))

(define-wlr-types-class wlr-output-manager-v1 ()
  #:descriptor %wlr-output-manager-v1-struct)

(define-wlr-types-class wlr-output-configuration-v1 ()
  #:descriptor %wlr-output-configuration-v1-struct)

(define-wlr-procedure (wlr-output-manager-v1-create display)
  ('* "wlr_output_manager_v1_create" (list '*))
  (wrap-wlr-output-manager-v1 (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-output-manager-v1-set-configuration manager config)
  (ffi:void "wlr_output_manager_v1_set_configuration" '(* *))
  (% (unwrap-wlr-output-manager-v1 manager)
     (unwrap-wlr-output-configuration-v1 config)))

(define-wlr-procedure (wlr-output-configuration-v1-create)
  ('* "wlr_output_configuration_v1_create" '())
  (wrap-wlr-output-configuration-v1 (%)))
