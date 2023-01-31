(define-module (wlroots types output-management)
  #:use-module (oop goops)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland util)
  #:use-module (wlroots types)
  #:use-module (wlroots types output)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (<wlr-output-manager-v1>
            wrap-wlr-output-manager-v1
            unwrap-wlr-output-manager-v1

            <wlr-output-configuration-v1>
            wrap-wlr-output-configuration-v1
            unwrap-wlr-output-configuration-v1

            wlr-output-manager-v1-create
            wlr-output-configuration-head-v1-create
            wlr-output-configuration-v1-create
            wlr-output-manager-v1-set-configuration
            .state
            .output
            .enabled
            .mode
            .x
            .y
            .scale
            .resource))

(define-wlr-types-class wlr-output-manager-v1 ()
  #:descriptor %wlr-output-manager-v1-struct)

(define-wlr-types-class wlr-output-configuration-v1 ()
  #:descriptor %wlr-output-configuration-v1-struct)

(define-wlr-types-class wlr-output-head-v1-state ()
  (output #:accessor .output)
  (enabled #:accessor .enabled)
  (mode #:accessor .mode)
  (x #:accessor .x)
  (y #:accessor .y)
  (scale #:accessor .scale)
  #:descriptor %wlr-output-head-v1-state-struct)

(define-wlr-types-class wlr-output-configuration-head-v1 ()
  (state #:accessor .state)
  (config #:accessor .config)
  (resource #:accessor .resource)
  (output-destroy #:accessor .output-destroy)
  #:descriptor %wlr-output-configuration-head-v1-struct)

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

(define-wlr-procedure (wlr-output-configuration-v1-destroy config)
  (ffi:void "wlr_output_configuration_v1_destroy" (list '*))
  (% (unwrap-wlr-output-configuration-v1 config)))

(define-wlr-procedure (wlr-output-configuration-v1-send-succeeded config)
  (ffi:void "wlr_output_configuration_v1_send_succeeded" (list '*))
  (% (unwrap-wlr-output-configuration-v1 config)))

(define-wlr-procedure (wlr-output-configuration-v1-send-failed config)
  (ffi:void "wlr_output_configuration_v1_send_failed" (list '*))
  (% (unwrap-wlr-output-configuration-v1 config)))

(define-wlr-procedure (wlr-output-configuration-head-v1-create config output)
  ('* "wlr_output_configuration_head_v1_create" '(* *))
  (wrap-wlr-output-configuration-head-v1
   (% (unwrap-wlr-output-configuration-v1 config) (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-output-head-v1-state-apply head_state output_state)
  (ffi:void "wlr_output_head_v1_state_apply" (list '* '*))
  (% (unwrap-wlr-output-head-v1-state head_state) (unwrap-wlr-output-state output_state)))
