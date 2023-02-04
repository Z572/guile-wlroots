(define-module (wlroots types gamma-control)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wayland display)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:duplicates
  (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.controls
            .data
            .display-destroy
            .link
            .output
            .output-commit-listener
            .output-destroy-listener
            .ramp-size
            .resource
            .table
            .global))

(define-wlr-types-class wlr-gamma-control-manager-v1 ()
  (global #:accessor .global)
  (controls #:accessor .controls)
  (display-destroy #:accessor .display-destroy)
  (events #:accessor .events)
  (data #:accessor .data)
  #:descriptor %wlr-gamma-control-manager-v1-struct)

(define-wlr-types-class wlr-gamma-control-v1 ()
  (resource #:accessor .resource)
  (output #:accessor .output)
  (link #:accessor .link)
  (table #:accessor .table)
  (ramp-size #:accessor .ramp-size)
  (output-commit-listener #:accessor .output-commit-listener)
  (output-destroy-listener #:accessor .output-destroy-listener)
  (data #:accessor .data)
  #:descriptor %wlr-gamma-control-v1-struct)


(define-wlr-procedure (wlr-gamma-control-manager-v1-create display)
  ('* "wlr_gamma_control_manager_v1_create" (list '*))
  (wrap-wlr-gamma-control-manager-v1 (% (unwrap-wl-display display))))
