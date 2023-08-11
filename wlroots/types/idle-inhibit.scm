(define-module (wlroots types idle-inhibit)
  #:use-module (wlroots utils)
  #:use-module (wayland server display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wlr-idle-inhibit-v1-create
            .surface
            .resource
            .surface-destroy
            .link
            .data
            .inhibitors
            .global
            .display-destroy
            .data))

(define-wlr-types-class wlr-idle-inhibit-manager-v1 ()
  (inhibitors  #:accessor .inhibitors)
  (global  #:accessor .global)
  (display-destroy #:accessor .display-destroy)
  (events  #:accessor .events)
  (data  #:accessor .data)
  #:descriptor %wlr-idle-inhibit-manager-v1-struct)

(define-wlr-types-class wlr-idle-inhibitor-v1 ()
  (surface  #:accessor .surface)
  (resource #:accessor .resource)
  (surface-destroy #:accessor .surface-destroy)
  (link #:accessor .link)
  (events #:accessor .events)
  (data #:accessor .data)
  #:descriptor %wlr-idle-inhibitor-v1-struct)

(define-wlr-procedure (wlr-idle-inhibit-v1-create display)
  ('* "wlr_idle_inhibit_v1_create" (list '*))
  (wrap-wlr-idle-inhibit-manager-v1 (% (unwrap-wl-display display))))
