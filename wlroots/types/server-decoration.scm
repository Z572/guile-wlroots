(define-module (wlroots types server-decoration)
  #:use-module (wayland display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module (wlroots types seat)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wlr-server-decoration-manager-create
            wlr-server-decoration-manager-set-default-mode
            .resource
            .surface
            .link
            .mode
            .surface-destroy-listener
            .global
            .resources
            .decorations
            .default-mode
            .display-destroy
            .data))

(define-wlr-types-class wlr-server-decoration-manager ()
  (global #:accessor .global)
  (resources #:accessor .resources)
  (decorations #:accessor .decorations)
  (default-mode #:accessor .default-mode)
  (display-destroy #:accessor .display-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-server-decoration-manager-struct)

(define-wlr-types-class wlr-server-decoration ()
  (resource #:accessor .resource)
  (surface #:accessor .surface)
  (link #:accessor .link)
  (mode #:accessor .mode)
  (surface-destroy-listener #:accessor .surface-destroy-listener)
  (data #:accessor .data)
  #:descriptor %wlr-server-decoration-struct)

(define-wlr-procedure (wlr-server-decoration-manager-create display)
  ('* "wlr_server_decoration_manager_create" (list '*))
  (wrap-wlr-server-decoration-manager (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-server-decoration-manager-set-default-mode manager default-mode)
  (ffi:void "wlr_server_decoration_manager_set_default_mode" (list '* ffi:uint32))
  (% (unwrap-wlr-server-decoration-manager manager) default-mode))
