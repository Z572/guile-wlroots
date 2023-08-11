(define-module (wlroots types xdg-decoration)
  #:use-module (wayland server display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module (wlroots types seat)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.resource
            .surface
            .link
            .mode
            .surface-destroy-listener
            .global
            .resources
            .decorations
            .default-mode
            .display-destroy
            .data
            .current
            .pending
            .scheduled-mode
            .requested-mode
            .added
            .configure-list
            .surface-destroy
            .surface-configure
            .surface-ack-configure
            .surface-commit
            .manager))

(define-wlr-types-class wlr-xdg-decoration-manager-v1 ()
  (global #:accessor .global)
  (decorations #:accessor .decorations)
  (display-destroy #:accessor .display-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-xdg-decoration-manager-v1-struct)

(define-wlr-types-class wlr-xdg-toplevel-decoration-v1-configure ()
  (link #:accessor .link)
  (surface-configure #:accessor .surface-configure)
  (mode #:accessor .mode)
  #:descriptor %wlr-xdg-toplevel-decoration-v1-configure-struct)

(define-wlr-types-class wlr-xdg-toplevel-decoration-v1-state ()
  (mode #:accessor .mode)
  #:descriptor %wlr-xdg-toplevel-decoration-v1-state-struct)

(define-wlr-types-class wlr-xdg-toplevel-decoration-v1 ()
  (resource #:accessor .resource)
  (surface #:accessor .surface)
  (manager #:accessor .manager)
  (link #:accessor .link)
  (current #:accessor .current)
  (pending #:accessor .pending)
  (scheduled-mode #:accessor .scheduled-mode)
  (requested-mode #:accessor .requested-mode)
  (added #:accessor .added)
  (configure-list #:accessor .configure-list)
  (surface-destroy #:accessor .surface-destroy)
  (surface-configure #:accessor .surface-configure)
  (surface-ack-configure #:accessor .surface-ack-configure)
  (surface-commit #:accessor .surface-commit)
  (data #:accessor .data)
  #:descriptor %wlr-xdg-toplevel-decoration-v1-struct)

(define-wlr-procedure (wlr-xdg-decoration-manager-v1-create display)
  ('* "wlr_xdg_decoration_manager_v1_create" (list '*))
  (wrap-wlr-xdg-decoration-manager-v1 (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-xdg-toplevel-decoration-v1-set-mode manager mode)
  (ffi:void "wlr_xdg_toplevel_decoration_v1_set_mode" (list '* ffi:uint32))
  (% (unwrap-wlr-xdg-decoration-manager-v1 manager)
     (bs:enum->integer %wlr-xdg-toplevel-decoration-v1-mode-enum mode)))
