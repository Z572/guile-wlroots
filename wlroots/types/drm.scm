(define-module (wlroots types drm)
  #:use-module (wayland server display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:export (wrap-wlr-drm
            unwrap-wlr-drm
            wlr-drm-create
            .display-destroy
            .renderer-destroy
            .global
            .node-name))

(define-wlr-types-class wlr-drm ()
  (global #:accessor .global)
  (renderer #:accessor .renderer)
  (node-name #:accessor .node-name)
  (display-destroy #:accessor .display-destroy)
  (renderer-destroy #:accessor .renderer-destroy)
  #:descriptor %wlr-drm-struct)

(define-wlr-procedure (wlr-drm-create display renderer)
  ('* "wlr_drm_create" (list '* '*))
  (wrap-wlr-drm (% (unwrap-wl-display display) (unwrap-wlr-renderer renderer))))
