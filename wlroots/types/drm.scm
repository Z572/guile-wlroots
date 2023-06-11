(define-module (wlroots types drm)
  #:use-module (wayland display)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:export (wlr-drm-create
            .display-destroy
            .formats
            .global
            .node-name))

;;;
;; A stub implementation of Mesa's wl_drm protocol.
;;
;; It only implements the minimum necessary for modern clients to behave
;; properly. In particular, flink handles are left unimplemented.
(define-wlr-types-class wlr-drm ()
  (global #:accessor .global)

  ;; private state
  (node-name #:accessor .node-name)
  (formats #:accessor .formats)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-drm-struct)

(define-wlr-procedure (wlr-drm-create display renderer)
  ('* "wlr_drm_create" (list '* '*))
  (wrap-wlr-drm (% (unwrap-wl-display display) (unwrap-wlr-renderer renderer))))