(define-module (wlroots types fullscreen-shell)
  #:use-module (wayland server display)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:export (<wlr-fullscreen-shell-v1-present-surface-event>
            wrap-wlr-fullscreen-shell-v1-present-surface-event
            unwrap-wlr-fullscreen-shell-v1-present-surface-event
            wlr-fullscreen-shell-v1-create
            .client
            .data
            .display-destroy
            .global
            .surface
            .method
            .output))

(define-wlr-types-class wlr-fullscreen-shell-v1 ()
  (global #:accessor .global)
  (display-destroy #:accessor .display-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-fullscreen-shell-v1-struct)

(define-wlr-types-class wlr-fullscreen-shell-v1-present-surface-event ()
  (client #:accessor .client)
  (surface #:accessor .surface)
  (method #:accessor .method)
  (output #:accessor .output)
  #:descriptor %wlr-fullscreen-shell-v1-present-surface-event-struct)

(define-wlr-procedure (wlr-fullscreen-shell-v1-create display)
  ('* "wlr_fullscreen_shell_v1_create" '(*))
  (wrap-wlr-fullscreen-shell-v1 (% (unwrap-wl-display display))))
