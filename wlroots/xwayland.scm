(define-module (wlroots xwayland)
  #:use-module (wlroots types)
  #:use-module ((system foreign ) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:export (wrap-wlr-xwayland-surface
            unwrap-wlr-xwayland-surface
            wlr-xwayland-surface-close))

(define-wlr-types-class wlr-xwayland-surface)

;; wlr_xwayland_surface_set_fullscreen
(define-wlr-procedure (wlr-xwayland-surface-close surface)
  (ffi:void "wlr_xwayland_surface_close" (list '*))
  "Request that this xdg toplevel closes."
  (% (unwrap-wlr-xwayland-surface surface)))
