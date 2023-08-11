(define-module (wlroots types input-inhibitor)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wayland server display)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wlr-input-inhibit-manager-create
            .global
            .active-client
            .active-inhibitor))

(define-wlr-types-class wlr-input-inhibit-manager ()
  (global #:accessor .global)
  (active-client #:accessor .active-client)
  (active-inhibitor #:accessor .active-inhibitor)
  #:descriptor %wlr-input-inhibit-manager-struct)

(define-wlr-procedure (wlr-input-inhibit-manager-create display)
  ('* "wlr_input_inhibit_manager_create" '(*))
  (wrap-wlr-input-inhibit-manager
   (% (unwrap-wl-display display))))
