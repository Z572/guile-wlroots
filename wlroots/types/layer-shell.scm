(define-module (wlroots types layer-shell)
  #:use-module (wlroots types surface)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland list)
  #:use-module (wayland display)
  #:use-module (wlroots types output)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:re-export (%wlr-layer-shell-v1-struct
               %wlr-layer-surface-v1-struct
               %wlr-layer-surface-v1-state-struct)
  #:export (wrap-wlr-layer-shell
            unwrap-wlr-layer-shell
            wrap-wlr-layer-surface-v1
            unwrap-wlr-layer-surface-v1
            wrap-wlr-layer-surface-v1-state
            unwrap-wlr-layer-surface-v1-state
            wlr-layer-shell-v1-create
            .surface
            .output
            .shell
            .namespace
            .added
            .configured
            .mapped
            .current
            .pending
            .data
            .committed
            .anchor
            .exclusive-zone
            .keyboard-interactive
            .desired-width
            .desired-height
            .layer
            .configure-serial
            .actual-width
            .actual-height

            wlr-layer-surface-v1-from-wlr-surface))

(define-wlr-types-class wlr-layer-shell ()
  #:descriptor %wlr-layer-shell-v1-struct)

(define-wlr-types-class wlr-layer-surface-v1 ()
  (surface     #:allocation #:bytestructure #:accessor .surface)
  (output      #:allocation #:bytestructure #:accessor .output)
  (shell       #:allocation #:bytestructure #:accessor .shell)
  (namespace   #:allocation #:bytestructure #:accessor .namespace)
  (added       #:allocation #:bytestructure #:accessor .added)
  (configured  #:allocation #:bytestructure #:accessor .configured)
  (mapped      #:allocation #:bytestructure #:accessor .mapped)
  (current     #:allocation #:bytestructure #:accessor .current)
  (pending     #:allocation #:bytestructure #:accessor .pending)
  (data        #:allocation #:bytestructure #:accessor .data)
  #:descriptor %wlr-layer-surface-v1-struct)

(define-wlr-types-class wlr-layer-surface-v1-state ()
  (committed #:accessor .committed)
  (anchor #:accessor .anchor)
  (exclusive-zone #:accessor .exclusive-zone)
  (keyboard-interactive #:accessor .keyboard-interactive)
  (desired-width #:accessor .desired-width)
  (desired-height #:accessor .desired-height)
  (layer #:accessor .layer)
  (configure-serial #:accessor .configure-serial)
  (actual-width #:accessor .actual-width)
  (actual-height #:accessor .actual-height)
  #:descriptor %wlr-layer-surface-v1-state-struct)

(define-wlr-procedure (wlr-layer-shell-v1-create display)
  ('* "wlr_layer_shell_v1_create" '(*))
  (wrap-wlr-layer-shell (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-layer-surface-v1-from-wlr-surface surface)
  ('* "wlr_layer_surface_v1_from_wlr_surface" '(*))
  (wrap-wlr-layer-surface-v1 (% (unwrap-wlr-surface surface))))
