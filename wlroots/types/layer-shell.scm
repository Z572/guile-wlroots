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
  #:re-export (%wlr-layer-shell-v1-struct
               %wlr-layer-surface-v1-struct
               %wlr-layer-surface-v1-status-struct)
  #:export (wrap-wlr-layer-shell
            unwrap-wlr-layer-shell
            wrap-wlr-layer-surface-v1
            unwrap-wlr-layer-surface-v1
            wlr-layer-shell-v1-create
            .surface
            .output
            wlr-layer-surface-v1-from-wlr-surface))

(define-wlr-types-class wlr-layer-shell ()
  #:descriptor %wlr-layer-shell-v1-struct)
(define-wlr-types-class wlr-layer-surface-v1 ()
  (surface #:allocation #:bytestructure
           #:getter .surface)
  (output #:allocation #:bytestructure
          #:getter .output)
  #:descriptor %wlr-layer-surface-v1-struct)
(define-wlr-procedure (wlr-layer-shell-v1-create display)
  ('* "wlr_layer_shell_v1_create" '(*))
  (wrap-wlr-layer-shell (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-layer-surface-v1-from-wlr-surface surface)
  ('* "wlr_layer_surface_v1_from_wlr_surface" '(*))
  (wrap-wlr-layer-surface-v1 (% (unwrap-wlr-surface surface))))
