(define-module (wlroots types layer-shell)
  #:autoload (wlroots types xdg-shell) (wrap-wlr-xdg-popup)
  #:use-module (wayland server resource)
  #:use-module (wayland server listener)
  #:use-module (wayland signal)
  #:use-module (wayland list)
  #:use-module (wayland server display)
  #:use-module (wlroots types)
  #:use-module (wlroots types output)
  #:use-module (wlroots types compositor)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wrap-wlr-layer-shell
            unwrap-wlr-layer-shell
            wrap-wlr-layer-surface-v1
            unwrap-wlr-layer-surface-v1
            wrap-wlr-layer-surface-v1-state
            unwrap-wlr-layer-surface-v1-state

            wlr-layer-shell-v1-create
            wlr-layer-surface-v1-configure
            wlr-layer-surface-v1-destroy
            wlr-surface-is-layer-surface
            wlr-layer-surface-v1-try-from-wlr-surface
            wlr-layer-surface-v1-from-resource

            .surface
            .output
            .shell
            .namespace
            .added
            .configured
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
            .initialized
            .initial-commit))

(define-wlr-types-class wlr-layer-shell ()
  (events (new-surface wrap-wlr-layer-surface-v1))
  #:descriptor %wlr-layer-shell-v1-struct)

(define-wlr-types-class wlr-layer-surface-v1 ()
  (events (new-popup wrap-wlr-xdg-popup))
  (surface     #:allocation #:bytestructure #:accessor .surface)
  (output      #:allocation #:bytestructure #:accessor .output)
  (shell       #:allocation #:bytestructure #:accessor .shell)
  (namespace   #:allocation #:bytestructure #:accessor .namespace)
  (added       #:allocation #:bytestructure #:accessor .added)
  (configured  #:allocation #:bytestructure #:accessor .configured)
  (initialized  #:allocation #:bytestructure #:getter .initialized)
  (initial-commit #:allocation #:bytestructure #:getter .initial-commit)
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

(define-wlr-procedure (wlr-layer-shell-v1-create display version)
  ('* "wlr_layer_shell_v1_create" (list '* ffi:uint32))
  (wrap-wlr-layer-shell (% (unwrap-wl-display display) version)))

(define-wlr-procedure (wlr-layer-surface-v1-configure surface width height)
  (ffi:uint32 "wlr_layer_surface_v1_configure" (list '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-layer-surface-v1 surface) width height))

(define-wlr-procedure (wlr-layer-surface-v1-destroy surface)
  (ffi:void "wlr_layer_surface_v1_destroy" (list '*))
  (% (unwrap-wlr-layer-surface-v1 surface)))

(define-wlr-procedure (wlr-layer-surface-v1-try-from-wlr-surface surface)
  ('* "wlr_layer_surface_v1_try_from_wlr_surface" '(*))
  (let ((o (% (unwrap-wlr-surface surface))))
    (if (ffi:null-pointer? o)
        #f
        (wrap-wlr-layer-surface-v1 o))))

(define-super-surface-from-surface wlr-layer-surface-v1-try-from-wlr-surface)

(define-wlr-procedure (wlr-layer-surface-v1-from-resource resource)
  ('* "wlr_layer_surface_v1_from_resource" (list '*))
  (wrap-wlr-layer-surface-v1 (% (unwrap-wl-resource resource))))
