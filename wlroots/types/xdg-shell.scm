
(define-module (wlroots types xdg-shell)
  #:use-module (wlroots types)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types seat)
  #:use-module (wayland)
  #:use-module (wlroots util box)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:re-export (%wlr-xdg-shell-struct
               %wlr-xdg-surface-struct
               %wlr-xdg-toplevel-resize-event-struct
               %wlr-xdg-surface-configure-struct
               %wlr-xdg-toplevel-struct
               %wlr-xdg-toplevel-state-struct)
  #:export (wlr-xdg-shell-create
            wrap-wlr-xdg-shell
            unwrap-wlr-xdg-shell
            .base
            wrap-wlr-xdg-surface
            unwrap-wlr-xdg-surface
            wrap-wlr-xdg-toplevel-resize-event
            unwrap-wlr-xdg-toplevel-resize-event
            wrap-wlr-xdg-toplevel
            unwrap-wlr-xdg-toplevel
            wrap-wlr-xdg-popup
            unwrap-wlr-xdg-popup
            <wlr-xdg-popup>
            wlr-xdg-surface-mapped?
            wlr-xdg-surface-from-wlr-surface
            wlr-xdg-toplevel-set-activated
            wlr-xdg-toplevel-set-tiled
            wlr-xdg-toplevel-set-fullscreen
            wlr-xdg-toplevel-set-resizing
            wlr-xdg-toplevel-send-close
            wlr-xdg-toplevel-appid
            wlr-xdg-toplevel-title
            wlr-xdg-toplevel-set-size
            wlr-xdg-popup-unconstrain-from-box
            .edges
            wlr-xdg-surface-toplevel
            wlr-xdg-surface-get-geometry
            wlr-xdg-surface-for-each-surface
            .maximized
            .fullscreen
            .resizing
            .activated
            .parent
            .current
            .base
            .app-id
            .title
            .ping-timeout
            .data
            .geometry
            .configure-serial
            .pending
            .scheduled-serial
            .popups
            .role
            .resource))

(eval-when (expand load eval)
  (load-extension "libguile-wlroots" "scm_init_wlr_xdg_shell"))

(define-wlr-types-class wlr-xdg-shell ()
  (global #:allocation #:bytestructure #:accessor .global)
  (clients #:allocation #:bytestructure #:accessor .clients)
  (ping-timeout #:allocation #:bytestructure #:accessor .ping-timeout)
  (data #:allocation #:bytestructure #:accessor .data)
  #:descriptor %wlr-xdg-shell-struct)

(define-wlr-types-class wlr-xdg-surface-state ()
  (configure-serial #:allocation #:bytestructure #:accessor .configure-serial)
  (geometry #:allocation #:bytestructure #:accessor .geometry)
  #:descriptor %wlr-xdg-surface-state-struct)

(define-wlr-types-class wlr-xdg-popup ()
  (base #:allocation #:bytestructure
        #:getter .base)
  #:descriptor %wlr-xdg-popup-struct)




(define-wlr-types-class-public wlr-xdg-toplevel-state ()
  (maximized  #:allocator #:bytestructure #:accessor .maximized )
  (fullscreen #:allocator #:bytestructure #:accessor .fullscreen)
  (resizing   #:allocator #:bytestructure #:accessor .resizing  )
  (activated  #:allocator #:bytestructure #:accessor .activated )

  #:descriptor %wlr-xdg-toplevel-state-struct)


(define-wlr-types-class wlr-xdg-toplevel ()
  (base #:allocation #:bytestructure #:accessor .base)
  (added #:allocation #:bytestructure #:accessor .added)
  (parent #:allocation #:bytestructure #:accessor .parent)
  (current #:allocation #:bytestructure #:accessor .current)
  (title #:allocation #:bytestructure #:accessor .title)
  (app-id #:allocation #:bytestructure #:accessor .app-id)
  #:descriptor %wlr-xdg-toplevel-struct)


(define-wlr-types-class wlr-xdg-toplevel-resize-event ()
  #:descriptor %wlr-xdg-toplevel-resize-event-struct)


(define-wlr-types-class-public wlr-xdg-toplevel-set-fullscreen-event ()
  #:descriptor %wlr-xdg-toplevel-set-fullscreen-event)

(define-method (.edges (o <wlr-xdg-toplevel-resize-event>))
  (bytestructure-ref
   (pointer->bytestructure (unwrap-wlr-xdg-toplevel-resize-event o) %wlr-xdg-toplevel-resize-event-struct)
   'edges))
(define-wlr-procedure (wlr-xdg-shell-create display)
  ('* "wlr_xdg_shell_create" '(*))
  (wrap-wlr-xdg-shell
   (% (unwrap-wl-display display))))
(define-wlr-types-class wlr-xdg-surface ()
  (resource #:accessor .resource)
  (role #:accessor .role )
  (popups #:accessor .popups)
  (added  #:accessor .added)
  (configured #:accessor .configured)
  (mapped #:accessor .mapped)

  (scheduled-serial #:accessor .scheduled-serial)

  (current #:accessor .current)
  (pending #:accessor .pending)
  #:descriptor %wlr-xdg-surface-struct)

(define-wlr-procedure (wlr-xdg-surface-from-wlr-surface surface)
  ('* "wlr_xdg_surface_from_wlr_surface" '(*))
  (wrap-wlr-xdg-surface
   (% (unwrap-wlr-xdg-surface surface))))

(define-wlr-procedure (wlr-xdg-toplevel-set-activated surface activated)
  (ffi:uint32 "wlr_xdg_toplevel_set_activated" (list '* ffi:int))
  "Returns the associated configure serial."
  (% (unwrap-wlr-xdg-surface surface) (if activated 1 0)))

(define-wlr-procedure (wlr-xdg-toplevel-send-close surface)
  (ffi:void "wlr_xdg_toplevel_send_close" (list '*))
  "Request that this xdg toplevel closes."
  (% (unwrap-wlr-xdg-surface surface)))


(define-wlr-procedure (wlr-xdg-toplevel-set-tiled surface tiled-edges)
  (ffi:uint32 "wlr_xdg_toplevel_set_tiled" (list '* ffi:uint32))
  (% (unwrap-wlr-xdg-surface surface) tiled-edges))

(define-wlr-procedure (wlr-xdg-toplevel-set-fullscreen surface fullscreen)
  (ffi:uint32 "wlr_xdg_toplevel_set_fullscreen" (list '* ffi:int))
  (% (unwrap-wlr-xdg-surface surface) (if fullscreen 1 0)))

(define-wlr-procedure (wlr-xdg-toplevel-set-resizing surface fullscreen)
  (ffi:uint32 "wlr_xdg_toplevel_set_resizing" (list '* ffi:int))
  (% (unwrap-wlr-xdg-surface surface) (if fullscreen 1 0)))

(define-wlr-procedure (wlr-xdg-popup-unconstrain-from-box popup box)
  (ffi:void "wlr_xdg_popup_unconstrain_from_box" (list '* '*))
  (% (unwrap-wlr-xdg-popup popup) (unwrap-wlr-box box)))

(define-wlr-procedure (wlr-xdg-surface-get-geometry surface)
  (ffi:void "wlr_xdg_surface_get_geometry" (list '* '*))
  "return a box"
  (let ((box (make <wlr-box>)))
    (% (unwrap-wlr-xdg-surface surface) (unwrap-wlr-box box))
    box))

(define-wlr-procedure (wlr-xdg-toplevel-set-size surface width height)
  (ffi:uint32 "wlr_xdg_toplevel_set_size" (list '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-xdg-surface surface) width height))

(define-wlr-procedure (wlr-xdg-surface-for-each-surface proc xdg-surface)
  (ffi:void "wlr_xdg_surface_for_each_surface" `(* * *))
  (% (unwrap-wlr-xdg-surface xdg-surface)
     (ffi:procedure->pointer
      ffi:void (lambda (surface sx sy data)
                 (proc (wrap-wlr-surface surface) sx sy))
      (list '* ffi:int ffi:int '*))
     ffi:%null-pointer))
