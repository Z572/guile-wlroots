(define-module (wlroots types xdg-shell)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (wlroots types)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types seat)
  #:use-module (wayland)
  #:use-module (wlroots util box)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:export (wrap-wlr-xdg-shell
            unwrap-wlr-xdg-shell
            wrap-wlr-xdg-surface
            unwrap-wlr-xdg-surface
            wrap-wlr-xdg-toplevel-resize-event
            unwrap-wlr-xdg-toplevel-resize-event
            wrap-wlr-xdg-toplevel
            unwrap-wlr-xdg-toplevel
            wrap-wlr-xdg-popup
            unwrap-wlr-xdg-popup
            <wlr-xdg-popup>

            wlr-xdg-surface-toplevel
            wlr-xdg-shell-create
            wlr-xdg-surface-from-resource
            wlr-xdg-popup-from-resource
            wlr-xdg-toplevel-from-resource
            wlr-xdg-positioner-from-resource
            wlr-xdg-surface-ping
            wlr-xdg-toplevel-set-size
            wlr-xdg-toplevel-set-activated
            wlr-xdg-toplevel-set-maximized
            wlr-xdg-toplevel-set-fullscreen
            wlr-xdg-toplevel-set-resizing
            wlr-xdg-toplevel-set-tiled
            wlr-xdg-toplevel-set-bounds
            wlr-xdg-toplevel-set-wm-capabilities
            wlr-xdg-toplevel-send-close
            wlr-xdg-toplevel-set-parent
            wlr-xdg-popup-destroy
            wlr-xdg-popup-get-position
            wlr-xdg-positioner-rules-get-geometry
            wlr-xdg-positioner-rules-unconstrain-box
            wlr-xdg-popup-get-toplevel-coords
            wlr-xdg-popup-unconstrain-from-box
            wlr-surface-is-xdg-surface
            wlr-xdg-surface-from-wlr-surface
            wlr-xdg-surface-get-geometry
            wlr-xdg-surface-for-each-surface
            wlr-xdg-surface-schedule-configure

            .app-id
            .base
            .client
            .committed
            .configure-serial
            .current
            .data
            .edges
            .fields
            .fullscreen
            .fullscreen-output
            .fullscreen-output-destroy
            .geometry
            .grab-link
            .height
            .height
            .link
            .mapped
            .max-height
            .max-width
            .maximized
            .min-height
            .min-width
            .parent
            .pending
            .ping-serial
            .ping-timeout
            .ping-timer
            .popups
            .positioner
            .reposition-token
            .requested
            .resizing
            .resource
            .role
            .rules
            .scheduled
            .scheduled-serial
            .seat
            .shell
            .surface
            .surfaces
            .tiled
            .title
            .width
            .activated))

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

(define-wlr-types-class wlr-xdg-popup-configure ()
  (fields #:accessor .fields)
  (geometry #:accessor .geometry)
  (rules #:accessor .rules)
  (reposition-token #:accessor .reposition-token)
  #:descriptor %wlr-xdg-popup-configure-struct)
(define-wlr-types-class wlr-xdg-popup ()
  (base #:accessor .base)
  (link #:accessor .link)
  (resource #:accessor .resource)
  (committed #:accessor .committed)
  (parent #:accessor .parent)
  (seat #:accessor .seat)
  (scheduled #:accessor .scheduled)
  (current #:accessor .current)
  (pending #:accessor .pending)
  (grab-link #:accessor .grab-link)
  #:descriptor %wlr-xdg-popup-struct)

(define-wlr-types-class wlr-xdg-popup-state ()
  (geometry #:accessor .geometry)
  (reactive #:accessor .reactive)
  #:descriptor %wlr-xdg-popup-state-struct)

(define-wlr-types-class-public wlr-xdg-toplevel-state ()
  (maximized  #:accessor .maximized )
  (fullscreen #:accessor .fullscreen)
  (resizing   #:accessor .resizing  )
  (activated  #:accessor .activated )
  (tiled      #:accessor .tiled     )
  (width      #:accessor .width     )
  (height     #:accessor .height    )
  (max-width  #:accessor .max-width )
  (max-height #:accessor .max-height)
  (min-width  #:accessor .min-width )
  (min-height #:accessor .min-height)

  #:descriptor %wlr-xdg-toplevel-state-struct)

(define-wlr-types-class wlr-xdg-toplevel-configure ()
  (maximized #:accessor .maximized)
  (fullscreen #:accessor .fullscreen)
  (resizing #:accessor .resizing)
  (activated #:accessor .activated)
  (tiled #:accessor .tiled)
  (width #:accessor .width)
  (height #:accessor .height)
  #:descriptor %wlr-xdg-toplevel-configure-struct)

(define-wlr-types-class wlr-xdg-toplevel-requested ()
  (maximized #:accessor .maximized)
  (minimized #:accessor .minimized)
  (fullscreen #:accessor .fullscreen)
  (fullscreen-output #:accessor .fullscreen-output)
  (fullscreen-output-destroy #:accessor .fullscreen-output-destroy)
  #:descriptor %wlr-xdg-toplevel-requested-struct)

(define-wlr-types-class wlr-xdg-toplevel ()
  (resource #:accessor .resource)
  (base  #:accessor .base)
  (added #:accessor .added)
  (parent #:accessor .parent)
  (parent-unmap #:accessor .parent-unmap)
  (current #:accessor .current)
  (pending #:accessor .pending)
  (scheduled #:accessor .scheduled)
  (requested #:accessor .requested)
  (title #:accessor .title)
  (app-id #:accessor .app-id)
  #:descriptor %wlr-xdg-toplevel-struct)

(define-wlr-types-class wlr-xdg-positioner-rules ()
  (anchor-rect #:accessor .anchor-rect)
  (anchor #:accessor .anchor)
  (gravity #:accessor .gravity)
  (constraint-adjustment #:accessor .constraint-adjustment)
  (reactive #:accessor .reactive)
  (has-parent-configure-serial #:accessor .has-parent-configure-serial)
  (parent-configure-serial #:accessor .parent-configure-serial)

  #:descriptor %wlr-xdg-positioner-rules-struct)
(define-wlr-types-class wlr-xdg-positioner ()
  (resource #:accessor .resource)
  (rules #:accessor .rules)
  #:descriptor %wlr-xdg-positioner-struct)

(define-wlr-types-class wlr-xdg-toplevel-move-event ()
  (toplevel #:accessor .toplevel)
  (seat #:accessor .seat)
  (serial #:accessor .serial)
  #:descriptor %wlr-xdg-toplevel-move-event-struct)

(define-wlr-types-class wlr-xdg-toplevel-resize-event ()
  (toplevel #:accessor .toplevel)
  (seat #:accessor .seat)
  (serial #:accessor .serial)
  (edges #:accessor .edges)
  #:descriptor %wlr-xdg-toplevel-resize-event-struct)

(define-wlr-types-class-public wlr-xdg-toplevel-set-fullscreen-event ()
  #:descriptor %wlr-xdg-toplevel-set-fullscreen-event)

(define-method (.edges (o <wlr-xdg-toplevel-resize-event>))
  (bytestructure-ref
   (pointer->bytestructure (unwrap-wlr-xdg-toplevel-resize-event o) %wlr-xdg-toplevel-resize-event-struct)
   'edges))

(define-wlr-types-class wlr-xdg-client ()
  (shell #:accessor .shell)
  (resource #:accessor .resource)
  (client #:accessor .client)
  (surfaces #:accessor .surfaces)
  (link #:accessor .link)
  (ping-serial #:accessor .ping-serial)
  (ping-timer #:accessor .ping-timer)
  #:descriptor %wlr-xdg-client-struct)

(define-wlr-types-class wlr-xdg-surface ()
  (client #:accessor .client)
  (resource #:accessor .resource)
  (surface #:accessor .surface)
  (role #:accessor .role )
  (popups #:accessor .popups)
  (added  #:accessor .added)
  (configured #:accessor .configured)
  (mapped #:accessor .mapped)

  (scheduled-serial #:accessor .scheduled-serial)

  (current #:accessor .current)
  (pending #:accessor .pending)
  (data #:accessor .data)
  #:descriptor %wlr-xdg-surface-struct)

(define-wlr-types-class wlr-xdg-toplevel-show-window-menu-event ()
  (toplevel #:accessor .toplevel)
  (seat #:accessor .seat)
  (serial #:accessor .serial)
  (x #:accessor .x)
  (y #:accessor .y)
  #:descriptor %wlr-xdg-toplevel-show-window-menu-event-struct)

(define-public (wlr-xdg-surface-popups xdg-surface)
  (assert (wlr-xdg-surface? xdg-surface))
  (wl-list->list (.popups xdg-surface) <wlr-xdg-popup> 'link))

(define-wlr-procedure (wlr-xdg-shell-create display version)
  ('* "wlr_xdg_shell_create" (list '* ffi:uint32))
  (wrap-wlr-xdg-shell (% (unwrap-wl-display display) version)))

(define-wlr-procedure (wlr-xdg-surface-from-resource resource)
  ('* "wlr_xdg_surface_from_resource" (list '*))
  (wrap-wlr-xdg-surface(% (unwrap-wl-resource resource))))

(define-wlr-procedure (wlr-xdg-popup-from-resource resource)
  ('* "wlr_xdg_popup_from_resource" (list '*))
  (wrap-wlr-xdg-popup (% (unwrap-wl-resource resource))))

(define-wlr-procedure (wlr-xdg-toplevel-from-resource resource)
  ('* "wlr_xdg_toplevel_from_resource" (list '*))
  (wrap-wlr-xdg-toplevel (% (unwrap-wl-resource resource))))

(define-wlr-procedure (wlr-xdg-positioner-from-resource resource)
  ('* "wlr_xdg_positioner_from_resource" (list '*))
  (wrap-wlr-xdg-positioner(% (unwrap-wl-resource resource))))

(define-wlr-procedure (wlr-xdg-surface-ping surface)
  (ffi:void "wlr_xdg_surface_ping" (list '*))
  (% (unwrap-wlr-xdg-surface surface)))

(define-wlr-procedure (wlr-xdg-toplevel-set-size toplevel width height)
  (ffi:uint32 "wlr_xdg_toplevel_set_size" (list '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-xdg-toplevel toplevel) width height))

(define-wlr-procedure (wlr-xdg-toplevel-set-activated toplevel activated)
  (ffi:uint32 "wlr_xdg_toplevel_set_activated" (list '* ffi:int))
  "Returns the associated configure serial."
  (% (unwrap-wlr-xdg-toplevel toplevel) (if activated 1 0)))

(define-wlr-procedure (wlr-xdg-toplevel-set-maximized toplevel maximized)
  (ffi:uint32 "wlr_xdg_toplevel_set_maximized" (list '* ffi:int8))
  (% (unwrap-wlr-xdg-toplevel toplevel) (if maximized 1 0)))

(define-wlr-procedure (wlr-xdg-toplevel-set-fullscreen toplevel fullscreen)
  (ffi:uint32 "wlr_xdg_toplevel_set_fullscreen" (list '* ffi:int))
  (% (unwrap-wlr-xdg-toplevel toplevel) (if fullscreen 1 0)))

(define-wlr-procedure (wlr-xdg-toplevel-set-resizing toplevel fullscreen)
  (ffi:uint32 "wlr_xdg_toplevel_set_resizing" (list '* ffi:int))
  (% (unwrap-wlr-xdg-toplevel toplevel) (if fullscreen 1 0)))

(define-wlr-procedure (wlr-xdg-toplevel-set-tiled toplevel tiled-edges)
  (ffi:uint32 "wlr_xdg_toplevel_set_tiled" (list '* ffi:uint32))
  (% (unwrap-wlr-xdg-toplevel toplevel) tiled-edges))

(define-wlr-procedure (wlr-xdg-toplevel-set-bounds toplevel width height)
  (ffi:uint32 "wlr_xdg_toplevel_set_bounds" (list '* ffi:int32 ffi:int32))
  (% (unwrap-wlr-xdg-toplevel toplevel) width height))

(define-wlr-procedure (wlr-xdg-toplevel-set-wm-capabilities toplevel caps)
  (ffi:uint32 "wlr_xdg_toplevel_set_wm_capabilities" (list '* ffi:uint32))
  (% (unwrap-wlr-xdg-toplevel toplevel) caps))

(define-wlr-procedure (wlr-xdg-toplevel-send-close toplevel)
  (ffi:void "wlr_xdg_toplevel_send_close" (list '*))
  "Request that this xdg toplevel closes."
  (% (unwrap-wlr-xdg-toplevel toplevel)))

(define-wlr-procedure (wlr-xdg-toplevel-set-parent toplevel parent)
  (ffi:int8 "wlr_xdg_toplevel_set_parent" (list '* '*))
  (not (zero? (% (unwrap-wlr-xdg-toplevel toplevel)
                 (unwrap-wlr-xdg-toplevel parent)))))

(define-wlr-procedure (wlr-xdg-popup-destroy popup)
  (ffi:void "wlr_xdg_popup_destroy" (list '*))
  (% (unwrap-wlr-xdg-popup popup)))

(define-wlr-procedure (wlr-xdg-popup-get-position popup)
  (ffi:void "wlr_xdg_popup_get_position" (list '* '* '*))
  (let* ((x (bytestructure double))
         (y (bytestructure double)))
    (% (unwrap-wlr-xdg-popup popup)
       (bytestructure->pointer x)
       (bytestructure->pointer y))
    (cons (bytestructure-ref x) (bytestructure-ref y))))

(define-wlr-procedure (wlr-xdg-positioner-rules-get-geometry rules #:optional (box (make <wlr-box>)))
  (ffi:void "wlr_xdg_positioner_rules_get_geometry" (list '* '*))
  (% (unwrap-wlr-xdg-positioner-rules rules) (unwrap-wlr-box box))
  box)

(define-wlr-procedure (wlr-xdg-positioner-rules-unconstrain-box rules constraint box)
  (ffi:void "wlr_xdg_positioner_rules_unconstrain_box" (list '* '* '*))
  (% (unwrap-wlr-xdg-positioner-rules rules)
     (unwrap-wlr-box constraint)
     (unwrap-wlr-box box)))

(define-wlr-procedure (wlr-xdg-popup-get-toplevel-coords
                       popup
                       popup-sx
                       popup-sy)
  (ffi:void "wlr_xdg_popup_get_toplevel_coords"
            (list '* ffi:int ffi:int '* '*))
  (let* ((toplevel-sx (bytestructure int))
         (toplevel-sy (bytestructure int)))
    (% (unwrap-wlr-xdg-popup popup) popup-sx popup-sy
       (bytestructure->pointer toplevel-sx)
       (bytestructure->pointer toplevel-sy))
    (cons (bytestructure-ref toplevel-sx)
          (bytestructure-ref toplevel-sy))))

(define-wlr-procedure (wlr-xdg-popup-unconstrain-from-box
                       popup #:optional (box (make <wlr-box>)))
  (ffi:void "wlr_xdg_popup_unconstrain_from_box" (list '* '*))
  (% (unwrap-wlr-xdg-popup popup) (unwrap-wlr-box box))
  box)

(define-wlr-procedure (wlr-surface-is-xdg-surface surface)
  (ffi:int8 "wlr_surface_is_xdg_surface" '(*))
  (and (wlr-surface? surface)
       (not (zero? (% (unwrap-wlr-surface surface))))))

(define-wlr-procedure (wlr-xdg-surface-from-wlr-surface surface)
  ('* "wlr_xdg_surface_from_wlr_surface" '(*))
  (wrap-wlr-xdg-surface
   (% (unwrap-wlr-surface surface))))

(define-super-surface-from-surface
  wlr-surface-is-xdg-surface
  wlr-xdg-surface-from-wlr-surface)

(define-wlr-procedure (wlr-xdg-surface-get-geometry surface)
  (ffi:void "wlr_xdg_surface_get_geometry" (list '* '*))
  "return a box"
  (let ((box (make <wlr-box>)))
    (% (unwrap-wlr-xdg-surface surface) (unwrap-wlr-box box))
    box))

(define-wlr-procedure (wlr-xdg-surface-for-each-surface proc xdg-surface)
  (ffi:void "wlr_xdg_surface_for_each_surface" `(* * *))
  (% (unwrap-wlr-xdg-surface xdg-surface)
     (ffi:procedure->pointer
      ffi:void (lambda (surface sx sy data)
                 (proc (wrap-wlr-surface surface) sx sy))
      (list '* ffi:int ffi:int '*))
     ffi:%null-pointer))

(define-wlr-procedure (wlr-xdg-surface-for-each-popup-surface proc xdg-surface)
  (ffi:void "wlr_xdg_surface_for_each_popup_surface" `(* * *))
  (% (unwrap-wlr-xdg-surface xdg-surface)
     (ffi:procedure->pointer
      ffi:void (lambda (surface sx sy data)
                 (proc (wrap-wlr-surface surface) sx sy))
      (list '* ffi:int ffi:int '*))
     ffi:%null-pointer))

(define-wlr-procedure (wlr-xdg-surface-schedule-configure surface)
  (ffi:uint32 "wlr_xdg_surface_schedule_configure" (list '*))
  (% (unwrap-wlr-xdg-surface surface)))
