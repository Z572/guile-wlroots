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
  #:export (%wlr-xdg-shell-struct
            %wlr-xdg-surface-struct
            %wlr-xdg-toplevel-resize-event-struct
            %wlr-xdg-surface-configure-struct
            %wlr-xdg-toplevel-struct
            %wlr-xdg-toplevel-state-struct
            wlr-xdg-shell-create
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
            wlr-xdg-surface-for-each-surface))

(eval-when (expand load eval)
  (load-extension "libguile-wlroots" "scm_init_wlr_xdg_shell"))


(define %wlr-xdg-shell-struct
  (bs:struct `((global ,(bs:pointer '*))
               (clients ,%wl-list)
               (popup-grabs ,%wl-list)
               (ping-timeout ,uint32)
               (display-destroy ,%wl-listener)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))
(define-wlr-types-class wlr-xdg-shell ()
  #:descriptor %wlr-xdg-shell-struct)

(define %wlr-xdg-surface-state-struct
  (bs:struct `((configure-serial ,uint32)
               (geometry ,%wlr-box-struct))))
(define %wlr-xdg-surface-struct
  (bs:struct `((client ,(bs:pointer '*))
               (resource ,(bs:pointer '*))
               (surface ,(bs:pointer '*))
               (link ,%wl-list)
               (role ,int)
               (union ,(bs:union `((toplevel
                                    ,(bs:pointer
                                      (delay %wlr-xdg-toplevel-struct)))
                                   (popup ,(bs:pointer '*)))))
               (popups ,%wl-list)
               (added ,bool)
               (configured ,bool)
               (mapped ,bool)
               (configure-idle ,(bs:pointer '*))
               (scheduled-serial ,uint32)
               (configure-list ,%wl-list)
               (current ,%wlr-xdg-surface-state-struct)
               (pending ,%wlr-xdg-surface-state-struct)
               (surface-destroy ,%wl-listener)
               (surface-commit ,%wl-listener)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (ping-timeout ,%wl-signal-struct)
                                     (new-popup ,%wl-signal-struct)
                                     (map ,%wl-signal-struct)
                                     (unmap ,%wl-signal-struct)
                                     (configure ,%wl-signal-struct)
                                     (ack-configure ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define %wlr-xdg-surface-configure-struct
  (bs:struct `((surface ,(bs:pointer %wlr-xdg-surface-struct))
               (link ,%wl-list)
               (serial ,uint32))))

(define %wlr-xdg-toplevel-configure-struct
  (bs:struct `(,@(map (lambda (a) (list a int8))
                      '(maximized fullscreen resizing activated))
               ,@(map (lambda (a) (list a uint32))
                      '(titled width height)))))
(define %wlr-xdg-toplevel-requested-struct
  (bs:struct `(,@(map (lambda (a) (list a int8))
                      '(maximized minimized fullscreen))
               (fullscreen-output ,(bs:pointer '*))
               (fullscreen-output-destroy ,%wl-listener))))

(define %wlr-xdg-positioner-struct
  (bs:struct `((anchor-rect ,%wlr-box-struct)
               (anchor ,int8)
               (gravity ,int8)
               (constraint-adjustment ,int8)
               (size ,(bs:struct `((width ,int32)
                                   (height ,int32))))
               (offset ,(bs:struct `((x ,int32)
                                     (y ,int32)))))))
(define %wlr-xdg-popup-struct
  (bs:struct `((base ,(bs:pointer %wlr-xdg-surface-struct))
               (link ,%wl-list)
               (resource ,(bs:pointer '*))
               (committed ,bool)
               (parent ,(bs:pointer %wlr-surface-struct))
               (seat ,(bs:pointer %wlr-seat-struct))
               (geometry ,%wlr-box-struct)
               (positioner ,%wlr-xdg-positioner-struct)
               (grab-link ,%wl-list))))

(define-wlr-types-class wlr-xdg-popup ()
  (base #:allocation #:bytestructure
        #:getter .base)
  #:descriptor %wlr-xdg-popup-struct)


(define %wlr-xdg-toplevel-state-struct
  (bs:struct `(,@(map (lambda (o) `(,o ,bool))
                      '(maximized fullscreen resizing activated))
               ,@(map (lambda (o) `(,o ,uint32))
                      '(tiled width height max-width max-height min-width min-height)))))
(define %wlr-xdg-toplevel-struct
  (bs:struct `((resource ,(bs:pointer '*))
               (base ,(bs:pointer %wlr-xdg-surface-struct))
               (added ,bool)
               (parent ,(bs:pointer %wlr-xdg-surface-struct))
               (parent-unmap ,%wl-listener)
               (current ,%wlr-xdg-toplevel-state-struct)
               (pending ,%wlr-xdg-toplevel-state-struct)
               (scheduled ,%wlr-xdg-toplevel-configure-struct)
               (requested ,%wlr-xdg-toplevel-requested-struct)
               (title ,cstring-pointer)
               (app-id ,cstring-pointer)
               (events ,(bs:struct
                         (map (lambda (a) (list a %wl-signal-struct))
                              '(request-maximize
                                request-fullscreen
                                request-minimize
                                request-move
                                request-resize
                                request-show-window-menu
                                set-parent
                                set-title
                                set-app-id
                                )))))))

(define-wlr-types-class wlr-xdg-toplevel ()
  #:descriptor %wlr-xdg-toplevel-struct)

(define %wlr-xdg-toplevel-resize-event-struct
  (bs:struct `((surface ,(bs:pointer %wlr-xdg-surface-struct))
               (seat ,(bs:pointer %wlr-seat-client-struct))
               (serial ,uint32)
               (edges ,uint32))))
(define-wlr-types-class wlr-xdg-toplevel-resize-event)

(define %wlr-xdg-toplevel-set-fullscreen-event
  (bs:struct `((surface ,(bs:pointer %wlr-xdg-surface-struct))
               (fullscreen ,bool)
               (output ,(bs:pointer '*)))))
(define-wlr-types-class-public wlr-xdg-toplevel-set-fullscreen-event)

(define-method (.edges (o <wlr-xdg-toplevel-resize-event>))
  (bytestructure-ref
   (pointer->bytestructure (unwrap-wlr-xdg-toplevel-resize-event o) %wlr-xdg-toplevel-resize-event-struct)
   'edges))
(define-wlr-procedure (wlr-xdg-shell-create display)
  ('* "wlr_xdg_shell_create" '(*))
  (wrap-wlr-xdg-shell
   (% (unwrap-wl-display display))))
(define-wlr-types-class wlr-xdg-surface ()
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
  (let ((box (bytestructure->pointer (bytestructure
                                      %wlr-box-struct))))
    (% (unwrap-wlr-xdg-surface surface) box)
    (wrap-wlr-box box)))

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
