(define-module (wlroots types layer-shell)
  #:use-module (wlroots types surface)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland list)
  #:use-module (wayland display)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:export (%wlr-layer-shell-v1-struct
            wrap-wlr-layer-shell
            unwrap-wlr-layer-shell
            wrap-wlr-layer-surface-v1
            unwrap-wlr-layer-surface-v1
            wlr-layer-shell-v1-create
            wlr-layer-surface-v1-from-wlr-surface))

(define %wlr-layer-shell-v1-struct
  (bs:struct `((global ,(bs:pointer '*))
               (display-destroy ,%wl-listener)
               (events ,(bs:struct `((new-surface ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))
(define %wlr-layer-surface-v1-status-struct
  (bs:struct `((committed ,uint32)
               (anchor ,uint32)
               (exclusive-zone ,int32)
               (margin ,(bs:struct `((top ,uint32)
                                     (right ,uint32)
                                     (bottom ,uint32)
                                     (left ,uint32))))
               (keyboard-interactive ,int)
               (layer ,int)
               (configure-serial ,uint32)
               (actual-width ,uint32)
               (actual-height ,uint32))))
(define %wlr-layer-surface-v1-struct
  (bs:struct `((surface ,(bs:pointer %wlr-surface-struct))
               (output ,(bs:pointer '*))
               (resource ,(bs:pointer '*))
               (shell ,(bs:pointer %wlr-layer-shell-v1-struct))
               (popups ,%wl-list)
               (namespace ,cstring-pointer)
               (added ,int8)
               (configured ,int8)
               (mapped ,int8)
               (configure-list ,%wl-list)
               (current ,%wlr-layer-surface-v1-status-struct)
               (pending ,%wlr-layer-surface-v1-status-struct)
               (surface-destroy ,%wl-listener)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (map ,%wl-signal-struct)
                                     (unmap ,%wl-signal-struct)
                                     (new-popup ,%wl-signal-struct))))
               (data ,(bs:pointer 'void)))))

(define-wlr-types-class wlr-layer-shell)
(define-wlr-types-class wlr-layer-surface-v1)
(define-wlr-procedure (wlr-layer-shell-v1-create display)
  ('* "wlr_layer_shell_v1_create" '(*))
  (wrap-wlr-layer-shell (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-layer-surface-v1-from-wlr-surface surface)
  ('* "wlr_layer_surface_v1_from_wlr_surface" '(*))
  (wrap-wlr-layer-surface-v1 (% (unwrap-wlr-surface surface))))

(define-method (get-event-signal (b <wlr-layer-shell>) (signal-name <symbol>))
  (wrap-wl-signal (bytestructure+offset->pointer
                   (bytestructure-ref (bytestructure-ref
                                       (pointer->bytestructure
                                        (unwrap-wlr-layer-shell b)
                                        %wlr-layer-shell-v1-struct)
                                       'events)  signal-name))))
