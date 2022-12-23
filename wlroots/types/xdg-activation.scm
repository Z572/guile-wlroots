(define-module (wlroots types xdg-activation)
  #:use-module (oop goops)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland display)
  #:use-module (wayland util)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:export (wlr-xdg-activation-v1-create
            <wlr-xdg-activation-v1>
            wrap-wlr-xdg-activation-v1
            unwrap-wlr-xdg-activation-v1
            %wlr-xdg-activation-v1-struct))

(define %wlr-xdg-activation-v1-struct
  (bs:struct `((token-timeout-msec ,uint32)
               (tokens ,%wl-list-struct)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)
                                     (request-activate ,%wl-signal-struct))))
               (display ,(bs:pointer '*))
               (global ,(bs:pointer '*))
               (display-destroy ,%wl-listener-struct))))


(define-wlr-types-class-public wlr-xdg-activation-v1 ()
  #:descriptor %wlr-xdg-activation-v1-struct)

(define-wlr-procedure (wlr-xdg-activation-v1-create display)
  ('* "wlr_xdg_activation_v1_create" '(*))
  (wrap-wlr-xdg-activation-v1 (% (unwrap-wl-display display)) ))
