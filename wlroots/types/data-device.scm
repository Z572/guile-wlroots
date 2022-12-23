(define-module (wlroots types data-device)
  #:use-module (wayland display)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland listener)
  #:use-module (wayland list)
  #:use-module (bytestructures guile)
  #:autoload (wlroots types seat)
  (%wlr-seat-keyboard-grab-struct
   %wlr-seat-pointer-grab-struct
   %wlr-seat-touch-grab-struct
   %wlr-seat-client-struct
   %wlr-seat-struct)
  #:use-module (wlroots types)
  #:use-module (wlroots types surface)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:export (wrap-wlr-data-source
            unwrap-wlr-data-source
            wrap-wlr-data-device-manager
            unwrap-wlr-data-device-manager
            wlr-data-device-manager-create
            %wlr-drag-struct
            %wlr-data-source-struct
            %wlr-drap-icon-struct))

(eval-when (expand load eval)
  (define %wlr-data-source-struct
    (bs:struct `((impl ,(bs:pointer '*))
                 (mime-types ,%wl-array)
                 (actions ,int32)
                 (accepted ,bool)
                 (current-dnd-action ,int32)
                 (compositor-action ,uint32)
                 (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))
  (define %wlr-drap-icon-struct
    (bs:struct `((drag ,(bs:pointer (delay %wlr-drag-struct)))
                 (surface ,(bs:pointer %wlr-surface-struct))
                 (mapped ,bool)
                 (events ,(bs:struct `((map ,%wl-signal-struct)
                                       (unmap ,%wl-signal-struct)
                                       (destroy ,%wl-signal-struct))))
                 (surface-destroy ,%wl-listener-struct)
                 (data ,(bs:pointer 'void)))))
  (define %wlr-drag-struct
    (bs:struct `((grab-type ,int32)
                 (keyboard-grab ,%wlr-seat-keyboard-grab-struct)
                 (pointer-grab ,%wlr-seat-pointer-grab-struct)
                 (touch-grab ,%wlr-seat-touch-grab-struct)
                 (seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (seat-client ,(bs:pointer (delay %wlr-seat-client-struct)))
                 (focus-client ,(bs:pointer (delay %wlr-seat-client-struct)))
                 (icon ,(bs:pointer %wlr-drap-icon-struct))
                 (focus ,(bs:pointer %wlr-surface-struct))
                 (source ,(bs:pointer %wlr-data-source-struct))
                 (started ,bool)
                 (dropped ,bool)
                 (cancelling ,bool)
                 (grab-touch-id ,int32)
                 (touch-id ,int32)
                 (events ,(bs:struct `((focus ,%wl-signal-struct)
                                       (motion ,%wl-signal-struct)
                                       (drop ,%wl-signal-struct)
                                       (destroy ,%wl-signal-struct))))
                 (source-destroy ,%wl-listener-struct)
                 (seat-client-destroy ,%wl-listener-struct)
                 (icon-destroy ,%wl-listener-struct)
                 (data ,(bs:pointer 'void))))))

(define-wlr-types-class wlr-drag ()
  #:descriptor %wlr-drag-struct)

(define-wlr-types-class wlr-data-source ()
  #:descriptor %wlr-data-source-struct)

(define-wlr-types-class wlr-data-device-manager ())


(define-wlr-procedure (wlr-data-device-manager-create display)
  ('* "wlr_data_device_manager_create" '(*))
  (wrap-wlr-data-device-manager (% (unwrap-wl-display display))))
