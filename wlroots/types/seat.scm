(define-module (wlroots types seat)
  #:use-module (wayland list)
  #:use-module (wayland display)
  #:use-module (wayland signal)
  #:use-module (wayland listener)
  #:use-module (srfi srfi-26)
  ;; #:use-module (wlroots render renderer)
  ;; #:use-module (wlroots types output-layout)
  #:use-module (wlroots time)
  #:use-module (wlroots types)
  #:autoload (wlroots types data-device)
  (unwrap-wlr-data-source %wlr-drag-struct %wlr-data-source-struct)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types surface)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select ((uint32 . ffi:uint32)
                                           (int32 . ffi:int32)
                                           (float . ffi:float)
                                           (int . ffi:int)
                                           (void . ffi:void)
                                           (double . ffi:double)
                                           pointer-address
                                           %null-pointer
                                           string->pointer))
  #:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (wrap-wlr-seat
            unwrap-wlr-seat
            wlr-seat-create
            wlr-seat-pointer-notify-frame
            WLR_POINTER_BUTTONS_CAP
            %wlr-seat-request-set-selection-event-struct
            wrap-wlr-seat-request-set-selection-event
            unwrap-wlr-seat-request-set-selection-event
            %wlr-seat-request-set-cursor-event-struct
            %wlr-seat-keyboard-grab-struct
            %wlr-seat-pointer-grab-struct
            %wlr-seat-touch-grab-struct
            wrap-wlr-seat-pointer-request-set-cursor-event
            unwrap-wlr-seat-pointer-request-set-cursor-event
            wlr-seat-set-selection
            wlr-seat-set-keyboard
            %wlr-seat-struct
            %wlr-seat-client-struct
            wrap-wlr-seat-client
            unwrap-wlr-seat-client
            wlr-seat-pointer-notify-axis
            wlr-seat-set-capabilities
            wlr-seat-keyboard-notify-key
            wlr-seat-keyboard-send-key
            .serial
            .selection-serial
            .pointer-state
            .seat
            .focused-client
            .focused-surface
            .sx
            .sy
            .capabilities
            .selection-offers
            .last-event
            .display
            .drag
            .name
            .accumulated-capabilities
            .primary-selection-source
            .primary-selection-serial
            .keyboard-state
            .touch-state
            .drag-serial
            .grab-serial
            .grab
            .default-grab
            .needs-touch-frame
            .data-devices
            .end
            .buttons
            .count
            .source))

(eval-when (expand load eval)
  (define WLR_SERIAL_RINGSET_SIZE 128)
  (define %wlr-serial-range-struct
    (bs:struct `((min-incl ,uint32)
                 (max-incl ,uint32))))
  (define %wlr-serial-ringset-struct
    (bs:struct `((data ,(bs:vector WLR_SERIAL_RINGSET_SIZE %wlr-serial-range-struct))
                 (end ,int)
                 (count ,int))))
  (define %wlr-seat-client-struct
    (bs:struct `((client ,(bs:pointer '*))
                 (seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (link ,%wl-list-struct)

                 (resources ,%wl-list-struct)
                 (pointers ,%wl-list-struct)
                 (keyboards ,%wl-list-struct)
                 (touches ,%wl-list-struct)
                 (data-devices ,%wl-list-struct)

                 (events ,(bs:struct `((destroy ,%wl-signal-struct))))
                 (serials ,%wlr-serial-ringset-struct)
                 (needs-touch-frame ,bool))))

  (define WLR_POINTER_BUTTONS_CAP 16)

  (define %wlr-seat-pointer-state-struct
    (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (focused-client ,(bs:pointer %wlr-seat-client-struct))
                 (focused-surface ,(bs:pointer %wlr-surface-struct))
                 (sx ,double)
                 (sy ,double)
                 (grab ,(bs:pointer (delay %wlr-seat-pointer-grab-struct)))
                 (default-grab ,(bs:pointer (delay %wlr-seat-pointer-grab-struct)))
                 (sent-axis-source ,bool)
                 (cached-axis-source ,int32)
                 (buttons ,(bs:vector WLR_POINTER_BUTTONS_CAP uint32))
                 (button-count ,size_t)
                 (grab-button ,uint32)
                 (grab-serial ,uint32)
                 (grab-time ,uint32)
                 (surface-destroy ,%wl-listener-struct)
                 (events ,(bs:struct `((focus-change ,%wl-signal-struct)))))))
  (define %wlr-seat-keyboard-state-struct
    (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (keyboard ,(bs:pointer '*))
                 (focused-client ,(bs:pointer %wlr-seat-client-struct))
                 (focused-surface ,(bs:pointer %wlr-surface-struct))
                 (keyboard-destroy ,%wl-listener-struct)
                 (keyboard-keymap ,%wl-listener-struct)
                 (keyboard-repeat-info ,%wl-listener-struct)
                 (surface-destroy ,%wl-listener-struct)
                 (grab ,(bs:pointer (delay %wlr-seat-keyboard-grab-struct)))
                 (default-grab ,(bs:pointer (delay %wlr-seat-keyboard-grab-struct)))
                 (events ,(bs:struct `((focus-change ,%wl-signal-struct)))))))
  (define %wlr-seat-touch-state-struct
    (bs:struct `((seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (touch-points ,%wl-list-struct)
                 (grab-serial ,uint32)
                 (grab-id ,uint32)
                 (grab ,(bs:pointer '*))
                 (default-grab ,(bs:pointer '*)))))
  (define %wlr-seat-keyboard-grab-struct
    (bs:struct `((interface ,(bs:pointer '*))
                 (seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (data ,(bs:pointer 'void)))))
  (define %wlr-seat-pointer-grab-struct
    (bs:struct `((interface ,(bs:pointer '*))
                 (seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (data ,(bs:pointer 'void)))))
  (define %wlr-seat-touch-grab-struct
    (bs:struct `((interface ,(bs:pointer '*))
                 (seat ,(bs:pointer (delay %wlr-seat-struct)))
                 (data ,(bs:pointer 'void)))))
  (define %wlr-seat-request-set-cursor-event-struct
    (bs:struct `((seat-client ,(bs:pointer %wlr-seat-client-struct))
                 (surface ,(bs:pointer %wlr-surface-struct))
                 (serial ,uint32)
                 (hostpot-x ,int32)
                 (hostpot-y ,int32))))
  (define %wlr-seat-struct
    (bs:struct `((global ,(bs:pointer '*))
                 (display ,(bs:pointer '*))
                 (clients ,%wl-list-struct)
                 (name ,cstring-pointer)
                 (capabilities ,uint32)
                 (accumulated-capabilities ,uint32)
                 (last-event ,%timespec-struct)
                 (selection-source ,(bs:pointer (delay %wlr-data-source-struct)))
                 (selection-serial ,uint32)
                 (selection-offers ,%wl-list-struct)
                 (primary-selection-source ,(bs:pointer '*))
                 (primary-selection-serial ,uint32)

                 (drag ,(bs:pointer (delay %wlr-drag-struct)))
                 (drag-source ,(bs:pointer (delay %wlr-data-source-struct)))
                 (drag-serial ,uint32)
                 (drag-offers ,%wl-list-struct)

                 (pointer-state ,%wlr-seat-pointer-state-struct)
                 (keyboard-state ,%wlr-seat-keyboard-state-struct)
                 (touch-state ,%wlr-seat-touch-state-struct)

                 (display-destroy ,%wl-listener-struct)
                 (selection-source-destroy ,%wl-listener-struct)
                 (primary-selection-source-destroy ,%wl-listener-struct)
                 (drag-source-destroy ,%wl-listener-struct)

                 (events ,(bs:struct (map (cut cons <> (list %wl-signal-struct))
                                          '(pointer-grab-begin
                                            pointer-grab-end
                                            keyboard-grab-begin
                                            keyboard-grab-end
                                            touch-grab-begin
                                            touch-grab-end
                                            request-set-cursor
                                            request-set-selection
                                            set-selection
                                            request-set-primary-selection
                                            set-primary-selection
                                            request-start-drag
                                            start-drag
                                            destroy))))
                 (data ,(bs:pointer 'void)))))

  (define %wlr-seat-request-set-selection-event-struct
    (bs:struct `((source ,(bs:pointer (delay %wlr-data-source-struct)))
                 (serial ,uint32)))))

(define-wlr-types-class wlr-seat-touch-state ()
  (seat #:allocation #:bytestructure #:accessor .seat)
  (grab-serial #:allocation #:bytestructure #:accessor .grab-serial)
  (grab-id #:allocation #:bytestructure #:accessor .grab-id)
  (grab #:allocation #:bytestructure #:accessor .grab)
  (default-grab #:allocation #:bytestructure #:accessor .default-grab)
  #:descriptor %wlr-seat-touch-state-struct)

(define-wlr-types-class wlr-serial-ringset ()
  (end #:allocation #:bytestructure #:accessor .end)
  (count #:allocation #:bytestructure #:accessor .count)
  #:descriptor %wlr-serial-ringset-struct)
(define-wlr-types-class wlr-seat-client ()
  (seat #:allocation #:bytestructure #:getter .seat)
  (data-devices #:allocation #:bytestructure #:accessor .data-devices)
  (serials #:allocation #:bytestructure #:accessor .serials)
  (needs-touch-frame #:allocation #:bytestructure #:accessor .needs-touch-frame)
  #:descriptor %wlr-seat-client-struct)

(define-wlr-types-class wlr-seat-request-set-selection-event ()
  (source #:accessor .source #:allocation #:bytestructure)
  (serial #:accessor .serial #:allocation #:bytestructure)
  #:descriptor %wlr-seat-request-set-selection-event-struct)

(define-wlr-types-class wlr-seat-pointer-request-set-cursor-event ())



(define-wlr-types-class-public wlr-seat-pointer-state ()
  (seat #:allocation #:bytestructure #:accessor .seat)
  (focused-client #:allocation #:bytestructure #:accessor .focused-client)
  (focused-surface #:allocation #:bytestructure #:accessor .focused-surface)
  (sx #:allocation #:bytestructure #:getter .sx)
  (sy #:allocation #:bytestructure #:getter .sy)
  (buttons #:allocation #:bytestructure #:accessor .buttons)
  #:descriptor %wlr-seat-pointer-state-struct)

(define-wlr-types-class wlr-seat-keyboard-state ()
  (seat #:allocation #:bytestructure #:accessor .seat)
  (focused-client #:allocation #:bytestructure #:accessor .focused-client)
  (focused-surface #:allocation #:bytestructure #:accessor .focused-surface)
  (grab #:allocation #:bytestructure #:accessor .grab)
  (default-grab #:allocation #:bytestructure #:accessor .default-grab)
  #:descriptor %wlr-seat-keyboard-state-struct)

(define-wlr-types-class wlr-seat ()
  (display #:allocation #:bytestructure #:getter .display)
  (name #:allocation #:bytestructure #:getter .name)
  (capabilities #:allocation #:bytestructure #:getter .capabilities)
  (accumulated-capabilities #:allocation #:bytestructure #:getter .accumulated-capabilities)
  (last-event #:allocation #:bytestructure #:getter .last-event)

  (selection-source #:allocation #:bytestructure #:getter .selection-source)
  (selection-serial #:allocation #:bytestructure #:getter .selection-serial)
  (selection-offers #:allocation #:bytestructure #:getter .selection-offers)

  (primary-selection-source #:allocation #:bytestructure #:getter .primary-selection-source)
  (primary-selection-serial #:allocation #:bytestructure #:getter .primary-selection-serial)

  (drag #:allocation #:bytestructure #:getter .drag)
  (drag-serial #:allocation #:bytestructure #:getter .drag-serial)

  (pointer-state #:allocation #:bytestructure #:getter .pointer-state)
  (keyboard-state #:allocation #:bytestructure #:getter .keyboard-state)
  (touch-state #:allocation #:bytestructure #:getter .touch-state)

  #:descriptor %wlr-seat-struct)

(define-wlr-procedure (wlr-seat-create display name)
  ('* "wlr_seat_create" '(* *))
  (wrap-wlr-seat
   (% (unwrap-wl-display display)
      (string->pointer name ))))
(define-wlr-procedure (wlr-seat-pointer-notify-frame seat)
  (ffi:void "wlr_seat_pointer_notify_frame" '(*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-set-selection seat source serial)
  (ffi:void "wlr_seat_set_selection" `(* * ,ffi:uint32))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-data-source source) serial))

(define-wlr-procedure (wlr-seat-set-keyboard seat dev)
  (ffi:void "wlr_seat_set_keyboard" `(* *))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-input-device dev)))

(define-wlr-procedure (wlr-seat-pointer-notify-axis wlr-seat time-msec orientation value value-discrete source)
  (ffi:void "wlr_seat_pointer_notify_axis" (list '* ffi:uint32 ffi:int ffi:double ffi:int32 ffi:int))
  (% (unwrap-wlr-seat wlr-seat) time-msec orientation value value-discrete source))

(define-wlr-procedure (wlr-seat-set-capabilities seat capabilities)
  (ffi:void "wlr_seat_set_capabilities" `(* ,ffi:uint32))
  (% (unwrap-wlr-seat seat) capabilities))

(define-wlr-procedure (wlr-seat-keyboard-notify-key seat time-msec key state)
  (ffi:void "wlr_seat_keyboard_notify_key"
            (list '* ffi:uint32 ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-seat seat) time-msec key state))

(define-wlr-procedure (wlr-seat-keyboard-send-key seat time-msec key state)
  (ffi:void "wlr_seat_keyboard_send_key"
            (list '* ffi:uint32 ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-seat seat) time-msec key state))
