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
  #:use-module (wlroots types data-device)
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
  #:re-export (%wlr-seat-keyboard-grab-struct
               %wlr-seat-pointer-grab-struct
               %wlr-seat-touch-grab-struct
               %wlr-seat-client-struct
               %wlr-seat-struct
               %wlr-seat-request-set-selection-event-struct
               %wlr-seat-struct
               %wlr-seat-client-struct)
  #:export (wrap-wlr-seat
            unwrap-wlr-seat
            wlr-seat-create
            wlr-seat-pointer-notify-frame
            WLR_POINTER_BUTTONS_CAP

            wrap-wlr-seat-request-set-selection-event
            unwrap-wlr-seat-request-set-selection-event
            wrap-wlr-seat-pointer-request-set-cursor-event
            unwrap-wlr-seat-pointer-request-set-cursor-event
            wlr-seat-set-selection
            wlr-seat-set-keyboard
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

(define-wlr-types-class wlr-seat-pointer-request-set-cursor-event ()
  #:descriptor %wlr-seat-pointer-request-set-cursor-event-struct)



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
