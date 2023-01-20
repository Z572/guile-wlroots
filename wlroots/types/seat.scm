(define-module (wlroots types seat)
  #:use-module (wayland list)
  #:use-module (wayland display)
  #:use-module (wayland client)
  #:use-module (wayland signal)
  #:use-module (wayland protocol)
  #:use-module (wayland listener)
  #:use-module (srfi srfi-26)
  ;; #:use-module (wlroots render renderer)
  ;; #:use-module (wlroots types output-layout)
  #:use-module (wlroots time)
  #:use-module (wlroots types)
  #:use-module (wlroots types data-device)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types keyboard)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module ((bytestructure-class) #:select (bs:enum->integer))
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
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
            wlr-seat-destroy
            wlr-seat-client-for-wl-client
            wlr-seat-set-capabilities
            wlr-seat-pointer-notify-button
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
            wlr-seat-keyboard-notify-key
            wlr-seat-keyboard-notify-modifiers
            wlr-seat-keyboard-notify-enter
            wlr-seat-keyboard-send-key
            wlr-seat-validate-pointer-grab-serial
            .accumulated-capabilities
            .buttons
            .capabilities
            .count
            .data-devices
            .default-grab
            .display
            .drag
            .drag-serial
            .end
            .focused-client
            .focused-surface
            .grab
            .grab-serial
            .hostpot-x
            .hostpot-y
            .keyboard-state
            .last-event
            .name
            .needs-touch-frame
            .new-surface
            .old-surface
            .origin
            .pointer-state
            .primary-selection-serial
            .primary-selection-source
            .seat
            .seat-client
            .selection-offers
            .selection-serial
            .serial
            .source
            .surface
            .sx
            .sy
            .touch-state))


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
  (seat-client #:accessor .seat-client)
  (surface #:accessor .surface)
  (serial #:accessor .serial)
  (hostpot-x #:accessor .hostpot-x)
  (hostpot-y #:accessor .hostpot-y)
  #:descriptor %wlr-seat-pointer-request-set-cursor-event-struct)


(define-wlr-types-class wlr-seat-request-start-drag-event ()
  (drag #:accessor .drag)
  (origin #:accessor .origin)
  (serial #:accessor .serial)
  #:descriptor %wlr-seat-request-start-drag-event-struct)

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

(define-wlr-types-class wlr-seat-pointer-focus-change-event ()
  (seat #:accessor .seat)
  (old-surface #:accessor .old-surface)
  (new-surface #:accessor .new-surface)
  (sx #:accessor .sx)
  (sy #:accessor .sy)
  #:descriptor %wlr-seat-pointer-focus-change-event-struct)

(define-wlr-types-class wlr-seat-keyboard-focus-change-event ()
  (seat #:accessor .seat)
  (old-surface #:accessor .old-surface)
  (new-surface #:accessor .new-surface)
  #:descriptor %wlr-seat-keyboard-focus-change-event-struct)

(define-wlr-procedure (wlr-seat-create display name)
  ('* "wlr_seat_create" '(* *))
  (wrap-wlr-seat
   (% (unwrap-wl-display display)
      (ffi:string->pointer name))))

(define-wlr-procedure (wlr-seat-destroy seat)
  (ffi:void "wlr_seat_destroy" '(*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-client-for-wl-client seat wl-client)
  ('* "wlr_seat_client_for_wl_client" '(* *))
  (wrap-wlr-seat-client (% (unwrap-wlr-seat (unwrap-wl-client wl-client)))))

(define-wlr-procedure (wlr-seat-set-capabilities seat capabilities)
  (ffi:void "wlr_seat_set_capabilities" (list '* ffi:uint32))
  (let ((cap (if (list? capabilities)
                 (apply logior (map (cut bs:enum->integer
                                         %wl-seat-capability-enum <>)
                                    capabilities))
                 capabilities)))

    (% (unwrap-wlr-seat seat) cap)))

(define-wlr-procedure (wlr-seat-pointer-notify-button seat time_msec button state)
  (ffi:uint32 "wlr_seat_pointer_notify_button" (list '* ffi:uint32 ffi:uint32 ffi:int))
  (% (unwrap-wlr-seat seat)
     time_msec
     button
     (bs:enum->integer %wlr-button-state-enum state)))

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

(define-wlr-procedure (wlr-seat-keyboard-notify-key seat time-msec key state)
  (ffi:void "wlr_seat_keyboard_notify_key"
            (list '* ffi:uint32 ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-seat seat) time-msec key state))

(define-wlr-procedure (wlr-seat-keyboard-notify-modifiers seat modifiers)
  (ffi:void "wlr_seat_keyboard_notify_modifiers" '(* *))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-keyboard-modifiers modifiers)))

(define-wlr-procedure (wlr-seat-keyboard-notify-enter
                       seat surface keycodes num-keycodes modifiers)
  (ffi:void "wlr_seat_keyboard_notify_enter" `(* * * ,ffi:size_t *))
  (% (unwrap-wlr-seat seat)
     (unwrap-wlr-surface surface)
     (if keycodes
         (bytestructure->pointer
          (bytestructure (bs:vector (length keycodes) uint32)
                         (list->vector keycodes)))
         ffi:%null-pointer)
     num-keycodes
     (unwrap-wlr-keyboard-modifiers modifiers)))

(define-wlr-procedure (wlr-seat-keyboard-send-key seat time-msec key state)
  (ffi:void "wlr_seat_keyboard_send_key"
            (list '* ffi:uint32 ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-seat seat) time-msec key state))
(define-wlr-procedure (wlr-seat-validate-pointer-grab-serial seat origin serial)
  (ffi:int8 "wlr_seat_validate_pointer_grab_serial" `(* * ,ffi:uint32))
  (not (zero? (% (unwrap-wlr-seat seat) (unwrap-wlr-surface origin) serial))))
