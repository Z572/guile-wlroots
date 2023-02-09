(define-module (wlroots types seat)
  #:use-module (wayland list)
  #:use-module (wayland display)
  #:use-module (wayland client)
  #:use-module (wayland signal)
  #:use-module (wayland protocol)
  #:use-module (wayland resource)
  #:use-module (wayland listener)
  #:use-module (srfi srfi-26)
  ;; #:use-module (wlroots render renderer)
  ;; #:use-module (wlroots types output-layout)
  #:use-module (wlroots time)
  #:use-module (wlroots types)
  #:use-module (wlroots types data-device)
  #:autoload (wlroots types input-device) (unwrap-wlr-input-device)
  #:use-module (wlroots types keyboard)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
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
  #:export (WLR_POINTER_BUTTONS_CAP
            unwrap-wlr-seat
            unwrap-wlr-seat-client
            unwrap-wlr-seat-pointer-request-set-cursor-event
            unwrap-wlr-seat-request-set-selection-event
            wlr-seat-client-for-wl-client
            wlr-seat-client-for-wl-client
            wlr-seat-client-from-pointer-resource
            wlr-seat-client-from-resource
            wlr-seat-client-next-serial
            wlr-seat-client-validate-event-serial
            wlr-seat-create
            wlr-seat-destroy
            wlr-seat-get-keyboard
            wlr-seat-get-keyboard
            wlr-seat-keyboard-clear-focus
            wlr-seat-keyboard-has-grab
            wlr-seat-keyboard-notify-clear-focus
            wlr-seat-keyboard-notify-enter
            wlr-seat-keyboard-notify-key
            wlr-seat-keyboard-notify-modifiers
            wlr-seat-keyboard-send-key
            wlr-seat-keyboard-send-modifiers
            wlr-seat-keyboard-start-grab
            wlr-seat-pointer-clear-focus
            wlr-seat-pointer-end-grab
            wlr-seat-pointer-enter
            wlr-seat-pointer-has-grab
            wlr-seat-pointer-notify-axis
            wlr-seat-pointer-notify-button
            wlr-seat-pointer-notify-clear-focus
            wlr-seat-pointer-notify-enter
            wlr-seat-pointer-notify-frame
            wlr-seat-pointer-notify-motion
            wlr-seat-pointer-send-axis
            wlr-seat-pointer-send-button
            wlr-seat-pointer-send-frame
            wlr-seat-pointer-send-motion
            wlr-seat-pointer-start-grab
            wlr-seat-pointer-warp
            wlr-seat-set-capabilities
            wlr-seat-set-keyboard
            wlr-seat-set-name
            wlr-seat-set-selection
            wlr-seat-touch-end-grab
            wlr-seat-touch-get-point
            wlr-seat-touch-has-grab
            wlr-seat-touch-notify-cancel
            wlr-seat-touch-notify-down
            wlr-seat-touch-notify-frame
            wlr-seat-touch-notify-motion
            wlr-seat-touch-notify-up
            wlr-seat-touch-num-points
            wlr-seat-touch-point-clear-focus
            wlr-seat-touch-point-focus
            wlr-seat-touch-send-cancel
            wlr-seat-touch-send-down
            wlr-seat-touch-send-frame
            wlr-seat-touch-send-motion
            wlr-seat-touch-send-up
            wlr-seat-touch-start-grab
            wlr-seat-validate-grab-serial
            wlr-seat-validate-pointer-grab-serial
            wlr-seat-validate-touch-grab-serial
            wlr-surface-accepts-touch
            wrap-wlr-seat-client
            wrap-wlr-seat-pointer-request-set-cursor-event
            wrap-wlr-seat-request-set-selection-event
            wrap-wlr-seat
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
            .touch-id
            .touch-state
            .focus-client
            .focus-surface
            .client
            .surface
            .accumulated-capabilities))


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

(define-wlr-types-class wlr-seat-keyboard-grab ()
  (interface #:accessor .interface)
  (seat #:accessor .seat)
  (data #:accessor .data)
  #:descriptor %wlr-seat-keyboard-grab-struct)

(define-wlr-types-class wlr-touch-point ()
  (touch-id #:accessor .touch-id)
  (surface #:accessor .surface)
  (client #:accessor .client)
  (focus-surface #:accessor .focus-surface)
  (focus-client #:accessor .focus-client)
  (sx #:accessor .sx)
  (sy #:accessor .sy)
  (surface-destroy #:accessor .surface-destroy)
  (focus-surface-destroy #:accessor .focus-surface-destroy)
  (client-destroy #:accessor .client-destroy)
  (link #:accessor .link)
  #:descriptor %wlr-touch-point-struct)

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
(define-wlr-types-class wlr-seat-request-set-primary-selection-event ()
  (source #:accessor .source)
  (serial #:accessor .serial)
  #:descriptor %wlr-seat-request-set-primary-selection-event-struct)

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

(define-wlr-types-class wlr-seat-touch-grab ()
  (seat #:accessor .seat)
  (data #:accessor .data)
  #:descriptor %wlr-seat-touch-grab-struct)

(define-wlr-types-class wlr-seat-pointer-grab ()
  (seat #:accessor .seat)
  (data #:accessor .data)
  #:descriptor %wlr-seat-pointer-grab-struct)

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

(define-wlr-procedure (wlr-seat-set-name wlr-seat name)
  (ffi:void "wlr_seat_set_name" (list '* '*))
  (% (unwrap-wlr-seat wlr-seat) (ffi:string->pointer name)))

(define-wlr-procedure
  (wlr-seat-pointer-surface-has-focus wlr-seat surface)
  (ffi:int8 "wlr_seat_pointer_surface_has_focus" (list '* '*))
  (not (zero? (% (unwrap-wlr-seat wlr-seat) (unwrap-wlr-surface surface)))))

(define-wlr-procedure (wlr-seat-pointer-enter wlr-seat surface sx sy)
  (ffi:void "wlr_seat_pointer_enter" (list '* '* ffi:double ffi:double))
  (% (unwrap-wlr-seat wlr-seat) (unwrap-wlr-surface surface) sx sy))

(define-wlr-procedure (wlr-seat-pointer-clear-focus wlr-seat)
  (ffi:void "wlr_seat_pointer_clear_focus" (list '*))
  (% (unwrap-wlr-seat wlr-seat)))

(define-wlr-procedure (wlr-seat-pointer-send-motion wlr-seat time_msec sx sy)
  (ffi:void
   "wlr_seat_pointer_send_motion"
   (list '* ffi:uint32 ffi:double ffi:double))
  (% (unwrap-wlr-seat wlr-seat) time_msec sx sy))

(define-wlr-procedure (wlr-seat-pointer-send-button
                       wlr-seat time_msec button state)
  (ffi:uint32
   "wlr_seat_pointer_send_button"
   (list '* ffi:uint32 ffi:uint32 ffi:int32))
  (% (unwrap-wlr-seat wlr-seat)
     time_msec
     button
     (bs:enum->integer %wlr-button-state-enum state)))

(define-wlr-procedure (wlr-seat-pointer-send-axis
                       wlr-seat time_msec orientation value
                       value_discrete source)
  (ffi:void "wlr_seat_pointer_send_axis" (list '* ffi:uint32
                                               ffi:int32
                                               ffi:double ffi:int32 ffi:int32))
  (% (unwrap-wlr-seat wlr-seat)
     time_msec
     (bs:enum->integer %wlr-axis-orientation-enum orientation)
     value
     value_discrete
     (bs:enum->integer %wlr-axis-source-enum source)))

(define-wlr-procedure (wlr-seat-pointer-send-frame wlr-seat)
  (ffi:void "wlr_seat_pointer_send_frame" (list '*))
  (% (unwrap-wlr-seat wlr-seat)))

(define-wlr-procedure (wlr-seat-pointer-notify-enter seat surface sx sy)
  (ffi:void "wlr_seat_pointer_notify_enter" `(* * ,ffi:double ,ffi:double))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-surface surface) sx sy))

(define-wlr-procedure (wlr-seat-pointer-notify-button seat time_msec button state)
  (ffi:uint32 "wlr_seat_pointer_notify_button" (list '* ffi:uint32 ffi:uint32 ffi:int))
  (% (unwrap-wlr-seat seat)
     time_msec
     button
     (bs:enum->integer %wlr-button-state-enum state)))

(define-wlr-procedure (wlr-seat-pointer-notify-clear-focus seat)
  (ffi:void "wlr_seat_pointer_notify_clear_focus" '(*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-pointer-warp wlr-seat sx sy)
  (ffi:void "wlr_seat_pointer_warp" (list '* ffi:double ffi:double))
  (% (unwrap-wlr-seat wlr-seat) sx sy))

(define-wlr-procedure (wlr-seat-pointer-notify-motion seat time sx sy)
  (ffi:void "wlr_seat_pointer_notify_motion" (list '* ffi:uint32 ffi:double ffi:double))
  (% (unwrap-wlr-seat seat) time sx sy))

(define-wlr-procedure (wlr-seat-set-selection seat source serial)
  (ffi:void "wlr_seat_set_selection" `(* * ,ffi:uint32))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-data-source source) serial))

(define-wlr-procedure (wlr-seat-pointer-notify-axis wlr-seat time-msec orientation value value-discrete source)
  (ffi:void "wlr_seat_pointer_notify_axis" (list '* ffi:uint32 ffi:int ffi:double ffi:int32 ffi:int))
  (% (unwrap-wlr-seat wlr-seat)
     time-msec
     (bs:enum->integer %wlr-axis-orientation-enum orientation)
     value
     value-discrete
     (bs:enum->integer %wlr-axis-source-enum source)))

(define-wlr-procedure (wlr-seat-pointer-notify-frame seat)
  (ffi:void "wlr_seat_pointer_notify_frame" '(*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-pointer-start-grab wlr-seat grab)
  (ffi:void "wlr_seat_pointer_start_grab" (list '* '*))
  (% (unwrap-wlr-seat wlr-seat) (unwrap-wlr-seat-pointer-grab grab)))

(define-wlr-procedure (wlr-seat-pointer-end-grab wlr-seat)
  (ffi:void "wlr_seat_pointer_end_grab" (list '*))
  (% (unwrap-wlr-seat wlr-seat)))

(define-wlr-procedure (wlr-seat-pointer-has-grab seat)
  (ffi:int8 "wlr_seat_pointer_has_grab" (list '*))
  (not (zero? (% (unwrap-wlr-seat seat)))))

(define-wlr-procedure (wlr-seat-set-keyboard seat dev)
  (ffi:void "wlr_seat_set_keyboard" `(* *))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-keyboard dev)))

(define-wlr-procedure (wlr-seat-get-keyboard seat)
  ('* "wlr_seat_get_keyboard" '(*))
  (wrap-wlr-keyboard (% (unwrap-wlr-seat seat))))

(define-wlr-procedure (wlr-seat-keyboard-send-key seat time-msec key state)
  (ffi:void "wlr_seat_keyboard_send_key"
            (list '* ffi:uint32 ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-seat seat) time-msec key state))

(define-wlr-procedure (wlr-seat-keyboard-send-modifiers seat modifiers)
  (ffi:void "wlr_seat_keyboard_send_modifiers" (list '* '*))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-keyboard-modifiers modifiers)))

(define-wlr-procedure
  (wlr-seat-keyboard-enter seat surface keycodes num_keycodes modifiers)
  (ffi:void "wlr_seat_keyboard_enter" (list '* '* '* ffi:size_t '*))
  (% (unwrap-wlr-seat seat)
     (unwrap-wlr-surface surface)
     (if keycodes
         (bytestructure->pointer
          (bytestructure (bs:vector (length keycodes) uint32)
                         (list->vector keycodes)))
         ffi:%null-pointer)
     num_keycodes
     (unwrap-wlr-keyboard-modifiers modifiers)))

(define-wlr-procedure (wlr-seat-keyboard-clear-focus wlr-seat)
  (ffi:void "wlr_seat_keyboard_clear_focus" (list '*))
  (% (unwrap-wlr-seat wlr-seat)))

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

(define-wlr-procedure (wlr-seat-keyboard-notify-clear-focus seat)
  (ffi:void "wlr_seat_keyboard_notify_clear_focus" '(*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-keyboard-start-grab wlr-seat grab)
  (ffi:void "wlr_seat_keyboard_start_grab" (list '* '*))
  (% (unwrap-wlr-seat wlr-seat) (unwrap-wlr-seat-keyboard-grab grab)))

(define-wlr-procedure (wlr-seat-keyboard-end-grab wlr-seat)
  (ffi:void "wlr_seat_keyboard_end_grab" (list '*))
  (% (unwrap-wlr-seat wlr-seat)))

(define-wlr-procedure (wlr-seat-keyboard-has-grab seat)
  (ffi:int8 "wlr_seat_keyboard_has_grab" (list '*))
  (not (zero? (% (unwrap-wlr-seat seat)))))

(define-wlr-procedure (wlr-seat-touch-get-point seat touch_id)
  ('* "wlr_seat_touch_get_point" (list '* ffi:int32))
  (wrap-wlr-touch-point (% (unwrap-wlr-seat seat) touch_id)))

(define-wlr-procedure (wlr-seat-touch-point-focus
                       seat surface time_msec touch_id sx sy)
  (ffi:void "wlr_seat_touch_point_focus"
            (list '* '* ffi:uint32 ffi:int32 ffi:double ffi:double))
  (% (unwrap-wlr-seat seat)
     (unwrap-wlr-surface surface)
     time_msec
     touch_id
     sx
     sy))

(define-wlr-procedure (wlr-seat-touch-point-clear-focus seat time_msec touch_id)
  (ffi:void "wlr_seat_touch_point_clear_focus" (list '* ffi:uint32 ffi:int32))
  (% (unwrap-wlr-seat seat) time_msec touch_id))

(define-wlr-procedure (wlr-seat-touch-send-down seat surface time_msec touch_id sx sy)
  (ffi:uint32 "wlr_seat_touch_send_down"
              (list '* '* ffi:uint32 ffi:int32 ffi:double ffi:double))
  (% (unwrap-wlr-seat seat)
     (unwrap-wlr-surface surface)
     time_msec
     touch_id
     sx
     sy))

;;
(define-wlr-procedure (wlr-seat-touch-send-up seat time_msec touch_id)
  (ffi:void "wlr_seat_touch_send_up" (list '* ffi:uint32 ffi:int32))
  (% (unwrap-wlr-seat seat) time_msec touch_id))

(define-wlr-procedure (wlr-seat-touch-send-motion seat time_msec touch_id sx sy)
  (ffi:void
   "wlr_seat_touch_send_motion"
   (list '* ffi:uint32 ffi:int32 ffi:double ffi:double))
  (% (unwrap-wlr-seat seat) time_msec touch_id sx sy))

(define-wlr-procedure (wlr-seat-touch-send-cancel seat surface)
  (ffi:void "wlr_seat_touch_send_cancel" (list '* '*))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-surface surface)))

(define-wlr-procedure (wlr-seat-touch-send-frame seat)
  (ffi:void "wlr_seat_touch_send_frame" (list '*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-touch-notify-down
                       seat surface time_msec touch_id sx sy)
  (ffi:uint32
   "wlr_seat_touch_notify_down"
   (list '* '* ffi:uint32 ffi:int32 ffi:double ffi:double))
  (% (unwrap-wlr-seat seat)
     (unwrap-wlr-surface surface)
     time_msec
     touch_id
     sx
     sy))

(define-wlr-procedure (wlr-seat-touch-notify-up seat time_msec touch_id)
  (ffi:void "wlr_seat_touch_notify_up" (list '* ffi:uint32 ffi:int32))
  (% (unwrap-wlr-seat seat) time_msec touch_id))

(define-wlr-procedure (wlr-seat-touch-notify-motion
                       seat time_msec touch_id sx sy)
  (ffi:void "wlr_seat_touch_notify_motion"
            (list '* ffi:uint32 ffi:int32 ffi:double ffi:double))
  (% (unwrap-wlr-seat seat) time_msec touch_id sx sy))

(define-wlr-procedure (wlr-seat-touch-notify-cancel seat surface)
  (ffi:void "wlr_seat_touch_notify_cancel" (list '* '*))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-surface surface)))

(define-wlr-procedure (wlr-seat-touch-notify-frame seat)
  (ffi:void "wlr_seat_touch_notify_frame" (list '*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-touch-num-points seat)
  (ffi:int "wlr_seat_touch_num_points" (list '*))
  (% (unwrap-wlr-seat seat)))

(define-wlr-procedure (wlr-seat-touch-start-grab wlr-seat grab)
  (ffi:void "wlr_seat_touch_start_grab" (list '* '*))
  (% (unwrap-wlr-seat wlr-seat) (unwrap-wlr-seat-touch-grab grab)))

(define-wlr-procedure (wlr-seat-touch-end-grab wlr-seat)
  (ffi:void "wlr_seat_touch_end_grab" (list '*))
  (% (unwrap-wlr-seat wlr-seat)))

(define-wlr-procedure (wlr-seat-touch-has-grab seat)
  (ffi:int8 "wlr_seat_touch_has_grab" (list '*))
  (not (zero? (% (unwrap-wlr-seat seat)))))

(define-wlr-procedure (wlr-seat-validate-grab-serial seat serial)
  (ffi:int8 "wlr_seat_validate_grab_serial" (list '* ffi:uint32))
  (not (zero? (% (unwrap-wlr-seat seat) serial))))

;;
(define-wlr-procedure (wlr-seat-validate-pointer-grab-serial seat origin serial)
  (ffi:int8 "wlr_seat_validate_pointer_grab_serial" `(* * ,ffi:uint32))
  (not (zero? (% (unwrap-wlr-seat seat) (unwrap-wlr-surface origin) serial))))

(define-wlr-procedure (wlr-seat-validate-touch-grab-serial
                       seat origin serial point_ptr)
  (ffi:int8 "wlr_seat_validate_touch_grab_serial" (list '* '* ffi:uint32 '*))
  (not (zero? (% (unwrap-wlr-seat seat)
                 (unwrap-wlr-surface origin) serial point_ptr))))

(define-wlr-procedure (wlr-seat-client-next-serial client)
  (ffi:uint32 "wlr_seat_client_next_serial" (list '*))
  (% (unwrap-wlr-seat-client client)))

(define-wlr-procedure (wlr-seat-client-validate-event-serial client serial)
  (ffi:int8 "wlr_seat_client_validate_event_serial" (list '* ffi:uint32))
  (not (zero? (% (unwrap-wlr-seat-client client) serial))))

(define-wlr-procedure (wlr-seat-client-from-resource resource)
  ('* "wlr_seat_client_from_resource" (list '*))
  (wrap-wlr-seat-client (% (unwrap-wl-resource resource))))

(define-wlr-procedure (wlr-seat-client-from-pointer-resource resource)
  ('* "wlr_seat_client_from_pointer_resource" (list '*))
  (wrap-wlr-seat-client (% (unwrap-wl-resource resource))))

(define-wlr-procedure (wlr-surface-accepts-touch wlr-seat surface)
  (ffi:int8 "wlr_surface_accepts_touch" (list '* '*))
  (not (zero? (% (unwrap-wlr-seat wlr-seat) (unwrap-wlr-surface surface)))))


;;
