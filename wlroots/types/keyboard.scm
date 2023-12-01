(define-module (wlroots types keyboard)
  #:use-module (oop goops)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (xkbcommon xkbcommon)
  #:use-module ((bytestructures guile) #:select (bytestructure-ref))
  #:use-module ((system foreign) #:prefix ffi:)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (WLR_LED_NUM_LOCK
            WLR_LED_CAPS_LOCK
            WLR_LED_SCROLL_LOCK

            WLR_MODIFIER_SHIFT
            WLR_MODIFIER_CAPS
            WLR_MODIFIER_CTRL
            WLR_MODIFIER_ALT
            WLR_MODIFIER_MOD2
            WLR_MODIFIER_MOD3
            WLR_MODIFIER_LOGO
            WLR_MODIFIER_MOD5
            wlr-keyboard-set-keymap
            wlr-keyboard-set-repeat-info
            wlr-keyboard-get-modifiers
            .keymap-string
            .keycodes
            .depressed
            .latched
            .locked
            .group
            .modifiers
            .time-msec
            .keycode
            .update-state
            .keymap
            .keymap-size
            .num-keycodes
            .xkb-state
            .data
            .state))

(define-wlr-types-class wlr-keyboard-modifiers ()
  (depressed #:getter .depressed)
  (latched #:getter .latched)
  (locked #:getter .locked)
  (group #:getter .group)
  #:descriptor %wlr-keyboard-modifiers-struct)

(define-wlr-types-class wlr-keyboard ()
  (events (key wrap-wlr-keyboard-key-event)
          (modifiers wrap-wlr-keyboard))
  (base #:accessor .base)
  (keymap-string #:accessor .keymap-string)
  (keymap-size #:accessor .keymap-size)
  (keymap-fd #:accessor .keymap-fd)
  (keymap #:accessor .keymap)
  (xkb-state #:accessor .xkb-state)
  (led-indexes #:accessor .led-indexes)
  (mod-indexes #:accessor .mod-indexes)
  (keycodes #:allocation #:virtual #:getter .keycodes
            #:slot-ref
            (lambda (o)
              (let ((b (bytestructure-ref (get-bytestructure o) 'keycodes)))
                (let loop ((n 0)
                           (value '()))
                  (if (>= n WLR_KEYBOARD_KEYS_CAP)
                      (reverse value)
                      (loop (+ n 1)
                            (cons (bytestructure-ref b n) value))))))
            #:slot-set! (const #f))
  (num-keycodes #:accessor .num-keycodes)
  (modifiers #:accessor .modifiers)
  (data #:accessor .data)
  #:descriptor %wlr-keyboard-struct)

(define-enumeration wlr-keyboard-led->value value->wlr-keyboard-led
  (WLR_LED_NUM_LOCK 1)
  (WLR_LED_CAPS_LOCK 2)
  (WLR_LED_SCROLL_LOCK 4))

(define-enumeration wlr-modifier->value value->wlr-modifier
  (WLR_MODIFIER_SHIFT 1)
  (WLR_MODIFIER_CAPS 2)
  (WLR_MODIFIER_CTRL 4)
  (WLR_MODIFIER_ALT 8)
  (WLR_MODIFIER_MOD2 16)
  (WLR_MODIFIER_MOD3 32)
  (WLR_MODIFIER_LOGO 64)
  (WLR_MODIFIER_MOD5 128))

(define-wlr-types-class wlr-keyboard-key-event ()
  (time-msec #:accessor .time-msec)
  (keycode #:accessor .keycode)
  (update-state #:accessor .update-state)
  (state #:accessor .state)
  #:descriptor %wlr-keyboard-key-event-struct)

(define-wlr-procedure (wlr-keyboard-from-input-device input-device)
  ('* "wlr_keyboard_from_input_device" (list '*))
  (wrap-wlr-keyboard (% (unwrap-wlr-input-device input-device))))

(define-wlr-procedure (wlr-keyboard-set-keymap kb keymap)
  (ffi:int8 "wlr_keyboard_set_keymap" '(* *))
  (not (zero? (% (unwrap-wlr-keyboard kb) (unwrap-xkb-keymap keymap)))))

(define-wlr-procedure (wlr-keyboard-set-repeat-info kb rate delay)
  (ffi:void "wlr_keyboard_set_repeat_info" `(* ,ffi:int32 ,ffi:int32))
  (% (unwrap-wlr-keyboard kb) rate delay))

(define-wlr-procedure (wlr-keyboard-keymaps-match km1 km2)
  (ffi:int8 "wlr_keyboard_keymaps_match" `(* *))
  (not (zero? (% (unwrap-xkb-keymap km1) (unwrap-xkb-keymap km2)))))

(define-wlr-procedure (wlr-keyboard-led-update keyboard leds)
  (ffi:void "wlr_keyboard_led_update" (list '* ffi:uint32))
  (% (unwrap-wlr-keyboard keyboard) leds))

(define-wlr-procedure (wlr-keyboard-get-modifiers keyboard)
  (ffi:uint32 "wlr_keyboard_get_modifiers" '(*))
  (% (unwrap-wlr-keyboard keyboard)))
