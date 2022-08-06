(define-module (wlroots types keyboard)
  #:use-module (wlroots utils))

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
