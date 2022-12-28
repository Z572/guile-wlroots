(define-module (wlroots types input-device)
  #:use-module (wayland signal)
  #:use-module (wlroots types)
  #:use-module (bytestructures guile)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:re-export (%wlr-input-device-struct)
  #:export (wrap-wlr-input-device
            unwrap-wlr-input-device
            wlr-input-device-name
            WLR_INPUT_DEVICE_KEYBOARD
            WLR_INPUT_DEVICE_POINTER
            WLR_INPUT_DEVICE_TOUCH
            WLR_INPUT_DEVICE_TABLET_TOOL
            WLR_INPUT_DEVICE_TABLET_PAD
            WLR_INPUT_DEVICE_SWITCH
            wlr-input-device-type
            wlr-input-device-type->value
            value->wlr-input-device-type))


(define-enumeration
  value->wlr-input-device-type
  wlr-input-device-type->value
  (WLR_INPUT_DEVICE_KEYBOARD 0)
  (WLR_INPUT_DEVICE_POINTER 1)
  (WLR_INPUT_DEVICE_TOUCH 2)
  (WLR_INPUT_DEVICE_TABLET_TOOL 3)
  (WLR_INPUT_DEVICE_TABLET_PAD 4)
  (WLR_INPUT_DEVICE_SWITCH 5))

(define-wlr-types-class wlr-input-device ()
  #:descriptor %wlr-input-device-struct)

(define (wlr-input-device-name device)
  (bytestructure-ref
   (pointer->bytestructure (unwrap-wlr-input-device device) %wlr-input-device-struct)
   'name))

(define (wlr-input-device-type device)
  (bytestructure-ref
   (pointer->bytestructure
    (unwrap-wlr-input-device device)
    %wlr-input-device-struct)
   'type))
