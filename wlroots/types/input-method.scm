(define-module (wlroots types input-method)
  #:use-module (wayland server display)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots util box)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types keyboard)
  #:use-module (bytestructure-class)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module (wlroots types)
  #:export (.resource .input-method .link .surface .data
                      .resource
                      .input-method
                      .keyboard
                      .keyboard-keymap
                      .keyboard-repeat-info
                      .global
                      .input-methods
                      .display-destroy
                      .keyboard-destroy))

(define-wlr-types-class wlr-input-method-v2-preedit-string ()
  (text #:accessor .text)
  (cursor-begin #:accessor .cursor-begin)
  (cursor-end #:accessor .cursor-end)
  #:descriptor %wlr-input-method-v2-preedit-string-struct)

(define-wlr-types-class wlr-input-method-v2-delete-surrounding-text ()
  (before-length #:accessor .before-length)
  (after-length #:accessor .after-length)
  #:descriptor %wlr-input-method-v2-delete-surrounding-text-struct)

(define-wlr-types-class wlr-input-method-v2-state ()
  (preedit #:accessor .preedit)
  (commit-text #:accessor .commit-text)
  (delete #:accessor .delete)
  #:descriptor %wlr-input-method-v2-state-struct)

(define-wlr-types-class wlr-input-method-v2 ()
  (resource #:accessor .resource)
  (seat #:accessor .seat)
  (seat-client #:accessor .seat-client)
  (pending #:accessor .pending)
  (current #:accessor .current)
  (active #:accessor .active)
  (client-active #:accessor .client-active)
  (current-serial #:accessor .current-serial)
  (popup-surfaces #:accessor .popup-surfaces)
  (keyboard-grab #:accessor .keyboard-grab)
  (link #:accessor .link)
  (seat-client-destroy #:accessor .seat-client-destroy)
  #:descriptor %wlr-input-method-v2-struct)
(define-wlr-types-class wlr-input-popup-surface-v2 ()
  (resource #:accessor .resource)
  (input-method #:accessor .input-method)
  (link #:accessor .link)
  (surface #:accessor .surface)
  (data #:accessor .data)
  #:descriptor %wlr-input-popup-surface-v2-struct)
(define-wlr-types-class wlr-input-method-keyboard-grab-v2 ()
  (resource #:accessor .resource)
  (input-method #:accessor .input-method)
  (keyboard #:accessor .keyboard)
  (keyboard-keymap #:accessor .keyboard-keymap)
  (keyboard-repeat-info #:accessor .keyboard-repeat-info)
  (keyboard-destroy #:accessor .keyboard-destroy)
  #:descriptor %wlr-input-method-keyboard-grab-v2-struct  )

(define-wlr-types-class wlr-input-method-manager-v2 ()
  (global #:accessor .global)
  (input-methods #:accessor .input-methods)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-input-method-manager-v2-struct)

(define-wlr-procedure (wlr-input-method-manager-v2-create display)
  ('* "wlr_input_method_manager_v2_create" (list '*))
  (wrap-wlr-input-method-manager-v2 (% (unwrap-wl-display display))))
(define-wlr-procedure (wlr-input-method-v2-send-activate input-method)
  (ffi:void "wlr_input_method_v2_send_activate" (list '*))
  (% (unwrap-wlr-input-method-v2 input-method)))
(define-wlr-procedure (wlr-input-method-v2-send-deactivate input-method)
  (ffi:void "wlr_input_method_v2_send_deactivate" (list '*))
  (% (unwrap-wlr-input-method-v2 input-method)))
(define-wlr-procedure (wlr-input-method-v2-send-surrounding-text
                       input-method text cursor anchor)
  (ffi:void
   "wlr_input_method_v2_send_surrounding_text"
   (list '* '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-input-method-v2 input-method)
     (ffi:string->pointer text)
     cursor
     anchor))
(define-wlr-procedure (wlr-input-method-v2-send-content-type
                       input-method hint purpose)
  (ffi:void "wlr_input_method_v2_send_content_type"
            (list '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-input-method-v2 input-method) hint purpose))
(define-wlr-procedure (wlr-input-method-v2-send-text-change-cause
                       input-method cause)
  (ffi:void "wlr_input_method_v2_send_text_change_cause" (list '* ffi:uint32))
  (% (unwrap-wlr-input-method-v2 input-method) cause))
(define-wlr-procedure (wlr-input-method-v2-send-done input-method)
  (ffi:void "wlr_input_method_v2_send_done" (list '*))
  (% (unwrap-wlr-input-method-v2 input-method)))
(define-wlr-procedure (wlr-input-method-v2-send-unavailable input-method)
  (ffi:void "wlr_input_method_v2_send_unavailable" (list '*))
  (% (unwrap-wlr-input-method-v2 input-method)))

(define-wlr-procedure (wlr-input-popup-surface-v2-try-from-wlr-surface surface)
  ('* "wlr_input_popup_surface_v2_try_from_wlr_surface" (list '*))
  (let ((out (% surface)))
    (if (ffi:null-pointer? out)
        #f
        (wrap-wlr-input-popup-surface-v2 out))))

(define-wlr-procedure
  (wlr-input-popup-surface-v2-send-text-input-rectangle popup_surface sbox)
  (ffi:void
   "wlr_input_popup_surface_v2_send_text_input_rectangle"
   (list '* '*))
  (% (unwrap-wlr-input-popup-surface-v2 popup_surface) (unwrap-wlr-box sbox)))
(define-wlr-procedure
  (wlr-input-method-keyboard-grab-v2-send-key keyboard_grab time key state)
  (ffi:void
   "wlr_input_method_keyboard_grab_v2_send_key"
   (list '* ffi:uint32 ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-input-method-keyboard-grab-v2 keyboard_grab) time key state))
(define-wlr-procedure
  (wlr-input-method-keyboard-grab-v2-send-modifiers keyboard_grab modifiers)
  (ffi:void "wlr_input_method_keyboard_grab_v2_send_modifiers" (list '* '*))
  (% (unwrap-wlr-input-method-keyboard-grab-v2 keyboard_grab)
     (unwrap-wlr-keyboard-modifiers modifiers)))
(define-wlr-procedure
  (wlr-input-method-keyboard-grab-v2-set-keyboard keyboard_grab keyboard)
  (ffi:void "wlr_input_method_keyboard_grab_v2_set_keyboard" (list '* '*))
  (% (unwrap-wlr-input-method-keyboard-grab-v2 keyboard_grab)
     (unwrap-wlr-keyboard keyboard)))
(define-wlr-procedure
  (wlr-input-method-keyboard-grab-v2-destroy keyboard_grab)
  (ffi:void "wlr_input_method_keyboard_grab_v2_destroy" (list '*))
  (% (unwrap-wlr-input-method-keyboard-grab-v2 keyboard_grab)))
