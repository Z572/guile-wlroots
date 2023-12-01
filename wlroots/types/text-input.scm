(define-module (wlroots types text-input)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types)
  #:use-module (wlroots types compositor)
  #:use-module (wayland server display)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:duplicates
  (merge-accessors merge-generics replace warn-override-core warn last)
  #:export
  (.active-features
   .content-type
   .current
   .current-enabled
   .current-serial
   .cursor-rectangle
   .display-destroy
   .features
   .focused-surface
   .global
   .link
   .pending
   .pending-enabled
   .resource
   .seat-destroy
   .surface-destroy
   .surrounding
   .text-change-cause
   .text-inputs
   .seat))

(define-wlr-types-class wlr-text-input-v3-state ()
  (surrounding #:accessor .surrounding)
  (text-change-cause #:accessor .text-change-cause)
  (content-type #:accessor .content-type)
  (cursor-rectangle #:accessor .cursor-rectangle)
  (features #:accessor .features)
  #:descriptor %wlr-text-input-v3-state-struct)

(define-wlr-types-class wlr-text-input-v3 ()
  (events (enable wrap-wlr-text-input-v3)
          (commit wrap-wlr-text-input-v3)
          (disable wrap-wlr-text-input-v3)
          (destroy wrap-wlr-text-input-v3))
  (seat #:accessor .seat)
  (resource #:accessor .resource)
  (focused-surface #:accessor .focused-surface)
  (pending #:accessor .pending)
  (current #:accessor .current)
  (current-serial #:accessor .current-serial)
  (pending-enabled #:accessor .pending-enabled)
  (current-enabled #:accessor .current-enabled)
  (active-features #:accessor .active-features)
  (link #:accessor .link)
  (surface-destroy #:accessor .surface-destroy)
  (seat-destroy #:accessor .seat-destroy)
  #:descriptor %wlr-text-input-v3-struct)

(define-wlr-types-class wlr-text-input-manager-v3 ()
  (events (text-input wrap-wlr-text-input-v3)
          (destroy wrap-wlr-text-input-manager-v3))
  (global #:accessor .global)
  (text-inputs #:accessor .text-inputs)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-text-input-manager-v3-struct)

(define-wlr-procedure (wlr-text-input-manager-v3-create wl-display)
  ('* "wlr_text_input_manager_v3_create" (list '*))
  (wrap-wlr-text-input-manager-v3 (% (unwrap-wl-display wl-display))))

(define-wlr-procedure (wlr-text-input-v3-send-enter text-input wlr-surface)
  (ffi:void "wlr_text_input_v3_send_enter" (list '* '*))
  (% (unwrap-wlr-text-input-v3 text-input) (unwrap-wlr-surface wlr-surface)))

(define-wlr-procedure (wlr-text-input-v3-send-leave text-input)
  (ffi:void "wlr_text_input_v3_send_leave" (list '*))
  (% (unwrap-wlr-text-input-v3 text-input)))

(define-wlr-procedure (wlr-text-input-v3-send-preedit-string
                       text-input text cursor-begin cursor-end)
  (ffi:void "wlr_text_input_v3_send_preedit_string"
            (list '* '* ffi:int32 ffi:int32))
  (% (unwrap-wlr-text-input-v3 text-input)
     (ffi:string->pointer text) cursor-begin cursor-end))

(define-wlr-procedure (wlr-text-input-v3-send-commit-string text-input text)
  (ffi:void "wlr_text_input_v3_send_commit_string" (list '* '*))
  (% (unwrap-wlr-text-input-v3 text-input) (ffi:string->pointer text)))

(define-wlr-procedure (wlr-text-input-v3-send-delete-surrounding-text
                       text-input before-length after-length)
  (ffi:void "wlr_text_input_v3_send_delete_surrounding_text"
            (list '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-text-input-v3 text-input) before-length after-length))

(define-wlr-procedure (wlr-text-input-v3-send-done text-input)
  (ffi:void "wlr_text_input_v3_send_done" (list '*))
  (% (unwrap-wlr-text-input-v3 text-input)))
