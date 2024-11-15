(define-module (wlroots types cursor)
  #:use-module (wayland server display)
  #:use-module (wayland signal)
  #:use-module (wlroots types input-device)
  #:use-module (srfi srfi-26)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types touch)
  #:use-module (wlroots types pointer)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  ;; #:use-module (system foreign)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:autoload (wlroots types xcursor-manager) (unwrap-wlr-xcursor-manager)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.x
            .y))

(define-wlr-types-class wlr-cursor ()
  (events (motion wrap-wlr-pointer-motion-event)
          (motion-absolute wrap-wlr-pointer-motion-absolute-event)
          (button wrap-wlr-pointer-button-event)
          (axis wrap-wlr-pointer-axis-event)
          (frame wrap-wlr-cursor)

          (swipe-begin wrap-wlr-pointer-swipe-begin-event)
          (swipe-update wrap-wlr-pointer-swipe-update-event)
          (swipe-end wrap-wlr-pointer-swipe-end-event)

          (pinch-begin wrap-wlr-pointer-pinch-begin-event)
          (pinch-update wrap-wlr-pointer-pinch-update-event)
          (pinch-end wrap-wlr-pointer-pinch-end-event)

          (touch-up wrap-wlr-touch-up-event)
          (touch-down wrap-wlr-touch-down-event)
          (touch-motion wrap-wlr-touch-motion-event)
          (touch-cancel wrap-wlr-touch-cancel-event)
          ;; touch-frame #f

          (hold-begin wrap-wlr-pointer-hold-begin-event)
          (hold-end wrap-wlr-pointer-hold-end-event)
                                        ; TODO
          ;; tablet-tool-axis
          ;; tablet-tool-proximity
          ;; tablet-tool-tip
          ;; tablet-tool-button
          )
  (x #:accessor .x)
  (y #:accessor .y)
  #:descriptor %wlr-cursor-struct)

(define-wlr-procedure (wlr-cursor-create)
  ('* "wlr_cursor_create" '())
  (wrap-wlr-cursor (%)))

(define-wlr-procedure (wlr-cursor-destroy cursor)
  (ffi:void "wlr_cursor_destroy" '(*))
  (% (unwrap-wlr-cursor cursor)))

(define-wlr-procedure (wlr-cursor-attach-output-layout cursor output-layout)
  (ffi:void "wlr_cursor_attach_output_layout" '(* *))
  (% (unwrap-wlr-cursor cursor)
     (unwrap-wlr-output-layout output-layout)))

(define-wlr-procedure (wlr-cursor-set-xcursor cur manager name)
  (ffi:void "wlr_cursor_set_xcursor" `(* * *))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-xcursor-manager manager)
     (ffi:string->pointer name)))

(define-wlr-procedure (wlr-cursor-set-surface cur surface hostpot-x hostpot-y)
  (ffi:void "wlr_cursor_set_surface" `(* * ,ffi:int32 ,ffi:int32))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-surface surface)
     hostpot-x
     hostpot-y))

(define-wlr-procedure (wlr-cursor-move cur dev delta-x delta-y)
  (ffi:void "wlr_cursor_move" `(* * ,ffi:double ,ffi:double))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-input-device dev)
     delta-x
     delta-y))

(define-wlr-procedure (wlr-cursor-warp cur dev x y)
  (ffi:void "wlr_cursor_warp" `(* * ,ffi:double ,ffi:double))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-input-device dev)
     x
     y))

(define-wlr-procedure (wlr-cursor-absolute-to-layout-coords cur dev x y)
  (ffi:void "wlr_cursor_absolute_to_layout_coords" `(* * ,ffi:double ,ffi:double * *))
  (let ((sx  (bytestructure double))
        (sy (bytestructure double)))
    (% (unwrap-wlr-cursor cur)
       (unwrap-wlr-input-device dev)
       x
       y
       (bytestructure->pointer sx)
       (bytestructure->pointer sy))
    (cons (bytestructure-ref sx)
          (bytestructure-ref sy))))

(define-wlr-procedure (wlr-cursor-warp-absolute cur dev x y)
  (ffi:void "wlr_cursor_warp_absolute" `(* * ,ffi:double ,ffi:double))
  (% (unwrap-wlr-cursor cur)
     (unwrap-wlr-input-device dev)
     x
     y))
(define-wlr-procedure (wlr-cursor-attach-input-device cur dev)
  (ffi:void "wlr_cursor_attach_input_device" '(* *))
  (% (unwrap-wlr-cursor cur) (unwrap-wlr-input-device dev)))

(define-wlr-procedure (wlr-cursor-warp-closest cur dev x y)
  (ffi:void "wlr_cursor_warp_closest" (list '* '* ffi:double ffi:double))
  (% (unwrap-wlr-cursor cur) (unwrap-wlr-input-device dev) x y))
