(define-module (wlroots types output-layout)
  #:use-module (wayland display)
  #:use-module (wlroots types output)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module (wlroots types)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wayland signal)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:re-export (%wlr-output-layout-struct)
  #:export (wrap-wlr-output-layout
            unwrap-wlr-output-layout
            wlr-output-layout-create
            wlr-direction->value value->wlr-direction
            wlr-output-layout-output-at
            wlr-output-layout-get-box
            wlr-output-layout-remove))

(define-wlr-types-class wlr-output-layout ()
  #:descriptor %wlr-output-layout-struct)

(define-enumeration wlr-direction->value value->wlr-direction
  (WLR_DIRECTION_UP 1)
  (WLR_DIRECTION_DOWN 2)
  (WLR_DIRECTION_LEFT 4)
  (WLR_DIRECTION_RIGHT 8))

(define-wlr-procedure (wlr-output-layout-create)
  ('* "wlr_output_layout_create" '())
  (wrap-wlr-output-layout (%)))

(define-wlr-procedure (wlr-output-layout-get-box layout #:optional (reference #f))
  ('* "wlr_output_layout_get_box" '(* *))
  (wrap-wlr-box (% (unwrap-wlr-output-layout layout)
                   (if reference (unwrap-wlr-output reference)
                       ffi:%null-pointer))))

(define-wlr-procedure (wlr-output-layout-output-at layout lx ly)
  ('* "wlr_output_layout_output_at" (list '* ffi:double ffi:double))
  (let ((o (% (unwrap-wlr-output-layout layout) lx ly)))
    (if (ffi:null-pointer? o)
        #f
        (wrap-wlr-output o))))

(define-wlr-procedure (wlr-output-layout-remove layout output)
  (ffi:void "wlr_output_layout_remove" '(* *))
  (% (unwrap-wlr-output-layout layout) (unwrap-wlr-output output)))
