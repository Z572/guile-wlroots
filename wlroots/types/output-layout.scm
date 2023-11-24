(define-module (wlroots types output-layout)
  #:use-module (wayland server display)
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
  #:use-module (bytestructure-class)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.data
            .outputs
            .output
            .x
            .y
            .link
            .addon
            wrap-wlr-output-layout
            unwrap-wlr-output-layout
            wlr-output-layout-create
            wlr-direction->value value->wlr-direction
            wlr-output-layout-output-at
            wlr-output-layout-get-box
            wlr-output-layout-remove
            wlr-output-layout-add-auto
            wlr-output-layout-adjacent-output
            wlr-output-layout-farthest-output))

(define-wlr-types-class wlr-output-layout ()
  (outputs #:accessor .outputs)
  (data #:accessor .data)
  #:descriptor %wlr-output-layout-struct)

(define-wlr-types-class wlr-output-layout-output ()
  (output #:accessor .output)
  (x #:accessor .x)
  (y #:accessor .y)
  (link #:accessor .link)
  (addon #:accessor .addon)
  #:descriptor %wlr-output-layout-output-struct)

(define-enumeration wlr-direction->value value->wlr-direction
  (WLR_DIRECTION_UP 1)
  (WLR_DIRECTION_DOWN 2)
  (WLR_DIRECTION_LEFT 4)
  (WLR_DIRECTION_RIGHT 8))

(define-wlr-procedure (wlr-output-layout-create)
  ('* "wlr_output_layout_create" '())
  (wrap-wlr-output-layout (%)))

(define-wlr-procedure (wlr-output-layout-destroy layout)
  (ffi:void "wlr_output_layout_destroy" (list '*))
  (% (unwrap-wlr-output-layout layout)))

(define-wlr-procedure (wlr-output-layout-get layout reference)
  ('* "wlr_output_layout_get" (list '* '*))
  (wrap-wlr-output-layout-output
   (% (unwrap-wlr-output-layout layout) reference)))

(define-wlr-procedure (wlr-output-layout-output-at layout lx ly)
  ('* "wlr_output_layout_output_at" (list '* ffi:double ffi:double))
  (let ((o (% (unwrap-wlr-output-layout layout) lx ly)))
    (if (ffi:null-pointer? o)
        #f
        (wrap-wlr-output o))))

(define-wlr-procedure (wlr-output-layout-add layout output lx ly)
  ('* "wlr_output_layout_add" (list '* '* ffi:int ffi:int))
  (wrap-wlr-output-layout-output
   (% (unwrap-wlr-output-layout layout) output lx ly)))

(define-wlr-procedure (wlr-output-layout-remove layout output)
  (ffi:void "wlr_output_layout_remove" '(* *))
  (% (unwrap-wlr-output-layout layout) (unwrap-wlr-output output)))

(define-wlr-procedure (wlr-output-layout-output-coords layout reference)
  (ffi:void "wlr_output_layout_output_coords" (list '* '* '* '*))
  (let ((dlx (bytestructure double))
        (dly (bytestructure double)))
    (% (unwrap-wlr-output-layout layout)
       (unwrap-wlr-output reference)
       (bytestructure->pointer dlx)
       (bytestructure->pointer dly))
    (cons (bytestructure-ref dlx) (bytestructure-ref dly))))

(define-wlr-procedure (wlr-output-layout-contains-point layout reference lx ly)
  (ffi:int8 "wlr_output_layout_contains_point" (list '* '* ffi:int ffi:int))
  (not (zero? (% (unwrap-wlr-output-layout layout)
                 (unwrap-wlr-output reference) lx ly))))

(define-wlr-procedure (wlr-output-layout-intersects
                       layout reference target_lbox)
  (ffi:int8 "wlr_output_layout_intersects" (list '* '* '*))
  (not (zero?
        (% (unwrap-wlr-output-layout layout)
           (unwrap-wlr-output reference)
           (unwrap-wlr-box target_lbox)))))

(define-wlr-procedure (wlr-output-layout-closest-point
                       layout reference lx ly )
  (ffi:void
   "wlr_output_layout_closest_point"
   (list '* '* ffi:double ffi:double '* '*))
  (let ((dlx (bytestructure double))
        (dly (bytestructure double)))
    (% (unwrap-wlr-output-layout layout)
       (unwrap-wlr-output reference)
       lx ly
       (bytestructure->pointer dlx)
       (bytestructure->pointer dly))
    (cons (bytestructure-ref dlx)
          (bytestructure-ref dly))))

(define-wlr-procedure (wlr-output-layout-get-box
                       layout #:optional
                       reference (dest_box (make <wlr-box>)))
  (ffi:void "wlr_output_layout_get_box" (list '* '* '*))
  (% (unwrap-wlr-output-layout layout)
     (if reference
         (unwrap-wlr-output reference)
         ffi:%null-pointer)
     (unwrap-wlr-box dest_box))
  dest_box)

(define-wlr-procedure (wlr-output-layout-add-auto layout output)
  ('* "wlr_output_layout_add_auto" '(* *))
  (wrap-wlr-output-layout-output
   (% (unwrap-wlr-output-layout layout) (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-output-layout-get-center-output layout)
  ('* "wlr_output_layout_get_center_output" (list '*))
  (wrap-wlr-output (% (unwrap-wlr-output-layout layout))))

(define-wlr-procedure (wlr-output-layout-adjacent-output layout direction reference ref-lx ref-ly)
  ('* "wlr_output_layout_adjacent_output" (list '* ffi:int32 '* ffi:double ffi:double))
  (wrap-wlr-output
   (% (unwrap-wlr-output-layout layout)
      (bs:enum->integer %wlr-direction-enum direction)
      (unwrap-wlr-output reference)
      ref-lx
      ref-ly)))

(define-wlr-procedure (wlr-output-layout-farthest-output layout direction reference ref-lx ref-ly)
  ('* "wlr_output_layout_farthest_output" (list '* ffi:int32 '* ffi:double ffi:double))
  (wrap-wlr-output
   (% (unwrap-wlr-output-layout layout)
      (bs:enum->integer %wlr-direction-enum direction)
      (unwrap-wlr-output reference)
      ref-lx
      ref-ly)))
