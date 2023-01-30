(define-module (wlroots types xdg-output)
  #:use-module (wayland display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module (wlroots types)
  #:use-module (wlroots types output-layout)
  #:export
  (wlr-xdg-output-manager-v1-create
   .description
   .destroy
   .display-destroy
   .events
   .height
   .layout
   .layout-add
   .layout-change
   .layout-destroy
   .layout-output
   .link
   .manager
   .outputs
   .resources
   .width
   .x
   .y
   .global))

(define-wlr-types-class wlr-xdg-output-v1 ()
  (manager #:accessor .manager)
  (resources #:accessor .resources)
  (link #:accessor .link)
  (layout-output #:accessor .layout-output)
  (x #:accessor .x)
  (y #:accessor .y)
  (width #:accessor .width)
  (height #:accessor .height)
  (destroy #:accessor .destroy)
  (description #:accessor .description)
  #:descriptor %wlr-xdg-output-v1-struct)

(define-wlr-types-class wlr-xdg-output-manager-v1 ()
  (global #:accessor .global)
  (layout #:accessor .layout)
  (outputs #:accessor .outputs)
  (events #:accessor .events)
  (display-destroy #:accessor .display-destroy)
  (layout-add #:accessor .layout-add)
  (layout-change #:accessor .layout-change)
  (layout-destroy #:accessor .layout-destroy)
  #:descriptor %wlr-xdg-output-manager-v1-struct)

(define-wlr-procedure (wlr-xdg-output-manager-v1-create display layout)
  ('* "wlr_xdg_output_manager_v1_create" (list '* '*))
  (wrap-wlr-xdg-output-manager-v1
   (% (unwrap-wl-display display) (unwrap-wlr-output-layout layout))))
