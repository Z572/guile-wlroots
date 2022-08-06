(define-module (wlroots types output-layout)
  #:use-module (wayland display)
  ;;#:use-module (wlroots render renderer)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wayland signal)
  #:use-module (oop goops)
  #:use-module (bytestructures guile)
  #:export (wrap-wlr-output-layout
            unwrap-wlr-output-layout
            wlr-output-layout-create
            get-event-signal))

(define-wlr-types-class wlr-output-layout)

(define %wlr-output-layout-struct
  (bs:struct
   `((outputs ,%wl-list)
     (state ,(bs:pointer '*))
     (events ,(bs:struct
               `((add ,%wl-signal-struct)
                 (change ,%wl-signal-struct)
                 (destroy ,%wl-signal-struct))))
     (data ,(bs:pointer ffi:void)))))

(define-wlr-procedure (wlr-output-layout-create)
  ('* "wlr_output_layout_create" '())
  (wrap-wlr-output-layout (%)))

(define-method (get-event-signal (b <wlr-output-layout>) (signal-name <symbol>))
  (let* ((unwrap-b (unwrap-wlr-output-layout b))
         (o (bytestructure-ref
             (pointer->bytestructure
              unwrap-b
              %wlr-output-layout-struct)
             'events signal-name)))
    (wrap-wl-signal (+ o 24))))
