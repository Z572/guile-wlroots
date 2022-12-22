(define-module (wlroots render renderer)
  #:use-module (wayland display)
  #:use-module (wayland signal)
  #:use-module (wlroots utils)
  #:use-module (wlroots backend)
  #:use-module (wlroots types)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:export (wrap-wlr-renderer unwrap-wlr-renderer wlr-renderer-autocreate
                              wlr-renderer-init-wl-display
                              .rendering
                              .rendering-with-buffer))

(define %wlr-renderer-struct
  (bs:struct `((impl ,(bs:pointer '*))
               (rendering ,bool)
               (rendering-with-buffer ,bool)
               (events ,(bs:struct `((destroy ,%wl-signal-struct)))))))
(define-wlr-types-class wlr-renderer ()
  (rendering #:getter .rendering #:allocation #:bytestructure)
  (rendering-with-buffer #:getter .rendering-with-buffer #:allocation #:bytestructure)
  #:descriptor %wlr-renderer-struct)

(define-wlr-procedure (wlr-renderer-autocreate backend)
  ('* "wlr_renderer_autocreate" (list '*))
  (wrap-wlr-renderer (% (unwrap-wlr-backend backend))))
(define-wlr-procedure (wlr-renderer-init-wl-display renderer display)
  (ffi:int "wlr_renderer_init_wl_display" '(* *))
  (% (unwrap-wlr-renderer renderer) (unwrap-wl-display display)))
