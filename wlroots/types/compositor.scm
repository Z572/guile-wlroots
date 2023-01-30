(define-module (wlroots types compositor)
  #:use-module (wayland display)
  #:use-module (wayland resource)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots render texture)
  #:use-module (wlroots types)
  #:use-module (wlroots types output)
  #:use-module (wlroots time)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wrap-wlr-compositor
            unwrap-wlr-compositor
            wlr-compositor-create
            wlr-surface-has-buffer
            wlr-surface-send-enter
            wlr-surface-send-leave
            wlr-surface-send-frame-done
            wlr-surface-get-root-surface
            .resource
            .renderer
            .buffer
            .sx
            .sy
            .data))

(define-wlr-types-class-public wlr-surface ()
  (resource #:accessor .resource)
  (renderer #:accessor .renderer)
  (buffer #:accessor .buffer)
  (sx #:accessor .sx)
  (sy #:accessor .sy)
  (data #:accessor .data)
  #:descriptor %wlr-surface-struct)

(define-wlr-types-class wlr-surface-output ()
  (surface #:accessor .surface)
  (output #:accessor .output)
  (link #:accessor .link)
  (bind #:accessor .bind)
  (destroy #:accessor .destroy)
  #:descriptor %wlr-surface-output-struct)

(define-wlr-types-class wlr-compositor ()
  (global #:accessor .global)
  (renderer #:accessor .renderer)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-compositor-struct)

(define-wlr-procedure (wlr-surface-destroy-role-object surface)
  (ffi:void "wlr_surface_destroy_role_object" (list '*))
  (% (unwrap-wlr-surface surface)))
(define-wlr-procedure (wlr-surface-has-buffer surface)
  (ffi:int8 "wlr_surface_has_buffer" (list '*))
  (not (zero? (% (unwrap-wlr-surface surface)))))
(define-wlr-procedure (wlr-surface-get-texture surface)
  ('* "wlr_surface_get_texture" (list '*))
  (wrap-wlr-texture (% (unwrap-wlr-surface surface))))
(define-wlr-procedure (wlr-surface-get-root-surface surface)
  ('* "wlr_surface_get_root_surface" '(*))
  (let ((o (% (unwrap-wlr-surface surface))))
    (and (not (ffi:null-pointer? o))
         (wrap-wlr-surface o))))
(define-wlr-procedure (wlr-surface-point-accepts-input surface sx sy)
  (ffi:int8 "wlr_surface_point_accepts_input" (list '* ffi:double ffi:double))
  (not (zero? (% (unwrap-wlr-surface surface) sx sy))))

(define-wlr-procedure (wlr-surface-surface-at surface sx sy)
  ('* "wlr_surface_surface_at" (list '* ffi:double ffi:double '* '*))
  (define (ref-double-pointer p)
    (bytevector-ieee-double-native-ref
     (ffi:pointer->bytevector
      p (ffi:sizeof ffi:double)) 0))
  (let ((x (ffi:bytevector->pointer (make-bytevector (ffi:sizeof '*))))
        (y (ffi:bytevector->pointer (make-bytevector (ffi:sizeof '*)))))
    (values (% (unwrap-wlr-surface surface) sx sy x y)
            (ref-double-pointer x) (ref-double-pointer y))))

(define-wlr-procedure (wlr-surface-send-enter surface output)
  (ffi:void "wlr_surface_send_enter" (list '* '*))
  (% (unwrap-wlr-surface surface) (unwrap-wlr-output output)))
(define-wlr-procedure (wlr-surface-send-leave surface output)
  (ffi:void "wlr_surface_send_leave" '(* *))
  (% (unwrap-wlr-surface surface) (unwrap-wlr-output output)))
(define-wlr-procedure
  (wlr-surface-send-frame-done surface when)
  (ffi:void "wlr_surface_send_frame_done" (list '* '*))
  (% (unwrap-wlr-surface surface) (unwrap-timespec when)))
(define-wlr-procedure
  (wlr-surface-get-extends surface box)
  (ffi:void "wlr_surface_get_extends" (list '* '*))
  (% (unwrap-wlr-surface surface) (unwrap-wlr-box box)))
(define-wlr-procedure
  (wlr-surface-from-resource resource)
  ('* "wlr_surface_from_resource" (list '*))
  (% (unwrap-wl-resource resource)))
(define-wlr-procedure
  (wlr-surface-for-each-surface surface iterator user_data)
  (ffi:void "wlr_surface_for_each_surface" (list '* '* '*))
  (% (unwrap-wlr-surface surface) iterator user_data))
(define-wlr-procedure
  (wlr-surface-get-buffer-source-box surface box)
  (ffi:void "wlr_surface_get_buffer_source_box" (list '* '*))
  (% (unwrap-wlr-surface surface) (unwrap-wlr-fbox box)))
(define-wlr-procedure
  (wlr-surface-lock-pending surface)
  (ffi:uint32 "wlr_surface_lock_pending" (list '*))
  (% (unwrap-wlr-surface surface)))
(define-wlr-procedure
  (wlr-surface-unlock-cached surface seq)
  (ffi:void "wlr_surface_unlock_cached" (list '* ffi:uint32))
  (% (unwrap-wlr-surface surface) seq))
(define-wlr-procedure
  (wlr-compositor-create display renderer)
  ('* "wlr_compositor_create" (list '* '*))
  (wrap-wlr-compositor
   (% (unwrap-wl-display display) (unwrap-wlr-renderer renderer))))
