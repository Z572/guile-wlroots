(define-module (wlroots render drm-format-set)
  #:use-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:export (wlr-drm-format-set-finish
            wlr-drm-format-set-get
            wlr-drm-format-set-has
            wlr-drm-format-set-add
            wlr-drm-format-set-intersect
            .len .capacity .formats .format .modifiers))
(define-wlr-types-class wlr-drm-format ()
  (format #:accessor .format)
  (len #:accessor .len)
  (capacity #:accessor .capacity)
  (modifiers #:accessor .modifiers)
  #:descriptor %wlr-drm-format-struct)

(define-wlr-types-class wlr-drm-format-set ()
  (len #:accessor .len)
  (capacity #:accessor .capacity)
  (formats #:accessor .formats)
  #:descriptor %wlr-drm-format-set-struct)

(define-wlr-procedure (wlr-drm-format-set-finish set)
  (ffi:void "wlr_drm_format_set_finish" (list '*))
  (% (unwrap-wlr-drm-format-set set)))

(define-wlr-procedure (wlr-drm-format-set-get set format)
  ('* "wlr_drm_format_set_get" (list '* ffi:uint32))
  (wrap-wlr-drm-format (% (unwrap-wlr-drm-format-set set) format)))

(define-wlr-procedure (wlr-drm-format-set-has set format modifier)
  (ffi:int8 "wlr_drm_format_set_has" (list '* ffi:uint32 ffi:uint64))
  (not (zero? (% (unwrap-wlr-drm-format-set set) format modifier))))

(define-wlr-procedure (wlr-drm-format-set-add set format modifier)
  (ffi:int8 "wlr_drm_format_set_add" (list '* ffi:uint32 ffi:uint64))
  (not (zero? (% (unwrap-wlr-drm-format-set set) format modifier))))

(define-wlr-procedure (wlr-drm-format-set-intersect dst a b)
  (ffi:int8 "wlr_drm_format_set_intersect" (list '* '* '*))
  (not (zero?
        (% (unwrap-wlr-drm-format-set dst)
           (unwrap-wlr-drm-format-set a)
           (unwrap-wlr-drm-format-set b)))))
