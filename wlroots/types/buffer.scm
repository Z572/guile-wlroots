(define-module (wlroots types buffer)
  #:use-module (wlroots types)
  #:use-module (oop goops)
  #:use-module (wlroots utils)
  #:use-module ((system foreign) #:prefix ffi:)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:re-export (%wlr-buffer-struct)
  #:export (wrap-wlr-buffer
            unwrap-wlr-buffer
            wlr-buffer-drop
            wlr-buffer-lock
            wlr-buffer-unlock
            .base
            .texture
            .source
            .shm-source-format))

(define-wlr-types-class wlr-buffer ()
  #:descriptor %wlr-buffer-struct)

(define-wlr-types-class wlr-client-buffer ()
  (base #:accessor .base)
  (texture #:accessor .texture)
  (source #:accessor .source)
  (shm-source-format #:accessor .shm-source-format)
  #:descriptor %wlr-client-buffer-struct)

(define-wlr-procedure (wlr-buffer-drop buffer)
  (ffi:void "wlr_buffer_drop" '(*))
  (% (unwrap-wlr-buffer buffer)))

(define-wlr-procedure (wlr-buffer-lock buffer)
  ('* "wlr_buffer_lock" '(*))
  (wrap-wlr-buffer (% (unwrap-wlr-buffer buffer))))
(define-wlr-procedure (wlr-buffer-unlock buffer)
  (ffi:void "wlr_buffer_unlock" '(*))
  (% (unwrap-wlr-buffer buffer)))
