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
            .accessing-data-ptr
            .addons
            .base
            .dropped
            .height
            .n-ignore-locks
            .n-locks
            .source
            .texture
            .width))

(define-wlr-types-class wlr-buffer ()
  (width #:accessor .width)
  (height #:accessor .height)
  (dropped #:accessor .dropped)
  (n-locks #:accessor .n-locks)
  (accessing-data-ptr #:accessor .accessing-data-ptr)
  (addons #:accessor .addons)
  #:descriptor %wlr-buffer-struct)

(define-wlr-types-class wlr-client-buffer ()
  (base #:accessor .base)
  (texture #:accessor .texture)
  (source #:accessor .source)
  (n-ignore-locks #:accessor .n-ignore-locks)
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
