(define-module (wlroots types xcursor)
  #:use-module (wayland list)
  ;; #:use-module (wlroots render renderer)
  ;; #:use-module (wlroots types output-layout)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select ((uint32 . ffi:uint32)
                                           (float . ffi:float)
                                           (int . ffi:int)
                                           (void . ffi:void)
                                           %null-pointer
                                           string->pointer))
  #:use-module (wlroots types cursor)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)

  #:export (wrap-wlr-xcursor-manager
            unwrap-wlr-xcursor-manager
            wlr-xcursor-manager-create
            wlr-xcursor-manager-load
            wlr-xcursor-manager-set-cursor-image))

(define-wlr-types-class wlr-xcursor-manager ()
  #:descriptor %wlr-xcursor-manager-struct)
(define-wlr-procedure (wlr-xcursor-manager-create name size)
  ('* "wlr_xcursor_manager_create" (list '* ffi:uint32))
  (wrap-wlr-xcursor-manager (% (if name (string->pointer name ) %null-pointer) size)))

(define-wlr-procedure (wlr-xcursor-manager-destroy m)
  (ffi:void "wlr_xcursor_manager_destroy" '(*))
  (% (unwrap-wlr-xcursor-manager m)))

(define-wlr-procedure (wlr-xcursor-manager-load xmgr scale)
  (ffi:int "wlr_xcursor_manager_load" (list '* ffi:float))
  (wrap-wlr-xcursor-manager (% (unwrap-wlr-xcursor-manager xmgr) scale)))

(define-wlr-procedure (wlr-xcursor-manager-set-cursor-image manager name cursor)
  (ffi:void "wlr_xcursor_manager_set_cursor_image" (list '* '* '*))
  (% (unwrap-wlr-xcursor-manager manager)
     (string->pointer name)
     (unwrap-wlr-cursor cursor)))
