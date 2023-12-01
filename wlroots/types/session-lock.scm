(define-module (wlroots types session-lock)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types)
  #:use-module (wlroots types compositor)
  #:use-module (wayland server display)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:duplicates
  (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.configure-serial
            .configured
            .current
            .data
            .height
            .link
            .pending
            .resource
            .serial
            .surface
            .surfaces
            .width
            .global))

(define-wlr-types-class wlr-session-lock-manager-v1 ()
  (events (new-lock wrap-wlr-session-lock-v1))
  (global #:accessor .global)
  (data #:accessor .data)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-session-lock-manager-v1-struct)

(define-wlr-types-class wlr-session-lock-v1 ()
  (events (new-surface wrap-wlr-session-lock-surface-v1))
  (resource #:accessor .resource)
  (surfaces #:accessor .surfaces)
  (data #:accessor .data)
  #:descriptor %wlr-session-lock-v1-struct)

(define-wlr-types-class wlr-session-lock-surface-v1-state ()
  (width #:accessor .width)
  (height #:accessor .height)
  (configure-serial #:accessor .configure-serial)
  #:descriptor %wlr-session-lock-surface-v1-state-struct)

(define-wlr-types-class wlr-session-lock-surface-v1-configure ()
  (link #:accessor .link)
  (serial #:accessor .serial)
  (width #:accessor .width)
  (height #:accessor .height)
  #:descriptor %wlr-session-lock-surface-v1-configure-struct)

(define-wlr-types-class wlr-session-lock-surface-v1 ()
  (resource #:accessor .resource)
  (link #:accessor .link)
  (output #:accessor .output)
  (surface #:accessor .surface)
  (configured #:accessor .configured)
  (configure-list #:accessor .configure-list)
  (current #:accessor .current)
  (pending #:accessor .pending)
  (data #:accessor .data)
  #:descriptor %wlr-session-lock-surface-v1-struct)

(define-wlr-procedure (wlr-session-lock-manager-v1-create display)
  ('* "wlr_session_lock_manager_v1_create" (list '*))
  (wrap-wlr-session-lock-manager-v1 (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-session-lock-v1-send-locked lock)
  (ffi:void "wlr_session_lock_v1_send_locked" (list '*))
  (% (unwrap-wlr-session-lock-v1 lock)))

(define-wlr-procedure (wlr-session-lock-v1-destroy lock)
  (ffi:void "wlr_session_lock_v1_destroy" (list '*))
  (% (unwrap-wlr-session-lock-v1 lock)))

(define-wlr-procedure (wlr-session-lock-surface-v1-configure
                       lock-surface width height)
  (ffi:uint32 "wlr_session_lock_surface_v1_configure"
              (list '* ffi:uint32 ffi:uint32))
  (% (unwrap-wlr-session-lock-surface-v1 lock-surface) width height))

(define-wlr-procedure (wlr-session-lock-surface-v1-try-from-wlr-surface surface)
  ('* "wlr_session_lock_surface_v1_try_from_wlr_surface" (list '*))
  (wrap-wlr-session-lock-surface-v1 (% surface)))
