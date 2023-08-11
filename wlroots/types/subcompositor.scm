(define-module (wlroots types subcompositor)
  #:use-module (wayland server display)
  #:use-module (wayland server resource)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots render texture)
  #:use-module (wlroots types)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types output)
  #:use-module (wlroots time)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.added
            .cached-seq
            .current
            .data
            .display-destroy
            .global
            .has-cache
            .link
            .mapped
            .parent
            .parent-destroy
            .pending
            .reordered
            .surface
            .surface-client-commit
            .synchronized
            .x
            .y
            .resource))

(define-wlr-types-class wlr-subsurface-parent-state ()
(x #:accessor .x)
(y #:accessor .y)
(link #:accessor .link)
#:descriptor %wlr-subsurface-parent-state-struct)


(define-wlr-types-class wlr-subcompositor ()
  (global #:accessor .global)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-subcompositor-struct)

(define-wlr-types-class  wlr-subsurface ()
  (resource #:accessor .resource)
  (surface #:accessor .surface)
  (parent #:accessor .parent)
  (current #:accessor .current)
  (pending #:accessor .pending)
  (cached-seq #:accessor .cached-seq)
  (has-cache #:accessor .has-cache)
  (synchronized #:accessor .synchronized)
  (reordered #:accessor .reordered)
  (mapped #:accessor .mapped)
  (added #:accessor .added)
  (surface-client-commit #:accessor .surface-client-commit)
  (parent-destroy #:accessor .parent-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-subsurface-struct)

(define-wlr-procedure (wlr-surface-is-subsurface surface)
  (ffi:int8 "wlr_surface_is_subsurface" (list '*))
  (not (zero? (% (unwrap-wlr-surface surface)))))
(define-wlr-procedure (wlr-subsurface-from-wlr-surface surface)
  ('* "wlr_subsurface_from_wlr_surface" (list '*))
  (wrap-wlr-subsurface (% (unwrap-wlr-surface surface))))

(define-super-surface-from-surface
  wlr-surface-is-subsurface
  wlr-subsurface-from-wlr-surface)


(define-wlr-procedure (wlr-subcompositor-create display)
  ('* "wlr_subcompositor_create" (list '*))
  (wrap-wlr-subcompositor (% (unwrap-wl-display display))))
