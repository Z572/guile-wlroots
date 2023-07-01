(define-module (wlroots types export-dmabuf)
  #:use-module (wayland display)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.global
            .frames
            .display-destroy
            .resource
            .manager
            .link
            .output
            .cursor-locked
            .output-commit))

(define-wlr-types-class wlr-export-dmabuf-manager-v1 ()
  (global #:accessor .global)
  (frames #:accessor .frames)
  (display-destroy #:accessor .display-destroy)
  #:descriptor %wlr-export-dmabuf-manager-v1-struct)

(define-wlr-types-class wlr-export-dmabuf-frame-v1 ()
  (resource #:accessor .resource)
  (manager #:accessor .manager)
  (link #:accessor .link)
  (output #:accessor .output)
  (cursor-locked #:accessor .cursor-locked)
  (output-commit #:accessor .output-commit)
  #:descriptor %wlr-export-dmabuf-frame-v1-struct)

(define-wlr-procedure (wlr-export-dmabuf-manager-v1-create display)
  ('* "wlr_export_dmabuf_manager_v1_create" (list '*))
  (wrap-wlr-export-dmabuf-manager-v1 (% (unwrap-wl-display display))))
