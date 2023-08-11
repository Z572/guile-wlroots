(define-module (wlroots types screencopy)
  #:use-module (wayland server display)
  #:use-module (bytestructure-class)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:duplicates
  (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (.box
            .buffer
            .buffer-cap
            .client
            .cursor-locked
            .damages
            .data
            .display-destroy
            .dmabuf-format
            .frames
            .link
            .manager
            .output
            .output-commit
            .output-destroy
            .output-enable
            .overlay-cursor
            .ref
            .resource
            .shm-format
            .shm-stride
            .with-damage
            .global))

(define-wlr-types-class wlr-screencopy-manager-v1 ()
  (global #:accessor .global)
  (frames #:accessor .frames)
  (display-destroy #:accessor .display-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-screencopy-manager-v1-struct)

(define-wlr-types-class wlr-screencopy-v1-client ()
  (ref #:accessor .ref)
  (manager #:accessor .manager)
  (damages #:accessor .damages)
  #:descriptor %wlr-screencopy-v1-client-struct)

(define-wlr-types-class wlr-screencopy-frame-v1 ()
  (resource #:accessor .resource)
  (client #:accessor .client)
  (link #:accessor .link)
  (shm-format #:accessor .shm-format)
  (dmabuf-format #:accessor .dmabuf-format)
  (box #:accessor .box)
  (shm-stride #:accessor .shm-stride)
  (overlay-cursor #:accessor .overlay-cursor)
  (cursor-locked #:accessor .cursor-locked)
  (with-damage #:accessor .with-damage)
  (buffer-cap #:accessor .buffer-cap)
  (buffer #:accessor .buffer)
  (output #:accessor .output)
  (output-commit #:accessor .output-commit)
  (output-destroy #:accessor .output-destroy)
  (output-enable #:accessor .output-enable)
  (data #:accessor .data)
  #:descriptor %wlr-screencopy-frame-v1-struct)

(define-wlr-procedure (wlr-screencopy-manager-v1-create display)
  ('* "wlr_screencopy_manager_v1_create" (list '*))
  (wrap-wlr-screencopy-manager-v1 (% (unwrap-wl-display display))))
