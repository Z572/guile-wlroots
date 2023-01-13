(define-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland listener)
  #:use-module (wlroots render renderer)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots time)
  #:use-module (wlroots types)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots types output)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types presentation-time)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:re-export (%wlr-scene-node-type-enum
               %wlr-scene-node-state-struct
               %wlr-scene-node-struct
               %wlr-scene-struct
               %wlr-scene-rect-struct
               %wlr-scene-tree-struct
               %wlr-scene-buffer-struct)
  #:export (wrap-wlr-scene
            unwrap-wlr-scene
            wrap-wlr-scene-node
            unwrap-wlr-scene-node
            wrap-wlr-scene-tree
            unwrap-wlr-scene-tree
            wrap-wlr-scene-rect
            unwrap-wlr-scene-rect
            wrap-wlr-scene-buffer
            unwrap-wlr-scene-buffer
            wlr-scene-create
            wlr-scene-buffer-create
            wlr-scene-attach-output-layout
            wlr-scene-node-destroy
            wlr-scene-node-set-position
            wlr-scene-node-at
            wlr-scene-tree-create
            wlr-scene-set-presentation
            wlr-scene-node-raise-to-top
            wlr-scene-node-lower-to-bottom

            wlr-scene-node-set-enabled
            wlr-scene-node-reparent
            wlr-scene-surface-create
            wlr-scene-surface-from-node
            wlr-scene-rect-create
            wlr-scene-rect-set-size
            wlr-scene-rect-set-color
            wlr-scene-rect-node
            wlr-scene-output-create
            wlr-scene-output-destroy
            wlr-scene-output-set-position
            wlr-scene-output-commit
            wlr-scene-output-send-frame-done
            wlr-scene-xdg-surface-create
            wlr-scene-subsurface-tree-create
            wrap-wlr-scene-node-state
            unwrap-wlr-scene-node-state
            .data
            .enabled
            .output
            .parent
            .prev-height
            .prev-scanout
            .prev-width
            .primary-output
            .scene
            .state
            .surface
            .type
            .x
            .y
            .node))

(define-wlr-types-class wlr-scene-node-state ()
  (enabled #:accessor .enabled #:allocation #:bytestructure)
  (x #:accessor .x #:allocation #:bytestructure)
  (y #:accessor .y #:allocation #:bytestructure)

  #:descriptor %wlr-scene-node-state-struct)

(define-wlr-types-class wlr-scene-node ()
  (type #:accessor .type)
  (parent #:accessor .parent)
  (state #:accessor .state)
  (data #:accessor .data)
  #:descriptor %wlr-scene-node-struct)
(define-wlr-types-class wlr-scene-tree ()
  (node #:allocation #:bytestructure
        #:accessor .node)
  #:descriptor %wlr-scene-tree-struct)

(define-wlr-types-class wlr-scene-surface ()
  (node #:accessor .node)
  (surface #:accessor .surface)
  (primary-output #:accessor .primary-output)
  (prev-width #:getter .prev-width)
  (prev-height #:getter .prev-height)
  #:descriptor  %wlr-scene-surface-struct)

(define-wlr-types-class wlr-scene-buffer ()
  (node #:accessor .node
        #:allocation #:bytestructure)
  #:descriptor %wlr-scene-buffer-struct)

(define-wlr-types-class wlr-scene-rect ()
  (node #:accessor .node
        #:allocation
        #:bytestructure)
  #:descriptor %wlr-scene-rect-struct)

(define-wlr-types-class wlr-scene-output ()
  (output #:accessor .output)
  (scene #:accessor .scene)
  (x #:accessor .x)
  (y #:accessor .y)
  (prev-scanout #:accessor .prev-scanout)
  #:descriptor %wlr-scene-output-struct)
(define wlr-scene-rect-node .node)
(define-wlr-types-class wlr-scene ()
  (node #:allocation #:bytestructure
        #:accessor .node)
  #:descriptor %wlr-scene-struct)

(define-wlr-procedure (wlr-scene-create)
  ('* "wlr_scene_create" '())
  (wrap-wlr-scene (%)))

(define-wlr-procedure (wlr-scene-attach-output-layout scene output-layout)
  (ffi:int "wlr_scene_attach_output_layout" '(* *))
  (% (unwrap-wlr-scene scene) (unwrap-wlr-output-layout output-layout)))

(define-wlr-procedure (wlr-scene-node-destroy node)
  (ffi:void "wlr_scene_node_destroy" '(*))
  (% (unwrap-wlr-scene-node node)))

(define-wlr-procedure (wlr-scene-node-set-enabled node enabled)
  (ffi:void "wlr_scene_node_set_enabled" (list '* ffi:int))
  (% (unwrap-wlr-scene-node node) (if enabled 1 0)))

(define-wlr-procedure (wlr-scene-node-set-position scene x y)
  (ffi:void "wlr_scene_node_set_position" (list '* ffi:int ffi:int))
  (% (unwrap-wlr-scene-node scene) x y))

(define-wlr-procedure (wlr-scene-set-presentation scene presentation)
  (ffi:void "wlr_scene_set_presentation" '(* *))
  (% (unwrap-wlr-scene scene) (unwrap-wlr-presentation presentation)))

(define-wlr-procedure (wlr-scene-tree-create parent)
  ('* "wlr_scene_tree_create" '(*))
  (wrap-wlr-scene-tree (% (unwrap-wlr-scene-node parent))))
(define-wlr-procedure (wlr-scene-node-at node lx ly)
  ('* "wlr_scene_node_at" (list '* ffi:double ffi:double '* '*))
  (define (ref-double-pointer p)
    (bytevector-ieee-double-native-ref
     (ffi:pointer->bytevector
      p (ffi:sizeof ffi:double)) 0))
  (let* ((nx (ffi:bytevector->pointer (make-bytevector (ffi:sizeof '*))))
         (ny (ffi:bytevector->pointer (make-bytevector (ffi:sizeof '*))))
         (value (% (get-pointer node) lx ly nx ny)))
    (and (not (ffi:null-pointer? value))
         (values
          (wrap-wlr-scene-node value)
          `(,(ref-double-pointer nx) .
            ,(ref-double-pointer ny))))))

(define-wlr-procedure (wlr-scene-node-raise-to-top node)
  (ffi:void "wlr_scene_node_raise_to_top" '(*))
  (% (unwrap-wlr-scene-node node)))

(define-wlr-procedure (wlr-scene-node-lower-to-bottom node)
  (ffi:void "wlr_scene_node_lower_to_bottom" '(*))
  (% (unwrap-wlr-scene-node node)))

(define-wlr-procedure (wlr-scene-surface-create parent surface)
  ('* "wlr_scene_surface_create" '(* *))
  (wrap-wlr-scene-surface
   (% (unwrap-wlr-scene-node parent)
      (unwrap-wlr-surface surface) )))

(define-wlr-procedure (wlr-scene-surface-from-node node)
  ('* "wlr_scene_surface_from_node" '(*))
  (wrap-wlr-scene-surface (% (unwrap-wlr-scene-node node))))

(define-wlr-procedure (wlr-scene-rect-create parent width height color)
  ('* "wlr_scene_rect_create" (list '* ffi:int ffi:int '*))
  (wrap-wlr-scene-rect
   (% (unwrap-wlr-scene-node parent)
      width
      height
      (color->pointer color))))
(define-wlr-procedure (wlr-scene-rect-set-size rect width height)
  (ffi:void "wlr_scene_rect_set_size" (list '* ffi:int ffi:int))
  (% (unwrap-wlr-scene-rect rect)
     width height))
(define-wlr-procedure (wlr-scene-rect-set-color rect color)
  (ffi:void "wlr_scene_rect_set_color" (list '* '*))
  (% (unwrap-wlr-scene-rect rect)
     (color->pointer color)))

(define-wlr-procedure (wlr-scene-node-reparent node new-parent)
  (ffi:void "wlr_scene_node_reparent" '(* *))
  (% (unwrap-wlr-scene-node node)
     (unwrap-wlr-scene-node new-parent)))

(define-wlr-procedure (wlr-scene-buffer-create parent buffer)
  ('* "wlr_scene_buffer_create" '(* *))
  (wrap-wlr-scene-buffer (% (unwrap-wlr-scene-node parent)
                            (unwrap-wlr-buffer buffer))))

(define-wlr-procedure (wlr-scene-output-create scene output)
  ('* "wlr_scene_output_create" '(* *))
  (wrap-wlr-scene-output (% (unwrap-wlr-scene scene) (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-scene-output-destroy scene-output)
  (ffi:void "wlr_scene_output_destroy" '(*))
  (% (unwrap-wlr-scene-output scene-output)))

(define-wlr-procedure (wlr-scene-output-set-position scene-output lx ly)
  (ffi:void "wlr_scene_output_set_position" (list '* ffi:int ffi:int))
  (% (unwrap-wlr-scene-output scene-output) lx ly))

(define-wlr-procedure (wlr-scene-output-commit scene-output)
  (ffi:int "wlr_scene_output_commit" '(*))
  (not (zero? (% (unwrap-wlr-scene-output scene-output)))))

(define-wlr-procedure (wlr-scene-output-send-frame-done scene-output now)
  (ffi:void "wlr_scene_output_send_frame_done" '(* *))
  (% (unwrap-wlr-scene-output scene-output)
     (unwrap-timespec now)))
(define-wlr-procedure (wlr-scene-subsurface-tree-create parent surface)
  ('* "wlr_scene_subsurface_tree_create" '(* *))
  (wrap-wlr-scene-node (% (unwrap-wlr-scene-node parent)
                          (unwrap-wlr-surface surface))))

(define-wlr-procedure (wlr-scene-xdg-surface-create parent xdg-surface)
  ('* "wlr_scene_xdg_surface_create" '(* *))
  (wrap-wlr-scene-node (% (unwrap-wlr-scene-node parent)
                          (unwrap-wlr-xdg-surface xdg-surface))))
