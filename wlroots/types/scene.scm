(define-module (wlroots types scene)
  #:use-module (srfi srfi-71)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (wayland protocol)
  #:use-module (wayland signal)
  #:use-module (wayland listener)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots render dmabuf)
  #:use-module (wlroots time)
  #:use-module (wlroots types)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots types layer-shell)
  #:use-module (wlroots types output)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots types presentation-time)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:re-export (%wlr-scene-node-type-enum
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
            wrap-wlr-scene-node-state
            unwrap-wlr-scene-node-state

            wlr-scene-output-create
            wlr-scene-output-destroy
            wlr-scene-output-set-position
            wlr-scene-output-commit
            wlr-scene-output-send-frame-done
            wlr-scene-xdg-surface-create
            wlr-scene-subsurface-tree-create
            wlr-scene-create
            wlr-scene-attach-output-layout
            wlr-scene-node-destroy
            wlr-scene-node-set-enabled
            wlr-scene-node-set-position
            wlr-scene-set-presentation
            wlr-scene-tree-create
            wlr-scene-node-at
            wlr-scene-node-place-above
            wlr-scene-node-place-below
            wlr-scene-node-raise-to-top
            wlr-scene-node-lower-to-bottom
            wlr-scene-node-reparent
            wlr-scene-surface-create
            wlr-scene-surface-from-buffer
            wlr-scene-rect-create
            wlr-scene-rect-set-size
            wlr-scene-rect-set-color
            wlr-scene-buffer-create
            wlr-scene-buffer-set-buffer
            wlr-scene-buffer-set-source-box
            wlr-scene-buffer-set-dest-size
            wlr-scene-buffer-set-transform
            wlr-scene-buffer-send-frame-done
            wlr-scene-output-create
            wlr-scene-output-destroy
            wlr-scene-output-set-position
            wlr-scene-output-commit
            wlr-scene-output-send-frame-done
            wlr-scene-subsurface-tree-create
            wlr-scene-xdg-surface-create
            wlr-scene-layer-surface-v1-create
            wlr-scene-layer-surface-v1-configure
            .data
            .enabled
            .node
            .output
            .outputs
            .parent
            .peeding-buffers
            .presentation
            .presentation-destroy
            .prev-height
            .prev-scanout
            .prev-width
            .primary-output
            .tree
            .layer-surface
            .tree-destroy
            .layer-surface-destroy
            .layer-surface-map
            .layer-surface-unmap
            .scene
            .state
            .surface
            .type
            .x
            .y))

(define-wlr-types-class wlr-scene-node ()
  (type #:accessor .type)
  (parent #:accessor .parent)
  (link #:accessor .link)
  (enabled #:accessor .enabled)
  (x #:accessor .x)
  (y #:accessor .y)
  (data #:accessor .data)
  #:descriptor %wlr-scene-node-struct)
(define-wlr-types-class wlr-scene-tree ()
  (node #:allocation #:bytestructure
        #:accessor .node)
  #:descriptor %wlr-scene-tree-struct)

(define-wlr-types-class wlr-scene-surface ()
  (buffer #:accessor .buffer)
  (surface #:accessor .surface)
  #:descriptor  %wlr-scene-surface-struct)

(define-wlr-types-class wlr-scene-buffer ()
  (node #:accessor .node)
  (buffer #:accessor .buffer)
  (active-outputs #:accessor .active-outputs)
  (texture #:accessor .texture)
  (src-box #:accessor .src-box)
  (dst-width #:accessor .dst-width)
  (dst-height #:accessor .dst-height)
  (transform #:accessor .transform)
  #:descriptor %wlr-scene-buffer-struct)

(define-wlr-types-class wlr-scene-rect ()
  (node #:accessor .node
        #:allocation
        #:bytestructure)
  #:descriptor %wlr-scene-rect-struct)

(define-wlr-types-class wlr-scene-output ()
  (output #:accessor .output)
  (link #:accessor .link)
  (scene #:accessor .scene)
  (x #:accessor .x)
  (y #:accessor .y)
  (index #:accessor .index)
  (prev-scanout #:accessor .prev-scanout)
  #:descriptor %wlr-scene-output-struct)

(define-wlr-types-class wlr-scene ()
  (tree #:accessor .tree)
  (outputs #:accessor .outputs)
  (presentation #:accessor .presentation)
  (presentation-destroy #:accessor .presentation-destroy)
  (debug-damage-option #:accessor .debug-damage-option)
  (direct-scanout #:accessor .direct-scanout)
  (calculate-visibility #:accessor .calculate-visibility)
  #:descriptor %wlr-scene-struct)

(define-wlr-types-class wlr-scene-layer-surface-v1 ()
  (tree #:accessor .tree)
  (layer-surface #:accessor .layer-surface)
  (tree-destroy #:accessor .tree-destroy)
  (layer-surface-destroy #:accessor .layer-surface-destroy)
  (layer-surface-map #:accessor .layer-surface-map)
  (layer-surface-unmap #:accessor .layer-surface-unmap)
  #:descriptor %wlr-scene-layer-surface-v1-struct)

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
  (wrap-wlr-scene-tree (% (unwrap-wlr-scene-tree parent))))
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

(define-wlr-procedure
  (wlr-scene-node-place-above node sibling)
  (ffi:void "wlr_scene_node_place_above" (list '* '*))
  (% (unwrap-wlr-scene-node node) (unwrap-wlr-scene-node sibling)))
(define-wlr-procedure
  (wlr-scene-node-place-below node sibling)
  (ffi:void "wlr_scene_node_place_below" (list '* '*))
  (% (unwrap-wlr-scene-node node) (unwrap-wlr-scene-node sibling)))

(define-wlr-procedure (wlr-scene-node-raise-to-top node)
  (ffi:void "wlr_scene_node_raise_to_top" '(*))
  (% (unwrap-wlr-scene-node node)))

(define-wlr-procedure (wlr-scene-node-lower-to-bottom node)
  (ffi:void "wlr_scene_node_lower_to_bottom" '(*))
  (% (unwrap-wlr-scene-node node)))

(define-wlr-procedure (wlr-scene-node-reparent node new-parent)
  (ffi:void "wlr_scene_node_reparent" '(* *))
  (% (unwrap-wlr-scene-node node)
     (unwrap-wlr-scene-node new-parent)))

(define-wlr-procedure (wlr-scene-surface-create parent surface)
  ('* "wlr_scene_surface_create" '(* *))
  (wrap-wlr-scene-surface
   (% (unwrap-wlr-scene-tree parent)
      (unwrap-wlr-surface surface) )))

(define-wlr-procedure (wlr-scene-buffer-from-node node)
  ('* "wlr_scene_buffer_from_node" '(*))
  (wrap-wlr-scene-buffer (% (unwrap-wlr-scene-node node))))

(define-wlr-procedure (wlr-scene-surface-from-buffer scene-buffer)
  ('* "wlr_scene_surface_from_buffer" (list '*))
  (% scene-buffer))

(define-wlr-procedure (wlr-scene-rect-create parent width height color)
  ('* "wlr_scene_rect_create" (list '* ffi:int ffi:int '*))
  (wrap-wlr-scene-rect
   (% (unwrap-wlr-scene-tree parent)
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

(define-wlr-procedure (wlr-scene-buffer-create parent buffer)
  ('* "wlr_scene_buffer_create" '(* *))
  (wrap-wlr-scene-buffer (% (unwrap-wlr-scene-tree parent)
                            (unwrap-wlr-buffer buffer))))

(define-wlr-procedure (wlr-scene-buffer-set-buffer scene-buffer buffer)
  (ffi:void "wlr_scene_buffer_set_buffer" (list '* '*))
  (% (unwrap-wlr-scene-buffer scene-buffer) (unwrap-wlr-buffer buffer)))

(define-wlr-procedure (wlr-scene-buffer-set-source-box scene-buffer box)
  (ffi:void "wlr_scene_buffer_set_source_box" (list '* '*))
  (% (unwrap-wlr-scene-buffer scene-buffer) (unwrap-wlr-fbox box)))

(define-wlr-procedure (wlr-scene-buffer-set-dest-size scene-buffer width height)
  (ffi:void "wlr_scene_buffer_set_dest_size" (list '* ffi:int ffi:int))
  (% (unwrap-wlr-scene-buffer scene-buffer) width height))

(define-wlr-procedure (wlr-scene-buffer-set-transform scene-buffer transform)
  (ffi:void "wlr_scene_buffer_set_transform" (list '* ffi:int32))
  (% (unwrap-wlr-scene-buffer scene-buffer)
     (bs:enum->integer %wl-output-transform-enum transform)))

(define-wlr-procedure (wlr-scene-buffer-send-frame-done scene-buffer now)
  (ffi:void "wlr_scene_buffer_send_frame_done" (list '* '*))
  (% (unwrap-wlr-scene-buffer scene-buffer) (unwrap-timespec now)))

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
(define-wlr-procedure (wlr-scene-get-scene-output scene output)
  ('* "wlr_scene_get_scene_output" (list '* '*))
  (wrap-wlr-scene-output (% (unwrap-wlr-scene scene) (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-scene-subsurface-tree-create parent surface)
  ('* "wlr_scene_subsurface_tree_create" '(* *))
  (wrap-wlr-scene-tree (% (unwrap-wlr-scene-tree parent)
                          (unwrap-wlr-surface surface))))

(define-wlr-procedure (wlr-scene-xdg-surface-create parent xdg-surface)
  ('* "wlr_scene_xdg_surface_create" '(* *))
  (wrap-wlr-scene-tree (% (unwrap-wlr-scene-tree parent)
                          (unwrap-wlr-xdg-surface xdg-surface))))

(define-wlr-procedure (wlr-scene-layer-surface-v1-create parent layer-surface)
  ('* "wlr_scene_layer_surface_v1_create" (list '* '*))
  (wrap-wlr-scene-layer-surface-v1
   (% (unwrap-wlr-scene-tree parent)
      (unwrap-wlr-layer-surface-v1 layer-surface))))

(define-wlr-procedure (wlr-scene-layer-surface-v1-configure
                       scene-layer-surface full-area usable-area)
  (ffi:void "wlr_scene_layer_surface_v1_configure" (list '* '* '*))
  (% (unwrap-wlr-scene-layer-surface-v1 scene-layer-surface)
     (unwrap-wlr-box full-area)
     (unwrap-wlr-box usable-area)))
