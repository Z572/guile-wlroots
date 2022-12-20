(define-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland listener)
  #:use-module (wlroots render renderer)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots types)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots types output-layout)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (rnrs bytevectors)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
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
            wlr-scene-node-raise-to-top
            wlr-scene-node-lower-to-bottom
            %wlr-scene-struct
            %wlr-scene-rect-struct
            wlr-scene-node-set-enabled
            wlr-scene-node-reparent
            wlr-scene-rect-create
            wlr-scene-rect-set-size
            wlr-scene-rect-set-color
            wlr-scene-rect-node
            wlr-scene-xdg-surface-create
            wlr-scene-subsurface-tree-create
            .node))

(define %wlr-scene-node-state-struct
  (bs:struct `((link ,%wl-list)
               (children ,%wl-list)
               (enabled ,int8)
               (x ,int)
               (y ,int))))
(define %wlr-scene-node-struct
  (bs:struct `((type ,int)
               (parent ,(bs:pointer '*))
               (state ,%wlr-scene-node-state-struct)
               (events
                ,(bs:struct `((destroy ,%wl-signal-struct)))))))
(define %wlr-scene-tree-struct
  (bs:struct `((node ,%wlr-scene-node-struct))))

(define %wlr-scene-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (outputs ,%wl-list)
               (presentation ,(bs:pointer '*))
               (presentation-destroy ,%wl-listener)
               (peeding-buffers ,%wl-list))))

(define-wlr-types-class wlr-scene-node ()
  #:descriptor %wlr-scene-node-struct)
(define-wlr-types-class wlr-scene-tree ()
  #:descriptor %wlr-scene-tree-struct)

(define %wlr-scene-rect-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (width ,int)
               (height ,int)
               (color ,%color-struct))))
(define %wlr-scene-buffer-struct
  (bs:struct `((node ,%wlr-scene-node-struct)
               (buffer ,(bs:pointer '*))
               (texture  ,(bs:pointer '*))
               (src-box ,%wlr-fbox-struct)
               (dst-width ,int)
               (dst-height ,int)
               (transform ,int) ;; enum wl_output_transform
               (pending-link ,%wl-list))))
(define-wlr-types-class wlr-scene-buffer ()
  (node #:accessor .node
        #:allocation
        #:virtual
        #:slot-set! (lambda (o new-val)
                      (bytestructure-set! (pointer->bytestructure
                                           (get-pointer o) %wlr-scene-buffer-struct)
                                          'node
                                          (unwrap-wlr-scene-buffer new-val)))
        #:slot-ref (lambda (o)
                     (wrap-wlr-scene-node
                      (bytestructure->pointer (bytestructure-ref (pointer->bytestructure
                                                                  (get-pointer o) %wlr-scene-buffer-struct)
                                                                 'node)))))
  #:descriptor %wlr-scene-buffer-struct)

(define-wlr-types-class wlr-scene-rect ()
  (node #:accessor .node
        #:allocation
        #:virtual
        #:slot-set! (lambda (o new-val)
                      (bytestructure-set! (pointer->bytestructure
                                           (get-pointer o) %wlr-scene-rect-struct)
                                          'node
                                          (unwrap-wlr-scene-node new-val)))
        #:slot-ref (lambda (o)
                     (wrap-wlr-scene-node
                      (bytestructure->pointer (bytestructure-ref (pointer->bytestructure
                                                                  (get-pointer o) %wlr-scene-rect-struct)
                                                                 'node)))))
  #:descriptor %wlr-scene-rect-struct)
(define wlr-scene-rect-node .node)
(define-wlr-types-class wlr-scene ()
  (node #:allocation #:virtual
        #:accessor .node
        #:slot-ref
        (lambda (a)
          (wrap-wlr-scene-node
           (bytestructure->pointer
            (bytestructure-ref (get-bytestructure a) 'node))))
        #:slot-set!
        (lambda (a new-val)
          (bytestructure-set! (get-bytestructure a) 'node (unwrap-wlr-scene-node new-val))))
  #:descriptor %wlr-scene-struct)

(define-method (.node (o <wlr-scene-tree>))
  (wrap-wlr-scene-node
   (bytestructure->pointer
    (bytestructure-ref
     (pointer->bytestructure
      (unwrap-wlr-scene-tree o)
      %wlr-scene-tree-struct)
     'node))))

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
          (wrap-wlr-scene-node values)
          `(,(ref-double-pointer nx) .
            ,(ref-double-pointer ny))))))

(define-wlr-procedure (wlr-scene-node-raise-to-top node)
  (ffi:void "wlr_scene_node_raise_to_top" '(*))
  (% (unwrap-wlr-scene-node node)))

(define-wlr-procedure (wlr-scene-node-lower-to-bottom node)
  (ffi:void "wlr_scene_node_lower_to_bottom" '(*))
  (% (unwrap-wlr-scene-node node)))

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

(define-wlr-procedure (wlr-scene-subsurface-tree-create parent surface)
  ('* "wlr_scene_subsurface_tree_create" '(* *))
  (wrap-wlr-scene-node (% (unwrap-wlr-scene-node parent)
                          (unwrap-wlr-surface surface))))

(define-wlr-procedure (wlr-scene-xdg-surface-create parent xdg-surface)
  ('* "wlr_scene_xdg_surface_create" '(* *))
  (wrap-wlr-scene-node (% (unwrap-wlr-scene-node parent)
                          (unwrap-wlr-xdg-surface xdg-surface))))
