(define-module (wlroots types output)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots render allocator)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots backend)
  #:use-module (wlroots util addon)
  #:use-module (wlroots types)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots types buffer)
  #:use-module (wlroots utils)
  #:use-module (wayland protocol)
  #:use-module (wayland resource)
  #:use-module (wayland util)
  #:use-module (wayland listener)
  #:use-module (oop goops)
  #:use-module (wayland list)
  #:use-module (srfi srfi-26)
  #:use-module (wayland signal)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wrap-wlr-output
            unwrap-wlr-output
            wlr-output-init-render
            .modes
            <wlr-output>
            wlr-output-create-global
            wlr-output-destroy-global
            wlr-output-preferred-mode
            wlr-output-set-mode
            wlr-output-set-custom-mode
            wlr-output-set-transform
            wlr-output-enable
            wlr-output-commit
            wlr-output-modes
            wlr-output-enable-adaptive-sync
            wlr-output-set-transform
            wlr-output-set-name
            wlr-output-set-description
            wlr-output-set-scale
            wlr-output-schedule-done

            <wlr-output-cursor>
            wrap-wlr-output-cursor
            unwrap-wlr-output-cursor
            .adaptive-sync-enabled
            .adaptive-sync-status
            .allow-artifacts
            .backend
            .buffer
            .committed
            .current-mode
            .damage
            .data
            .description
            .display
            .enabled
            .gamma-lut
            .gamma-lut-size
            .global
            .height
            .mode
            .mode-type
            .name
            .needs-frame
            .non-desktop
            .phys-height
            .phys-width
            .refresh
            .render-format
            .renderer
            .scale
            .subpixel
            .transform
            .surface
            .width
            .allocator))

(define-wlr-types-class wlr-output-state ()
  (committed #:accessor .committed)
  (allow-artifacts #:accessor .allow-artifacts)
  (damage #:accessor .damage)
  (enabled #:accessor .enabled)
  (scale #:accessor .scale)
  (transform #:accessor .transform)
  (adaptive-sync-enabled #:accessor .adaptive-sync-enabled)
  (render-format #:accessor .render-format)
  (subpixel #:accessor .subpixel)
  (buffer #:accessor .buffer)
  (mode-type #:accessor .mode-type)
  (mode #:accessor .mode)
  (gamma-lut #:accessor .gamma-lut)
  (gamma-lut-size #:accessor .gamma-lut-size)
  #:descriptor %wlr-output-state-struct)

(define-wlr-types-class wlr-output ()
  (backend      #:accessor .backend)
  (display      #:accessor .display)
  (global       #:accessor .global)
  (resources    #:accessor .resources )
  (name         #:accessor .name)
  (description  #:accessor .description)
  (make         #:accessor .make)
  (model        #:accessor .model)
  (serial       #:accessor .serial)
  (phys-width   #:accessor .phys-width)
  (phys-height  #:accessor .phys-height)
  (current-mode #:accessor .current-mode)
  (width        #:accessor .width)
  (height       #:accessor .height)
  (refresh      #:accessor .refresh)
  (enabled      #:accessor .enabled)
  (scale        #:accessor .scale)
  (subpixel     #:accessor .subpixel)
  (transform    #:accessor .transform)
  (adaptive-sync-status #:accessor .adaptive-sync-status)
  (render-format #:accessor .render-format)
  (needs-frame #:accessor .needs-frame)
  (commit-seq #:accessor .commit-seq)
  (non-desktop  #:accessor .non-desktop)
  (allocator #:accessor .allocator)
  (renderer #:accessor .renderer)
  (data         #:accessor .data)
  #:descriptor %wlr-output-struct)

(eval-when (expand load eval)
  (load-extension "libguile-wlroots" "scm_init_wlr_output"))

(define-wlr-types-class wlr-output-mode ()
  #:descriptor %wlr-output-mode-struct)

(define (wlr-output-modes o)
  (wrap-wl-list (%wlr-output-modes o)))

(define .modes wlr-output-modes)

(define-wlr-procedure (wlr-output-enable output enable)
  (ffi:void "wlr_output_enable" (list '* ffi:int))
  (% (unwrap-wlr-output output) (if enable 1 0)))

(define-wlr-procedure (wlr-output-create-global output)
  (ffi:void "wlr_output_create_global" (list '*))
  (% (unwrap-wlr-output output)))

(define-wlr-procedure (wlr-output-destroy-global output)
  (ffi:void "wlr_output_destroy_global" (list '*))
  (% (unwrap-wlr-output output)))

(define-wlr-procedure (wlr-output-init-render output allocator renderer)
  (ffi:int8 "wlr_output_init_render" '(* * *))
  (not (zero? (% (unwrap-wlr-output output)
                 (unwrap-wlr-allocator allocator)
                 (unwrap-wlr-renderer renderer)))))
(define-wlr-procedure (wlr-output-preferred-mode output)
  ('* "wlr_output_preferred_mode" '(*))
  (wrap-wlr-output-mode (% (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-output-set-mode output mode)
  (ffi:void "wlr_output_set_mode" '(* *) )
  (% (unwrap-wlr-output output) (unwrap-wlr-output-mode mode)))

(define-wlr-procedure (wlr-output-set-custom-mode output width height refresh)
  (ffi:void "wlr_output_set_custom_mode"(list '* ffi:int32 ffi:int32 ffi:int32))
  (% (unwrap-wlr-output output)  width height refresh))
(define-wlr-procedure (wlr-output-set-transform output transform)
  (ffi:void "wlr_output_set_transform" (list '* ffi:int32))
  (% (unwrap-wlr-output output)
     (bs:enum->integer %wl-output-transform-enum
                       transform)))

(define-wlr-procedure (wlr-output-enable-adaptive-sync output enabled)
  (ffi:void "wlr_output_enable_adaptive_sync" (list '* ffi:int))
  (% (unwrap-wlr-output output) (if enabled 1 0)))

(define-wlr-procedure (wlr-output-set-render-format output format)
  (ffi:void "wlr_output_set_render_format" (list '* ffi:uint32))
  (% (unwrap-wlr-output output)  format))
(define-wlr-procedure (wlr-output-set-scale output scale)
  (ffi:void "wlr_output_set_scale" (list '* ffi:float))
  (% (unwrap-wlr-output output)  scale))
(define-wlr-procedure (wlr-output-set-subpixel output subpixel)
  (ffi:void "wlr_output_set_subpixel" (list '* ffi:int32))
  (% (unwrap-wlr-output output)
     (bs:enum->integer %wl-output-subpixel-enum subpixel)))
(define-wlr-procedure (wlr-output-set-name output name)
  (ffi:void "wlr_output_set_name" (list '* '*))
  (% (unwrap-wlr-output output)  (ffi:string->pointer name)))
(define-wlr-procedure
  (wlr-output-set-description output desc)
  (ffi:void "wlr_output_set_description" (list '* '*))
  (% (unwrap-wlr-output output)  (ffi:string->pointer desc)))
(define-wlr-procedure (wlr-output-schedule-done output)
  (ffi:void "wlr_output_schedule_done" (list '*))
  (% (unwrap-wlr-output output)))

(define-wlr-procedure (wlr-output-destroy output)
  (ffi:void "wlr_output_destroy" (list '*))
  (% (unwrap-wlr-output output) ))

(define-wlr-procedure (wlr-output-effective-resolution output)
  (ffi:void "wlr_output_effective_resolution" '(* * *))
  (let ((width (bytestructure int))
        (height (bytestructure int)))
    (% (unwrap-wlr-output output)
       (bytestructure->pointer width)
       (bytestructure->pointer height))

    (values (bytestructure-ref width) (bytestructure-ref height))))

(define-wlr-procedure (wlr-output-attach-render output #:optional (buffer_age #f))
  (ffi:int8 "wlr_output_attach_render" (list '* '*))
  (not (zero? (% (unwrap-wlr-output output)  (or buffer_age ffi:%null-pointer)))))

(define-wlr-procedure (wlr-output-attach-buffer output buffer)
  (ffi:void "wlr_output_attach_buffer" (list '* '*))
  (% (unwrap-wlr-output output)  (unwrap-wlr-buffer buffer)))

(define-wlr-procedure (wlr-output-preferred-read-format output)
  (ffi:uint32 "wlr_output_preferred_read_format" (list '*))
  (% (unwrap-wlr-output output) ))

(define-wlr-procedure (wlr-output-test output)
  (ffi:int8 "wlr_output_test" (list '*))
  (not (zero? (% (unwrap-wlr-output output) ))))

(define-wlr-procedure (wlr-output-commit output)
  (ffi:int8 "wlr_output_commit" '(*))
  (not (zero? (% (unwrap-wlr-output output)))))

(define-wlr-procedure (wlr-output-rollback output)
  (ffi:void "wlr_output_rollback" (list '*))
  (% (unwrap-wlr-output output) ))

(define-wlr-procedure (wlr-output-test-state output state)
  (ffi:int8 "wlr_output_test_state" (list '* '*))
  (not (zero? (% (unwrap-wlr-output output) (unwrap-wlr-output-state state)))))

(define-wlr-procedure (wlr-output-commit-state output state)
  (ffi:int8 "wlr_output_commit_state" (list '* '*))
  (not (zero? (% (unwrap-wlr-output output) (unwrap-wlr-output-state state)))))

(define-wlr-procedure
  (wlr-output-schedule-frame output)
  (ffi:void "wlr_output_schedule_frame" (list '*))
  (% (unwrap-wlr-output output) ))

(define-wlr-procedure
  (wlr-output-get-gamma-size output)
  (ffi:size_t "wlr_output_get_gamma_size" (list '*))
  (% (unwrap-wlr-output output) ))

(define-wlr-procedure (wlr-output-from-resource resource)
  ('* "wlr_output_from_resource" (list '*))
  (wrap-wlr-output (% (unwrap-wl-resource resource))))

(define-wlr-procedure
  (wlr-output-lock-attach-render output lock)
  (ffi:void "wlr_output_lock_attach_render" (list '* ffi:int8))
  (% (unwrap-wlr-output output)  (if lock 1 0)))

(define-wlr-procedure
  (wlr-output-lock-software-cursors output lock)
  (ffi:void "wlr_output_lock_software_cursors" (list '* ffi:int8))
  (% (unwrap-wlr-output output) (if lock 1 0)))

(define-wlr-types-class wlr-output-cursor ()
  (x #:accessor .x)
  (y #:accessor .y)
  (enabled #:accessor .enabled)
  (visible #:accessor .visible)
  (width #:accessor .width)
  (height #:accessor .height)
  (hostpot-x #:accessor .hostpot-x)
  (hostpot-y #:accessor .hostpot-y)
  (surface #:accessor .surface)
  #:descriptor %wlr-output-cursor-struct)

(define-wlr-procedure
  (wlr-output-cursor-create output)
  ('* "wlr_output_cursor_create" (list '*))
  (wrap-wlr-output-cursor (% (unwrap-wlr-output output) )))
(define-wlr-procedure
  (wlr-output-cursor-set-image
   cursor
   pixels
   stride
   width
   height
   hotspot-x
   hotspot-y)
  (ffi:int8
   "wlr_output_cursor_set_image"
   (list '* '* ffi:int32 ffi:uint32 ffi:uint32 ffi:int32 ffi:int32))
  (not (zero?
        (% (unwrap-wlr-output-cursor cursor)
           pixels
           stride
           width
           height
           hotspot-x
           hotspot-y))))
(define-wlr-procedure
  (wlr-output-cursor-set-surface cursor surface hotspot-x hotspot-y)
  (ffi:void "wlr_output_cursor_set_surface" (list '* '* ffi:int32 ffi:int32))
  (% (unwrap-wlr-output-cursor cursor)
     (unwrap-wlr-surface surface)
     hotspot-x
     hotspot-y))
(define-wlr-procedure
  (wlr-output-cursor-set-buffer cursor buffer hotspot-x hotspot-y)
  (ffi:int8 "wlr_output_cursor_set_buffer" (list '* '* ffi:int32 ffi:int32))
  (not (zero?
        (% (unwrap-wlr-output-cursor cursor)
           (unwrap-wlr-buffer buffer)
           hotspot-x
           hotspot-y))))

(define-wlr-procedure
  (wlr-output-cursor-move cursor x y)
  (ffi:int8 "wlr_output_cursor_move" (list '* ffi:double ffi:double))
  (not (zero? (% (unwrap-wlr-output-cursor cursor) x y))))

(define-wlr-procedure (wlr-output-cursor-destroy cursor)
  (ffi:void "wlr_output_cursor_destroy" (list '*))
  (% (unwrap-wlr-output-cursor cursor)))

(define-wlr-procedure (wlr-output-state-set-enabled state enabled)
  (ffi:void "wlr_output_state_set_enabled" (list '* ffi:int8))
  (% (unwrap-wlr-output-state state) (if enabled 1 0)))

(define-wlr-procedure (wlr-output-state-set-mode state mode)
  (ffi:void "wlr_output_state_set_mode" (list '* '*))
  (% (unwrap-wlr-output-state state) (unwrap-wlr-output-mode mode)))

(define-wlr-procedure (wlr-output-state-set-scale state scale)
  (ffi:void "wlr_output_state_set_scale" (list '* ffi:float))
  (% (unwrap-wlr-output-state state) scale))

(define-wlr-procedure (wlr-output-state-set-transform state transform)
  (ffi:void "wlr_output_state_set_transform" (list '* ffi:int32))
  (% (unwrap-wlr-output-state state)
     (bs:enum->integer %wl-output-transform-enum transform)))

(define-wlr-procedure (wlr-output-state-set-adaptive-sync-enabled state enabled)
  (ffi:void "wlr_output_state_set_adaptive_sync_enabled" (list '* ffi:int8))
  (% (unwrap-wlr-output-state state) (if enabled 1 0)))

(define-wlr-procedure (wlr-output-state-set-subpixel state subpixel)
  (ffi:void "wlr_output_state_set_subpixel" (list '* ffi:int32))
  (% (unwrap-wlr-output-state state)
     (bs:enum->integer %wl-output-subpixel-enum subpixel)))
