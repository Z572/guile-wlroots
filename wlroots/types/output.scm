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
  #:use-module (wayland server protocol wayland)
  #:use-module (wayland server resource)
  #:use-module (wayland util)
  #:use-module (wayland server listener)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (wayland list)
  #:use-module (srfi srfi-26)
  #:use-module (wayland signal)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wrap-wlr-output
            unwrap-wlr-output
            wrap-wlr-output-event-damage
            unwrap-wlr-output-event-damage
            wrap-wlr-output-event-request-state
            unwrap-wlr-output-event-request-state
            wlr-output-init-render
            .modes
            <wlr-output-state>
            <wlr-output>
            wlr-output-create-global
            wlr-output-destroy-global
            wlr-output-preferred-mode
            wlr-output-set-mode
            wlr-output-set-custom-mode
            wlr-output-set-transform
            wlr-output-enable
            wlr-output-commit
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
            .allow-reconfiguration
            .backend
            .buffer
            .committed
            .commit-seq
            .current-mode
            .damage
            .data
            .description
            .display
            .enabled
            .flags
            .gamma-lut
            .gamma-lut-size
            .global
            .height
            .hostpot-x
            .hostpot-y
            .mode
            .mode-type
            .name
            .needs-frame
            .non-desktop
            .output
            .phys-height
            .phys-width
            .presented
            .refresh
            .resource
            .render-format
            .renderer
            .scale
            .seq
            .src-box
            .state
            .subpixel
            .transform
            .visible
            .width
            .when
            .x
            .y
            .allocator))

(define-wlr-types-class wlr-output-state ()
  (committed #:accessor .committed)
  (allow-reconfiguration #:accessor .allow-reconfiguration)
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
  (events (damage wrap-wlr-output-event-damage)
          (precommit wrap-wlr-output-event-precommit)
          (commit wrap-wlr-output-event-commit)
          (request-state wrap-wlr-output-event-request-state)
          (description wrap-wlr-output)
          (needs-frame wrap-wlr-output)
          (frame wrap-wlr-output)
          (destroy wrap-wlr-output))
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
  (modes        #:getter   .modes)
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

(define-wlr-types-class wlr-output-event-damage ()
  (output #:accessor .output)
  (damage #:accessor .damage)
  #:descriptor %wlr-output-event-damage-struct)

(define-wlr-types-class wlr-output-event-precommit ()
  (output #:accessor .output)
  (when #:accessor .when)
  (state #:accessor .state)
  #:descriptor %wlr-output-event-precommit-struct)

(define-wlr-types-class wlr-output-event-commit ()
  (output #:accessor .output)
  (when #:accessor .when)
  (state #:accessor .state)
  #:descriptor %wlr-output-event-commit-struct)

(define-wlr-types-class wlr-output-event-present ()
  (output #:accessor .output)
  (commit-seq #:accessor .commit-seq)
  (presented #:accessor .presented)
  (when #:accessor .when)
  (seq #:accessor .seq)
  (refresh #:accessor .refresh)
  (flags #:accessor .flags)
  #:descriptor %wlr-output-event-present-struct)


(define-wlr-types-class wlr-output-event-bind ()
  (output #:accessor .output)
  (resource #:accessor .resource)
  #:descriptor %wlr-output-event-bind-struct)
(define-wlr-types-class wlr-output-event-request-state ()
  (output #:accessor .output)
  (state #:accessor .state)
  #:descriptor %wlr-output-event-request-state-struct)

(define-wlr-types-class wlr-output-mode ()
  (width #:getter .width)
  (height #:getter .height)
  (refresh #:getter .refresh)
  (preferred? #:field-name 'preferred #:getter .preferred?)
  (picture-aspent-ratio #:getter .picture-aspent-ratio)
  #:descriptor %wlr-output-mode-struct)

(define-method (write (o <wlr-output-mode>) file)
  (let ((class (class-of o)))
    (begin
      (display "#<" file)
      (display (class-name class) file)
      (display #\space file)
      (display (cons (.width o) (.height o)) file)
      (display #\space file)
      (display (.refresh o) file)
      (display #\space file)
      (display (number->string (object-address o) 16) file)
      (display #\> file))))

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

    (cons (bytestructure-ref width) (bytestructure-ref height))))

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

(define-wlr-procedure (wlr-output-is-direct-scanout-allowed output)
  (ffi:int8 "wlr_output_is_direct_scanout_allowed" '(*))
  (not (zero? (% (unwrap-wlr-output output)))))

(define-wlr-procedure
  (wlr-output-lock-attach-render output lock)
  (ffi:void "wlr_output_lock_attach_render" (list '* ffi:int8))
  (% (unwrap-wlr-output output)  (if lock 1 0)))

(define-wlr-procedure
  (wlr-output-lock-software-cursors output lock)
  (ffi:void "wlr_output_lock_software_cursors" (list '* ffi:int8))
  (% (unwrap-wlr-output output) (if lock 1 0)))

(define-wlr-types-class wlr-output-cursor ()
  (output #:getter .output)
  (x #:accessor .x)
  (y #:accessor .y)
  (enabled #:accessor .enabled)
  (visible #:accessor .visible)
  (width #:accessor .width)
  (height #:accessor .height)
  (src-box #:getter .src-box)
  (transform #:getter .transform)
  (hostpot-x #:accessor .hostpot-x)
  (hostpot-y #:accessor .hostpot-y)
  #:descriptor %wlr-output-cursor-struct)

(define-wlr-procedure
  (wlr-output-cursor-create output)
  ('* "wlr_output_cursor_create" (list '*))
  (wrap-wlr-output-cursor (% (unwrap-wlr-output output) )))

(define-wlr-procedure
  (wlr-output-cursor-set-buffer cursor buffer hotspot-x hotspot-y)
  (ffi:int8 "wlr_output_cursor_set_buffer" (list '* '* ffi:int32 ffi:int32))
  (not (zero?
        (% (unwrap-wlr-output-cursor cursor)
           (unwrap-wlr-buffer buffer)
           hotspot-x
           hotspot-y))))

(define %wlr_output_state_init (wlr->procedure ffi:void "wlr_output_state_init" '(*)))

(define-method (initialize (object <wlr-output-state>) initargs)
  (let ((descriptor(.descriptor (class-of object))))
    (if (get-keyword #:%pointer initargs #f)
        (next-method)
        (let ((p (ffi:bytevector->pointer
                  (make-bytevector
                   (bytestructure-descriptor-size descriptor)))))
          (%wlr_output_state_init p)
          (next-method
           object (append
                   (list #:%pointer p)
                   initargs))))))

(define-wlr-procedure
  (wlr-output-cursor-move cursor x y)
  (ffi:int8 "wlr_output_cursor_move" (list '* ffi:double ffi:double))
  (not (zero? (% (unwrap-wlr-output-cursor cursor) x y))))

(define-wlr-procedure (wlr-output-cursor-destroy cursor)
  (ffi:void "wlr_output_cursor_destroy" (list '*))
  (% (unwrap-wlr-output-cursor cursor)))


(define-wlr-procedure (wlr-output-state-finish state)
  (ffi:void "wlr_output_state_finish" (list '*))
  (% (unwrap-wlr-output-state state)))

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

(define-wlr-procedure (wlr-output-transform-invert tr)
  (ffi:int32 "wlr_output_transform_invert" (list ffi:int32))
  (% (bs:enum->integer %wl-output-transform-enum tr)))

(define-wlr-procedure (wlr-output-transform-compose tr-a tr-b)
  (ffi:int32 "wlr_output_transform_compose" (list ffi:int32 ffi:int32))
  (% (bs:enum->integer %wl-output-transform-enum tr-a)
     (bs:enum->integer %wl-output-transform-enum tr-b)))
