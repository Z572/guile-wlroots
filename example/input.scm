(use-modules
 (wlroots types keyboard)
 (wayland protocol)
 (wayland)
 (xkbcommon xkbcommon)
 (util572 color)
 (oop goops)
 (wlroots))

;;; this a simple example, pass ESC exit, other key will toggle background
;;; color.

(define w-display (wl-display-create))
(define w-backend (wlr-backend-autocreate w-display))
(define w-renderer (wlr-renderer-autocreate w-backend))
(define w-allocator (wlr-allocator-autocreate w-backend w-renderer))
(define w-seat (wlr-seat-create w-display "seat0"))

(define black? (make-parameter #t))

(define output-frame-listener
  (make-wl-listener
   (lambda (listener data)
     (let ((output (wrap-wlr-output data)))
       (wlr-output-attach-render output #f)
       (call-with-renderer
        w-renderer (.width output) (.height output)
        (lambda (renderer . _)
          (wlr-renderer-clear
           renderer (make-rgba-color (if (black?) "#0000" "#ffff")))))
       (wlr-output-commit output)))))

(define w-backend-new-output-listener
  (make-wl-listener
   (lambda (listener data)
     (let* ((output (wrap-wlr-output data)))
       (display "I get new output!\n")
       (wlr-output-init-render
        output w-allocator w-renderer)
       (wl-signal-add (get-event-signal output 'frame) output-frame-listener)
       (wlr-output-commit output)))))

(define (add-seat-capabilitie seat o)
  (wlr-seat-set-capabilities seat
                             (logior (.capabilities seat)
                                     (bs:enum->integer
                                      %wl-seat-capability-enum o))))



(define (create-keyboard device)
  (display "new keyboard create!\n")
  (let* ((wl-kb (wlr-keyboard-from-input-device device))
         (context (xkb-context-new XKB_CONTEXT_NO_FLAGS))
         (xkb-rule-names (make <xkb-rule-names>))
         (keymap (xkb-keymap-new-from-names
                  context
                  xkb-rule-names
                  XKB_KEYMAP_COMPILE_NO_FLAGS)))
    (wlr-keyboard-set-keymap wl-kb keymap)
    (xkb-keymap-unref keymap)
    (xkb-context-unref context)
    (wlr-keyboard-set-repeat-info wl-kb 10 600)
    (wl-signal-add (get-event-signal wl-kb 'modifiers)
                   (make-wl-listener
                    (lambda (listener data)
                      (let* ((wlr-keyboard (wrap-wlr-keyboard data))
                             (modifiers(.modifiers wlr-keyboard)))
                        (wlr-seat-set-keyboard
                         w-seat wl-kb)
                        (wlr-seat-keyboard-notify-modifiers
                         w-seat modifiers)
                        (pk 'new-modifiers modifiers)))))
    (wl-signal-add (get-event-signal wl-kb 'key)
                   (make-wl-listener
                    (lambda (listener data)
                      (let* ((event (wrap-wlr-keyboard-key-event data)))
                        (if (= (.keycode event) 1)
                            (wl-display-terminate w-display)
                            (begin (wlr-seat-set-keyboard w-seat wl-kb)
                                   (wlr-seat-keyboard-notify-key
                                    w-seat
                                    (.time-msec event)
                                    (.keycode event)
                                    (if (eq? (.state event)
                                             'WL_KEYBOARD_KEY_STATE_PRESSED)
                                        1 0))
                                   (when (eq? (.state event)
                                              'WL_KEYBOARD_KEY_STATE_PRESSED)
                                     (black? (not (black?))))
                                   (pk 'new-key (.keycode event))))))))))

(define w-backend-new-input-listener
  (make-wl-listener
   (lambda (listener data)
     (let* ((device (wrap-wlr-input-device data)))
       (display "I get new input device!\n")
       (case (.type device)
         ((WLR_INPUT_DEVICE_KEYBOARD)
          (create-keyboard device)
          (add-seat-capabilitie
           w-seat
           'WL_SEAT_CAPABILITY_KEYBOARD)))       ))))

(wlr-renderer-init-wl-display w-renderer w-display)

(wl-signal-add (get-event-signal w-backend 'new-output)
               w-backend-new-output-listener)

(wl-signal-add (get-event-signal w-backend 'new-input)
               w-backend-new-input-listener)

(wlr-backend-start w-backend)

(setenv "WAYLAND_DISPLAY" (wl-display-add-socket-auto w-display))

(wl-display-run w-display)

(wl-display-destroy-clients w-display)
(wl-display-destroy w-display)
