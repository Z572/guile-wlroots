#!/usr/bin/env -S guile --no-auto-compile -e main
!#
(use-modules
 (wayland server display)
 (wayland signal)
 (wayland server listener)
 (util572 color)
 (wlroots))

;;; this a simple example, can not input, display cursor , display client.
;;; please don't try at tty. use x11 or wayland backend. it just display a
;;; red windows.

(define w-display (wl-display-create))
(define w-backend (wlr-backend-autocreate w-display))
(define w-renderer (wlr-renderer-autocreate w-backend))
(define w-allocator (wlr-allocator-autocreate w-backend w-renderer))

(define output-frame-listener
  (make-wl-listener
   (lambda (listener data)
     (let ((output (wrap-wlr-output data)))
       (wlr-output-attach-render output #f)
       (call-with-renderer
        w-renderer
        (.width output) (.height output)
        (lambda (renderer . _)
          (wlr-renderer-clear renderer (make-rgba-color "#f000"))))
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

(define (main . _)
  (wlr-renderer-init-wl-display w-renderer w-display)

  (wl-signal-add (get-event-signal w-backend 'new-output)
                 w-backend-new-output-listener)

  (wlr-backend-start w-backend)

  (setenv "WAYLAND_DISPLAY" (wl-display-add-socket-auto w-display))

  (wl-display-run w-display)

  (wl-display-destroy-clients w-display)
  (wl-display-destroy w-display))
