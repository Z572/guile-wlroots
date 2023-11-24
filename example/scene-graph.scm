#!/usr/bin/env -S guile --no-auto-compile -e main
!#
(use-modules
 (ice-9 curried-definitions)
 (oop goops)
 (wayland server display)
 (wayland list)
 (wayland server listener)
 (wayland signal)
 (wlroots backend)
 (wlroots render allocator)
 (wlroots render renderer)
 (wlroots time)
 (wlroots types compositor)
 (wlroots types output)
 (wlroots types scene)
 (wlroots types surface)
 (wlroots types xdg-shell)
 (wlroots types))

(default-duplicate-binding-handler
  '(merge-generics replace warn-override-core warn last))

(define* (add-listen* obj symbol proc
                      #:key
                      (destroy-when obj)
                      (remove-when-destroy? #t))
  (let ((listener (make-wl-listener proc)))
    (wl-signal-add (get-event-signal obj symbol) listener)
    (when remove-when-destroy?
      (wl-signal-add
       (get-event-signal destroy-when 'destroy)
       (make-wl-listener
        (lambda _
          (wl-list-remove (.link listener))))))))

(define-class <server> ()
  (display #:getter server-display)
  (backend #:getter server-backend)
  (renderer #:getter server-renderer)
  (allocator #:getter server-allocator)
  (compositor #:getter server-compositor)
  (scene #:getter server-scene)
  (xdg-shell #:getter server-xdg-shell))

(define-class <monitor> ()
  (server #:getter monitor-server #:init-keyword #:server)
  (output #:getter monitor-output #:init-keyword #:output)
  (scene-output #:getter monitor-scene-output #:init-keyword #:scene-output))

(define-class <surface> ()
  (server #:getter .server #:init-keyword #:server)
  (surface #:getter .surface #:init-keyword #:surface)
  (scene-surface #:getter .scene-surface #:init-keyword #:scene-surface))

(define-method (initialize (object <server>) args)
  (next-method)
  (let* ((display (wl-display-create))
         (backend (wlr-backend-autocreate display))
         (renderer (wlr-renderer-autocreate backend))
         (allocator (wlr-allocator-autocreate backend renderer))
         (compositor (wlr-compositor-create display renderer))
         (scene (wlr-scene-create))
         (xdg-shell (wlr-xdg-shell-create display 5)))
    (wlr-renderer-init-wl-display renderer display)
    (slot-set! object 'display display)
    (slot-set! object 'backend backend)
    (slot-set! object 'renderer renderer)
    (slot-set! object 'allocator allocator)
    (slot-set! object 'compositor compositor)
    (slot-set! object 'scene scene)
    (slot-set! object 'xdg-shell xdg-shell)))

(define ((output-frame-notify m) listener data)
  (let ((scene-output (monitor-scene-output m)))
    (when (wlr-scene-output-commit scene-output)
      (let ((time (make <timespec>)))
        (clock-gettime 'CLOCK_MONOTONIC time)
        (wlr-scene-output-send-frame-done scene-output time)))))

(define ((new-output-notify server) listener data)
  (let* ((output (wrap-wlr-output data))
         (scene-output (wlr-scene-output-create (server-scene server) output))
         (m (make <monitor>
              #:server server
              #:output output
              #:scene-output scene-output)))
    (wlr-output-init-render
     output (server-allocator server)
     (server-renderer server))
    (add-listen* output 'frame (output-frame-notify m))
    (wlr-output-commit output)))

(define ((new-surface server) listener data)
  (let* ((wlr-surface (wrap-wlr-surface data))
         (scene-surface (wlr-scene-surface-create (.tree (server-scene server))
                                                  wlr-surface))
         (surface (make <surface>
                    #:server server
                    #:surface wlr-surface
                    #:scene-surface scene-surface)))
    (add-listen* wlr-surface 'destroy (surface-destroy-notify surface))
    (pk 'new-surface surface)))

(define ((surface-destroy-notify surface) listener data)
  (pk 'destroy-surface surface
      (.scene-surface surface)
      (.node (.buffer (.scene-surface surface)))))

(define-method (run! (obj <server>))
  (wlr-backend-start (server-backend obj))
  (setenv "WAYLAND_DISPLAY" (wl-display-add-socket-auto (server-display obj)))
  (wl-display-run (server-display obj)))

(define-method (cleanup! (obj <server>))
  (wl-display-destroy-clients (server-display obj))
  (wl-display-destroy (server-display obj)))

(define (main . args)
  (define server (make <server>))
  (unless (getenv "XDG_RUNTIME_DIR")
    (display "XDG_RUNTIME_DIR must be set.\n")
    (exit 1))
  (add-listen* (server-backend server)
               'new-output (new-output-notify server))
  (add-listen* (server-compositor server)
               'new-surface (new-surface server))

  (run! server)
  (cleanup! server))
