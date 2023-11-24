#!/usr/bin/env -S guile --no-auto-compile -e main
!#

(use-modules (ice-9 curried-definitions)
             (srfi srfi-71)
             (ice-9 format)
             (oop goops)
             ((bytestructures guile) #:select (bs:struct bs:pointer))
             ((util572 color) #:select (make-rgba-color))
             ((wayland server display) #:select (wl-display-create
                                                 wl-display-add-socket-auto
                                                 wl-display-run
                                                 wl-display-destroy-clients
                                                 wl-display-destroy))
             ((wayland list) #:select (make-wl-list
                                       wl-list-remove
                                       wl-list-insert))
             ((wayland server listener) #:select (make-wl-listener .link))
             ((wayland signal) #:select (wl-signal-add))
             ((wlroots backend) #:select (wlr-backend-autocreate
                                          wlr-backend-start))
             ((wlroots render allocator) #:select (wlr-allocator-autocreate))
             ((wlroots render renderer)
              #:select (wlr-renderer-autocreate
                        wlr-renderer-init-wl-display
                        wlr-renderer-clear
                        wlr-render-texture-with-matrix
                        call-with-renderer))
             ((wlroots time) #:select (<timespec> clock-gettime))
             ((wlroots types) #:select (get-event-signal
                                        define-bytestructure-class
                                        %timespec-struct
                                        %wlr-output-struct
                                        %wlr-renderer-struct))
             ((wlroots types compositor) #:select
              (wlr-compositor-create
               wlr-surface-for-each-surface
               wlr-surface-get-texture
               wlr-surface-send-frame-done))
             ((wlroots types fullscreen-shell)
              #:select (wlr-fullscreen-shell-v1-create))
             ((wlroots types matrix) #:select (wlr-matrix-project-box))
             (wlroots types output)
             ((wlroots types output-layout)
              #:select (wlr-output-layout-create
                        wlr-output-layout-add-auto))
             ((wlroots util box) #:select (make-wlr-box))
             ((wlroots util log) #:select (wlr-log-init)))

(default-duplicate-binding-handler
  '(merge-accessors merge-generics replace warn-override-core warn last))

(define* (add-listen* obj symbol proc
                      #:key (destroy-when obj)
                      (remove-when-destroy? #t))
  (let ((listener (make-wl-listener proc)))
    (wl-signal-add (get-event-signal obj symbol) listener)

    (when remove-when-destroy?
      (wl-signal-add (get-event-signal destroy-when 'destroy)
                     (make-wl-listener (lambda _
                                         (wl-list-remove (.link listener))))))))

(define-bytestructure-class <render-data> ()
  (bs:struct `((output   ,(bs:pointer %wlr-output-struct))
               (renderer ,(bs:pointer %wlr-renderer-struct))
               (when     ,(bs:pointer %timespec-struct))))
  wrap-render-data unwrap-render-data render-data?
  (output   #:getter get-render-data-output   #:init-keyword #:output)
  (renderer #:getter get-render-data-renderer #:init-keyword #:pass)
  (when     #:getter get-render-data-when     #:init-keyword #:when))

(define-class <fullscreen-output> ()
  (link    #:getter get-output-link    #:init-keyword #:link)
  (server  #:getter get-output-server  #:init-keyword #:server)
  (output  #:getter get-output-output  #:init-keyword #:output)
  (surface #:getter get-output-surface #:init-keyword #:surface
           #:setter set-output-surface))

(define (render-surface surface sx sy data)
  (let* ((rdata (wrap-render-data data))
         (output (get-render-data-output data))
         (texture (wlr-surface-get-texture surface)))
    (when texture
      (let* ((box (make-wlr-box
                   (* sx (.scale output))
                   (* sy (.scale output))
                   (* (.width  (.current surface)) (.scale output))
                   (* (.height (.current surface)) (.scale output))))
             (transform (wlr-output-transform-invert (.transform (.current surface))))
             (matrix (wlr-matrix-project-box box transform 0 (.transform output))))
        (wlr-render-texture-with-matrix (get-render-data-renderer data) texture matrix 1)
        (wlr-surface-send-frame-done surface (get-render-data-when data))))))

(define ((output-handle-frame fullscreen-output) listener data)
  (let ((renderer (get-server-renderer (get-output-server fullscreen-output)))
        (width+height (wlr-output-effective-resolution (get-output-output fullscreen-output)))
        (_ now (clock-gettime 'CLOCK_MONOTONIC)))
    (when (wlr-output-attach-render (get-output-output fullscreen-output))
      (call-with-renderer
       renderer width+height

       (lambda (renderer width height)
         (let* ((->16as10 (lambda (n) (inexact->exact (round (* 255 n)))))
                (color (make-rgba-color (->16as10 0.3)
                                        (->16as10 0.3)
                                        (->16as10 0.3)
                                        (->16as10 1.0))))
           (wlr-renderer-clear renderer color))
         (when (get-output-surface fullscreen-output)
           (let ((rdata (make <render-data>
                          #:output   (get-output-output fullscreen-output)
                          #:renderer renderer
                          #:when     now)))
             (wlr-surface-for-each-surface
              (get-output-surface fullscreen-output)
              render-surface rdata)))))

      (wlr-output-commit (get-output-output fullscreen-output)))))

(define ((output-handle-surface-destroy fullscreen-output) listener data)
  (output-set-surface fullscreen-output #f))

(define (output-set-surface fullscreen-output surface)
  (unless (equal? (get-output-surface surface) surface)
    (when (get-output-surface surface)
      (set-output-surface fullscreen-output #f))
    (when surface
      (add-listen* surface 'destroy
                   (output-handle-surface-destroy fullscreen-output))
      (set-output-surface surface))
    (format #t "Presenting surface ~p on output ~s"
            surface  (.name (get-output-output fullscreen-output)))))

(define-class <fullscreen-server> ()
  (display          #:getter get-server-display)
  (backend          #:getter get-server-backend)
  (renderer         #:getter get-server-renderer)
  (allocator        #:getter get-server-allocator)
  (fullscreen-shell #:getter get-server-fullscreen-shell)
  (output-layout    #:getter get-server-output-layout)
  (outputs          #:getter get-server-outputs))

(define ((server-handle-new-output server) listener data)
  (let* ((output (wrap-wlr-output data))
         (fullscreen-output (make <fullscreen-output>
                              #:link (make-wl-list)
                              #:server  server
                              #:output  output
                              #:surface #f)))
    (wlr-output-init-render output
                            (get-server-allocator server)
                            (get-server-renderer server))

    (add-listen* output 'frame
                 (output-handle-frame fullscreen-output))

    (wl-list-insert (get-server-outputs server)
                    (get-output-link fullscreen-output))

    (wlr-output-layout-add-auto (get-server-output-layout server)
                                (get-output-output fullscreen-output))

    (wlr-output-create-global output)

    (define state (make <wlr-output-state>))
    (wlr-output-state-set-enabled state #t)
    (let ((mode (wlr-output-preferred-mode output)))
      (when mode
        (wlr-output-state-set-mode state mode)))

    (wlr-output-commit-state output state)
    (wlr-output-state-finish state)))

(define ((server-handle-present-surface server) listener data)
  (let ((event (wrap-wlr-fullscreen-shell-v1-present-surface-event data)))
    (wl-list-for-each (lambda (obj wl-lst)
                        (let ((out-put            (.output event))
                              (fullscreen-out-put (get-output-output obj)))
                          (when (or (not out-put)
                                    (equal? out-put fullscreen-out-put))
                            (output-set-surface fullscreen-out-put
                                                (.surface event)))))
                      (get-server-outputs server)
                      <fullscreen-output>
                      'link)))


(define-method (initialize (object <fullscreen-server>) args)
  (next-method)
  (let* ((display (wl-display-create))
         (backend (wlr-backend-autocreate   display))
         (renderer (wlr-renderer-autocreate  backend))
         (allocator (wlr-allocator-autocreate backend renderer))
         (compositor (wlr-compositor-create display 5 renderer))
         (layout (wlr-output-layout-create))
         (outputs (make-wl-list))
         (shell (wlr-fullscreen-shell-v1-create display)))
    (wlr-renderer-init-wl-display renderer display)
    (slot-set! object 'display          display)
    (slot-set! object 'backend          backend)
    (slot-set! object 'renderer         renderer)
    (slot-set! object 'allocator        allocator)
    (slot-set! object 'fullscreen-shell shell)
    (slot-set! object 'output-layout    layout)
    (slot-set! object 'outputs          outputs)))



(define-method (run! (obj <fullscreen-server>))
  (unless (wlr-backend-start (get-server-backend obj))
    (wl-display-destroy (get-server-display obj))
    (exit 1))

  (setenv "WAYLAND_DISPLAY"
          (wl-display-add-socket-auto (get-server-display obj)))

  (wl-display-run (get-server-display obj)))

(define-method (cleanup! (obj <fullscreen-server>))
  (wl-display-destroy-clients (get-server-display obj))
  (wl-display-destroy (get-server-display obj)))

(define (main . args)
  (define server (make <fullscreen-server>))
  (wlr-log-init 'debug)

  (unless (getenv "XDG_RUNTIME_DIR")
    (display "XDG_RUNTIME_DIR must be set.")
    (newline)
    (exit 1))
  (add-listen* (get-server-backend server) 'new-output
               (server-handle-new-output server))
  (add-listen* (get-server-fullscreen-shell server) 'present-surface
               (server-handle-present-surface server))

  (run! server)
  (cleanup! server))
