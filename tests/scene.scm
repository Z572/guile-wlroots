(define-module (tests scene)
  #:use-module (wlroots types)
  #:use-module (wayland server listener)
  #:use-module (wayland signal)
  #:use-module (wayland server display)
  #:use-module (wlroots backend)
  #:use-module (wlroots backend headless)
  #:use-module (util572 color)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots time)
  #:use-module (oop goops)
  #:use-module (wlroots types scene))

(define test-display (make-parameter (wl-display-create)))
(define test-scene
  (make-parameter
   #f))
(define test-backend
  (make-parameter (wlr-headless-backend-create
                   (test-display))))
(define test-output
  (make-parameter #f))

(test-group "scene"
  (test-assert "wlr-scene-create"
    (and=> (wlr-scene-create)
           (lambda (x)
             (test-scene x)
             x)))
  (test-assert "get-slots"
    (map
     (lambda (slot)
       (cons (slot-definition-name slot)
             ((or (slot-definition-getter slot)
                  (slot-definition-accessor slot))
              (test-scene))))
     (class-slots <wlr-scene>)))
  (test-assert "wlr-scene-tree-create"
    (wlr-scene-tree? (wlr-scene-tree-create (.tree (test-scene)))))
  (test-assert "wlr-scene-rect-create"
    (wlr-scene-rect?
     (wlr-scene-rect-create (.tree (test-scene))
                            30 40
                            (make-rgba-color 0 0 0 0))))
  (wl-signal-add (get-event-signal (test-backend) 'new-output)
                 (make-wl-listener (lambda (listener output)
                                     (test-output output))))

  (let ((screen-output #f))
    (test-assert ""
      (begin (wlr-headless-add-output (test-backend) 1920 1080)
             (wlr-backend-start (test-backend))
             (let ((o (wlr-scene-output-create (test-scene)
                                               (test-output))))
               (set! screen-output o)
               (wlr-scene-output? o))))
    (test-eq "wlr-scene-get-scene-output"
      screen-output
      (wlr-scene-get-scene-output (test-scene) (test-output)))
    (test-assert ""
      (wlr-scene-output-set-position screen-output 30 40))

    (test-assert ""
      (wlr-scene-output-send-frame-done screen-output (make <timespec>)))))
