(define-module (tests output)
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
  #:use-module (wlroots types output))

(define test-display (make-parameter (wl-display-create)))
(define test-backend
  (make-parameter (wlr-headless-backend-create
                   (test-display))))
(define test-output
  (make-parameter #f))

(test-group "output"
  (wl-signal-add (get-event-signal (test-backend) 'new-output)
                 (make-wl-listener (lambda (listener output)
                                     (test-output output))))
  (wlr-backend-start (test-backend))
  (wlr-headless-add-output (test-backend) 1920 1080)
  (test-assert "get-slots"
    (map
     (lambda (slot)
       (cons (slot-definition-name slot)
             ((or (slot-definition-getter slot)
                  (slot-definition-accessor slot))
              (test-output))))
     (class-slots <wlr-output>)))
  (test-equal "wlr-output-set-name"
    "hello!"
    (begin (wlr-output-set-name (test-output) "hello!")
           (.name (test-output))))
  (test-equal "wlr-output-set-description"
    "description"
    (begin (wlr-output-set-description (test-output) "description")
           (.description (test-output))))
  (let ((cursor #f))
    (test-assert "wlr-output-cursor-create"
      (let ((o (wlr-output-cursor-create (test-output))))
        (set! cursor o)
        o))
    (test-equal "wlr-output-cursor-move"
      (list 30.0 40.1)
      (begin (wlr-output-cursor-move cursor 30.0 40.1)
             (list (.x cursor) (.y cursor))))
    (test-assert "get-slots <wlr-output-cursor>"
      (map
       (lambda (slot)
         (cons (slot-definition-name slot)
               ((or (slot-definition-getter slot)
                    (slot-definition-accessor slot))
                cursor)))
       (class-slots <wlr-output-cursor>)))
    (test-eq (test-output) (.output cursor) )
    (test-assert "wlr-output-cursor-destroy"
      (wlr-output-cursor-destroy cursor)))
  (let ((state (make <wlr-output-state>)))
    (test-assert "wlr-output-state-set-enabled"
      (begin (wlr-output-state-set-enabled state #t)
             (.enabled state)))
    (test-equal "wlr-output-state-set-scale"
      2.0
      (begin (wlr-output-state-set-scale state 2.0)
             (.scale state)))
    (test-assert "wlr-output-commit-state"
      (begin (and (wlr-output-commit-state (test-output) state)
                  (.committed state))))
    (test-assert "wlr-output-state-set-adaptive-sync-enabled"
      (begin (wlr-output-state-set-adaptive-sync-enabled state #t)
             (.adaptive-sync-enabled state)))
    (test-assert "wlr-output-state-finish"
      (wlr-output-state-finish state))))
