(define-module (tests viewporter)
  #:use-module (wayland server display)
  #:use-module (wayland signal)
  #:use-module (wayland server listener)
  #:use-module (wlroots types)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots time)
  #:use-module (srfi srfi-64)
  #:use-module (oop goops)
  #:use-module (wlroots types viewporter))
(define test-display (wl-display-create))
(define test-viewporter (make-parameter (wlr-viewporter-create test-display)))
(test-group "viewporter"
  (test-assert "create" (test-viewporter))
  (test-assert "get-slots"
    (map
     (lambda (slot)
       (cons (slot-definition-name slot)
             ((or (slot-definition-getter slot)
                  (slot-definition-accessor slot))
              (test-viewporter))))
     (class-slots <wlr-viewporter>)))
  (let ((destroy? #f))
    (test-assert "destroy"
      (begin (wl-signal-add (get-event-signal (test-viewporter) 'destroy)
                            (make-wl-listener (lambda (listener output)
                                                (set! destroy? #t))))
             (wl-display-destroy test-display)
             destroy?))))
