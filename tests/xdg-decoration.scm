(define-module (tests xdg-decoration)
  #:use-module (wayland server display)
  #:use-module (wayland signal)
  #:use-module (wayland server listener)
  #:use-module (wlroots types)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots time)
  #:use-module (srfi srfi-64)
  #:use-module (oop goops)
  #:use-module (wlroots types xdg-decoration))
(define test-display (wl-display-create))
(define test (make-parameter (wlr-xdg-decoration-manager-v1-create test-display)))
(test-group "xdg-decoration"
  (test-assert "create" (test))
  (test-assert "get-slots"
    (map
     (lambda (slot)
       (cons (slot-definition-name slot)
             ((or (slot-definition-getter slot)
                  (slot-definition-accessor slot))
              (test))))
     (class-slots <wlr-xdg-decoration-manager-v1>)))
  (test-assert "wlr-xdg-toplevel-decoration-v1-set-mode"
    (wlr-xdg-toplevel-decoration-v1-set-mode
     (test)
     'WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE))
  (let ((destroy? #f))
    (test-assert "destroy"
      (begin (wl-signal-add (get-event-signal (test) 'destroy)
                            (make-wl-listener (lambda (listener output)
                                                (set! destroy? #t))))
             (wl-display-destroy test-display)
             destroy?))))
