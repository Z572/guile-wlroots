(define-module (tests xdg-shell)
  #:use-module (wayland server display)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots time)
  #:use-module (srfi srfi-64)
  #:use-module (oop goops)
  #:use-module (wlroots types xdg-shell))
(define test-xdg-shell (make-parameter #f))
(test-group "xdg-shell"
  (test-assert "create"
    (and=> (wlr-xdg-shell-create (wl-display-create) 5)
           (lambda (x)
             (test-xdg-shell x)
             x)))
  (test-assert "get-slots"
    (map
     (lambda (slot)
       (cons (slot-definition-name slot)
             ((or (slot-definition-getter slot)
                  (slot-definition-accessor slot))
              (test-xdg-shell))))
     (class-slots <wlr-xdg-shell>))))
