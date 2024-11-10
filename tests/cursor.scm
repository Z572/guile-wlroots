(define-module (tests cursor)
  #:use-module (wlroots types output-layout)
  #:use-module (wayland server display)
  #:use-module (wayland signal)
  #:use-module (wayland server listener)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots time)
  #:use-module (wlroots types)
  #:use-module (srfi srfi-64)
  #:use-module (oop goops)
  #:use-module (wlroots types cursor))
(define test-cursor (make-parameter #f))
(define test-output-layout (wlr-output-layout-create))
(test-group "cursor"
  (parameterize ((test-cursor (wlr-cursor-create)))
    (test-assert "wlr-cursor-create"
      (test-cursor))

    (test-assert "get-slots"
      (map
       (lambda (slot)
         (cons (slot-definition-name slot)
               ((or (slot-definition-getter slot)
                    (slot-definition-accessor slot))
                (test-cursor))))
       (class-slots <wlr-cursor>)))
    (test-assert 'wlr-cursor-attach-output-layout
      (wlr-cursor-attach-output-layout
       (test-cursor) test-output-layout))
    (test-equal "wlr-cursor-move"
      (cons (.x (test-cursor)) (.y (test-cursor)))
      (begin (wlr-cursor-move (test-cursor) #f 30.0 40.0)
             (cons (.x (test-cursor)) (.y (test-cursor)))))
    (test-assert "wlr-cursor-destroy"
      (wlr-cursor-destroy (test-cursor)))))
