(define-module (tests backend)
  #:use-module (wlroots backend libinput)
  #:use-module (srfi srfi-64)
  #:use-module (wlroots types)
  #:use-module (wayland signal)
  #:use-module (wayland server display)
  #:use-module (wlroots backend headless)
  #:use-module (wlroots backend multi)
  #:use-module (srfi srfi-71)
  #:use-module (wlroots time)
  #:use-module (oop goops)
  #:use-module (wayland server listener)
  #:use-module (wayland server event-loop)
  #:use-module (wlroots types xdg-shell)
  #:use-module (wlroots backend))
(define test-backend (make-parameter #f))
(define test-display (make-parameter (wl-display-create)))
(define test-session (make-parameter #f))

(test-group "backend"
  (parameterize ((test-backend (wlr-headless-backend-create
                                (test-display))))
    (let ((out #f))
      (test-assert "wlr-backend-is-headless"
        (wlr-backend-is-headless (test-backend)))
      (test-assert 'wlr-backend-start
        (wlr-backend-start (test-backend)))
      (test-assert "get-slots"
        (map
         (lambda (slot)
           (cons (slot-definition-name slot)
                 ((or (slot-definition-getter slot)
                      (slot-definition-accessor slot))
                  (test-backend))))
         (class-slots <wlr-backend>)))
      (test-assert "new-output signal"
        (wl-signal-add (get-event-signal (test-backend) 'new-output)
                       (make-wl-listener (lambda (listener output)
                                           (set! out output)))))

      (test-eq "wlr-headless-add-output"
        (wlr-headless-add-output (test-backend) 30 40)
        out)
      (test-assert "wlr-output-is-headless"
        (wlr-output-is-headless out))

      (test-assert 'wlr-backend-destroy
        (begin (wlr-backend-destroy (test-backend))
               (test-backend #f)
               #t))))
  (parameterize ((test-backend (wlr-multi-backend-create
                                (test-display))))
    (test-assert "wlr-backend-is-multi"
      (wlr-backend-is-multi (test-backend)))
    (test-assert "wlr-multi-is-empty"
      (wlr-multi-is-empty (test-backend)))
    (test-assert "wlr-multi-backend-add"
      (and (wlr-multi-backend-add (test-backend)
                                  (wlr-headless-backend-create
                                   (test-display)))
           (not (wlr-multi-is-empty (test-backend)))))
    (let ((out '()))
      (test-assert "wlr-multi-for-each-backend"
        (begin (wlr-multi-for-each-backend
                (test-backend)
                (lambda (x)
                  (set! out (cons x out))))
               (not (null? out))))
      (test-assert "wlr-multi-backend-remove"
        (wlr-multi-backend-remove (test-backend) (car out)))
      (wlr-backend-destroy (car out))
      (wlr-backend-destroy (test-backend))))
  )
