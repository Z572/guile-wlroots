(define-class <wlr> ())
(define-class <wlr-pointer> (<wlr>)
  (pointer #:accessor .pointer #:init-keyword #:pointer))

;; (define-method (= (a <class>) (b <class>)))
(define-method (= (f <wlr-pointer>) (l <wlr-pointer>))
  (and (eq (class-of f) (class-of l))
       (= (pointer-address (.pointer f))
          (pointer-address (.pointer l)))))
