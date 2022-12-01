(define-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module ((system foreign) #:select(pointer-address pointer? %null-pointer))
  #:use-module ((bytestructures guile) #:select(bytestructure?))
  #:export-syntax ( define-wlr-types-class
                    define-wlr-types-class-public)
  #:export (get-pointer
            get-event-signal))

(define-generic get-event-signal)

(define-class <wlr-type> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))

(define-method (= (f <wlr-type>) (l <wlr-type>))
  (= (.pointer f)
     (.pointer l)))

(define-generic get-pointer)
(define-syntax define-wlr-types-class
  (lambda (x)
    (syntax-case x ()
      ((_ name (supers ...) slots ...
          )
       (let ((symbol (syntax->datum #'name))
             (identifier (cut datum->syntax #'name <>)))
         (with-syntax ((rtd (identifier (symbol-append '< symbol '>)))
                       (wrap (identifier (symbol-append 'wrap- symbol )))
                       (unwrap (identifier (symbol-append 'unwrap- symbol)))
                       (is? (identifier (symbol-append symbol '?))))
           #`(begin
               (define-class rtd (supers ... <wlr-type>)
                 slots ...)
               (define wrap
                 (let ((ptr->obj (make-weak-value-hash-table 3000)))
                   (lambda (ptr)
                     (or (hash-ref ptr->obj ptr)
                         (let ((o (make rtd #:pointer ptr)))
                           (hash-set! ptr->obj ptr o)
                           o)))))
               (define (unwrap o)
                 (if o
                     (.pointer o)
                     %null-pointer))
               (define-method (get-pointer (o rtd))
                 (let ((u (unwrap o)))
                   (cond ((pointer? u) u)
                         ((bytestructure? u) (bytestructure->pointer u)))))
               (define (is? o) (is-a? o rtd))))))
      ((_ name)
       #'(define-wlr-types-class name ())))))

(define-syntax define-wlr-types-class-public
  (lambda (x)
    (syntax-case x ()
      ((_ name others ...)
       (let ((symbol (syntax->datum #'name))
             (identifier (cut datum->syntax #'name <>)))
         (with-syntax ((rtd (identifier (symbol-append '< symbol '>)))
                       (wrap (identifier (symbol-append 'wrap- symbol )))
                       (unwrap (identifier (symbol-append 'unwrap- symbol)))
                       (is? (identifier (symbol-append symbol '?))))
           #`(begin
               (define-wlr-types-class name others ...)
               (export wrap)
               (export unwrap)
               (export is? ))))))))
