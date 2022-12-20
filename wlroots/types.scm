(define-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-2)
  #:use-module (wayland signal)
  #:use-module ((system foreign) #:select(pointer-address pointer? %null-pointer))
  #:use-module (bytestructures guile)
  #:export-syntax ( define-wlr-types-class
                    define-wlr-types-class-public)
  #:export (get-pointer
            get-bytestructure
            get-bytevector
            get-event-signal
            .descriptor
            .wrap
            .unwrap))

(define-generic get-event-signal)

(define-class <bytestructure-class> (<class>)
  (descriptor #:init-keyword #:descriptor
              #:init-value #f
              #:getter .descriptor)
  (wrap #:init-keyword #:wrap #:getter .wrap)
  (unwrap #:init-keyword #:unwrap #:getter .unwrap))
(define-class <wlr-type> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer)
  #:metaclass <bytestructure-class>)

(define-method (= (f <wlr-type>) (l <wlr-type>))
  (= (.pointer f)
     (.pointer l)))
(define-method (get-bytestructure (obj <wlr-type>))
  (and-let* ((class (class-of obj))
             (descriptor (.descriptor class))
             (unwrap (.unwrap class)))
    (pointer->bytestructure (unwrap obj) descriptor)))
(define-method (get-bytevector (obj <wlr-type>))
  (and=> (get-bytestructure obj) bytestructure-bytevector))
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
               (define wrap
                 (let ((ptr->obj (make-weak-value-hash-table 3000)))
                   (lambda (ptr)
                     (or (hash-ref ptr->obj ptr)
                         (let ((o (make rtd #:pointer ptr)))
                           (hash-set! ptr->obj ptr o)
                           o)))))
               (define (unwrap o)
                 (if o
                     (begin (unless (is? o)
                              (error (string-append
                                      "not a "
                                      (symbol->string (class-name rtd))
                                      " or #f")
                                     o))
                            (.pointer o))
                     %null-pointer))
               (define-class rtd (supers ... <wlr-type>)
                 slots ...
                 #:wrap wrap
                 #:unwrap unwrap)
               (when (.descriptor rtd)
                 (when (assq 'events (struct-metadata-field-alist
                                      (bytestructure-descriptor-metadata
                                       (.descriptor rtd))))
                   (define-method (get-event-signal (b rtd) (signal-name <symbol>))
                     (let* ((unwrap-b (unwrap b))
                            (o (bytestructure-ref
                                (pointer->bytestructure
                                 unwrap-b
                                 (.descriptor rtd)) 'events)))
                       (wrap-wl-signal
                        (bytestructure+offset->pointer
                         (bytestructure-ref o signal-name)))))))
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
