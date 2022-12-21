(define-module (wlroots types)
  #:use-module (wayland util)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-2)
  #:use-module (wayland signal)
  #:use-module ((system foreign) #:select(pointer-address pointer? %null-pointer make-pointer null-pointer?))
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

(define %bytestructures (make-hash-table 1000))
(define-class <bytestructure-class> (<class>)
  (descriptor #:init-keyword #:descriptor
              #:init-value #f
              #:getter .descriptor)
  (wrap #:init-keyword #:wrap #:getter .wrap)
  (unwrap #:init-keyword #:unwrap #:getter .unwrap))

(define-method (initialize (object <bytestructure-class>) initargs)
  (next-method)
  (and=> (.descriptor object) (cut hash-set! %bytestructures <> object)))

(define (get-field-alist o)
  (cond ((struct-metadata? o) (struct-metadata-field-alist o))
        (else #f)))

(define-inlinable (force-or-nothing o)
  (if (promise? o)
      (force o)
      o))
(define (haneld-pointer-descriptor o)
  (let* ((metadata (bytestructure-descriptor-metadata o))
         (is-pointer? (pointer-metadata? metadata)))
    (values is-pointer? (if is-pointer?
                            (force-or-nothing (pointer-metadata-content-descriptor metadata))
                            o))))
(define-method (compute-get-n-set (class <bytestructure-class>) slot)
  (if (eq? (slot-definition-allocation slot) #:bytestructure)
      (let* ((index (slot-ref class 'nfields))
             (s (slot-definition-options slot))
             (b-name (or (get-keyword #:field-name s #f)
                         (slot-definition-name slot)))
             (descriptor (.descriptor class))
             (metadata (bytestructure-descriptor-metadata descriptor))
             (alist (get-field-alist metadata))
             (field-descriptor
              (assq-ref alist b-name)))
        (unless field-descriptor
          (goops-error "not field name'd `~S' found in class `~S' descriptor " b-name class))
        (slot-set! class 'nfields (+ index 1))
        (let* ((is-pointer? field-descriptor (haneld-pointer-descriptor field-descriptor))
               (handle (if is-pointer? make-pointer identity))
               (field-wrap (delay (or (and=>
                                       (hash-ref
                                        %bytestructures
                                        field-descriptor #f) .wrap)
                                      identity)))
               (field-unwrap (delay (or (and=>
                                         (hash-ref %bytestructures
                                                   field-descriptor #f)
                                         .unwrap)
                                        identity))))
          (list (lambda (o)
                  (let* ((f (force field-wrap))
                         (out (bytestructure-ref (get-bytestructure o) b-name)))
                    (f (handle
                        (cond ((bytestructure? out)
                               (bytestructure->pointer out))
                              (else out))))))
                (lambda (o v)
                  (bytestructure-set!
                   (get-bytestructure o) b-name
                   ((force field-unwrap) v))))))
      (next-method)))

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
                     (unless (pointer? ptr)
                       (goops-error "In ~S,~S is not a pointer" wrap ptr))
                     (if (null-pointer? ptr)
                         #f
                         (or (hash-ref ptr->obj ptr)
                             (let ((o (make rtd #:pointer ptr)))
                               (hash-set! ptr->obj ptr o)
                               o))))))
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
