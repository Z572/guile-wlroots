(define-module (wlroots utils)
  #:use-module ((system foreign) #:select (pointer-address make-pointer pointer->procedure))
  #:use-module (util572 color)
  #:use-module (util572 ffi-helpers)
  #:use-module (wlroots config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (wayland util)
  #:use-module (rnrs bytevectors)
  #:use-module ((bytestructures guile)
                #:select
                (bytestructure
                 bs:pointer
                 float
                 bytestructure-offset
                 bs:vector
                 make-bytestructure-descriptor))
  #:use-module (oop goops)
  #:export (wlr->pointer wlr->procedure color->pointer %color-struct bool bs:enum enum-metadata-field-alist)
  #:export-syntax (define-wlr-procedure define-enumeration))

(define bool
  (make-bytestructure-descriptor
   1 1
   #f
   (lambda (syntax? bytevector offset)
     (if syntax?
         #`(not (zero? (bytevector-s8-ref #,bytevector #,offset)))
         (not (zero? (bytevector-s8-ref bytevector offset)))))
   (lambda (syntax? bytevector offset value)
     (if syntax?
         #`(bytevector-s8-set! #,bytevector #,offset #,(if value 1 0))
         (bytevector-s8-set! bytevector offset (if value 1 0))))))

(define-record-type <enum-metadata>
  (make-enum-metadata field-alist)
  enum-metadata?
  (field-alist enum-metadata-field-alist))

(define (bs:enum fields)
  ;; TODO: check duplicates.
  (define-values (symbols integers) (unzip2 fields))

  (define alist (map (lambda (o)
                       (let ((symbol (first o))
                             (integer (second o)))
                         (unless (and (symbol? symbol)
                                      (integer? integer))
                           (error "Invalid value" o))
                         (cons integer symbol)))
                     fields))

  (define meta (make-enum-metadata alist))
  (define (getter syntax? bytevector offset)
    (if syntax?
        #`(or (assq-ref #,alist (bytevector-s8-ref #,bytevector #,offset))
              (error "get invalid value!"))
        (or (assq-ref alist (bytevector-s8-ref bytevector offset))
            (error "get invalid value!"))))
  (define (setter syntax? bytevector offset value)
    (define (to-value v)
      (cond ((symbol? v)
             (or (and=> (list-index (lambda (o) (eq? v o)) symbols)
                        (cut list-ref integers <>) )
                 (error "can't not found symbol in enum!" v)))
            ((integer? v)
             (unless (member v integers)
               (error "can't not found integer in enum!" v))
             v)
            (else (error "Is not symbol or "))))
    (if syntax?
        #`(bytevector-s8-set! #,bytevector #,offset #,(to-value value))
        (bytevector-s8-set! bytevector offset (to-value value))))
  (make-bytestructure-descriptor
   4 4 #f getter setter meta))

(define <bytestructure> (class-of (bytestructure (bs:pointer '*))))

(define %color-struct (bs:vector 4 float))
(define-method (color->pointer (color <rgba-color>))
  (bytestructure->pointer
   (bytestructure %color-struct
                  (list->vector (map (cut / <> 255)
                                     (list (color-r color)
                                           (color-g color)
                                           (color-b color)
                                           (color-a color))))))
  )
(define-method (= (p <foreign>) (p2 <foreign>))
  (= (pointer-address p)
     (pointer-address p2)))

(define-method (= (b <bytestructure>) (b* <bytestructure>))
  (= (bytestructure->pointer b)
     (bytestructure->pointer b*)))

(define-method (- (p <foreign>) . (n <number>))
  (apply + p (- n)))
(define-method (+ (p <foreign>) . a)
  (make-pointer (apply + (pointer-address p) a)))

(define-method (- (p <bytestructure>) (n <number>))
  (+ p (- n)))

(define-method (+ (p <bytestructure>) . n)
  (apply + (bytestructure->pointer p) n))

(define (wlr->pointer name)
  (dynamic-func name (dynamic-link %libwlroots)))

(define (wlr->procedure return name params)
  (let ((ptr (wlr->pointer name)))
    (pointer->procedure return ptr params)))

(define-make-ffi-procedure define-wlr-procedure wlr->procedure)

;;; copy define from (system vm dwarf) module
(define-syntax-rule (define-enumeration code->name name->code
                      (tag value) ...)
  (begin
    (define tag value) ...
    (define code->name
      (let ((table (make-hash-table)))
        (hashv-set! table value 'tag)
        ...
        (lambda (v)
          (hashv-ref table v v))))
    (define name->code
      (let ((table (make-hash-table)))
        (hashv-set! table 'tag value)
        ...
        (lambda (v)
          (hashv-ref table v v))))))
