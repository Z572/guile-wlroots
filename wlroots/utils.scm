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
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (bytestructure-class)
  #:use-module ((bytestructures guile)
                #:select
                (bytestructure
                 bs:pointer
                 float
                 bytestructure-offset
                 bs:vector
                 make-bytestructure-descriptor))
  #:use-module (oop goops)
  #:export (wlr->pointer wlr->procedure color->pointer %color-struct)
  #:re-export (assert)
  #:export-syntax (define-wlr-procedure define-enumeration))

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

(define-syntax define-wlr-procedure
  (lambda (x)
    (syntax-case x ()
      ((_ (pname args ...) (return-type cname arg-types) body ...)
       (with-syntax ((% (datum->syntax x '%)))
         #'(begin
             (define-public pname
               (let ((% (wlr->procedure return-type cname arg-types)))
                 (lambda* (args ...)
                   #((name . pname))
                   body ...)))))))))

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
