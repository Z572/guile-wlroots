(define-module (wlroots utils)
  #:use-module (system foreign)
  #:use-module (util572 ffi-helpers)
  #:use-module (wlroots config)
  #:use-module (wayland util)
  #:use-module ((bytestructures guile)
                #:select
                (bytestructure
                 bs:pointer
                 bytestructure-offset))
  #:use-module (oop goops)
  #:export (wlr->pointer wlr->procedure)
  #:export-syntax (define-wlr-procedure define-enumeration))

(define <bytestructure> (class-of (bytestructure (bs:pointer '*))))

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
