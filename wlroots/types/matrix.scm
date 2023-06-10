(define-module (wlroots types matrix)
  #:use-module (wayland protocol)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wlroots util box)
  #:use-module (bytestructure-class)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (9-vecotr-or-list->pointer) )

(define-inlinable (9-vecotr-or-list->pointer projection)
  (let* ((projection* (if (vector? projection)
                          projection
                          (list->vector projection))))
    (if (= (vector-length projection*) 9)
        (bytestructure->pointer (bytestructure (bs:vector 9 float) projection*))
        (raise
         (condition
          (&message (message "Not a 9 length vector or list.")))))))

(define-wlr-procedure (wlr-matrix-project-box box transform rotation projection)
  (ffi:void "wlr_matrix_project_box" (list '* '* ffi:int32 ffi:float '*))
  (let* ((mat (bytestructure (bs:vector 9 float))))
    (% (bytestructure->pointer mat)
       (unwrap-wlr-box box)
       (bs:enum->integer %wl-output-transform-enum transform)
       rotation
       (9-vecotr-or-list->pointer projection))

    (map (lambda (n) (bytestructure-ref mat n)) (iota 9))))
