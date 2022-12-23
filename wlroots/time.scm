(define-module (wlroots time)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign-library)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (%timespec-struct
            clock-gettime
            wrap-timespec
            unwrap-timespec
            <timespec>
            .tv-sec
            .tv-nsec
            value->clockid_t
            clockid_t->value))
(define %timespec-struct
  (bs:struct
   `((tv-sec ,int64) ;; __time_t
     (tv-nsec ,long) ;; __syscall_slong_t
     )))

(define-enumeration value->clockid_t clockid_t->value
  (CLOCK_REALTIME           0)
  (CLOCK_MONOTONIC          1)
  (CLOCK_PROCESS_CPUTIME_ID 2)
  (CLOCK_THREAD_CPUTIME_ID  3)
  (CLOCK_MONOTONIC_RAW      4)
  (CLOCK_REALTIME_COARSE    5)
  (CLOCK_MONOTONIC_COARSE   6)
  (CLOCK_BOOTTIME           7)
  (CLOCK_REALTIME_ALARM     8)
  (CLOCK_BOOTTIME_ALARM     9)
  (CLOCK_TAI                11))

(define-wlr-types-class timespec ()
  (tv-sec #:allocation #:bytestructure #:accessor .tv-sec)
  (tv-nsec #:allocation #:bytestructure #:accessor .tv-nsec)
  #:descriptor %timespec-struct)
(define %clock-gettime
  (foreign-library-function
   #f "clock_gettime"
   #:return-type ffi:int
   #:arg-types (list ffi:int32 '*)))

(define* (clock-gettime clock-id #:optional (timespec (make <timespec>)))
  (let* ((clock-id (if (symbol? clock-id) (clockid_t->value clock-id) clock-id))
         (o (%clock-gettime clock-id (get-pointer timespec))))
    (values o timespec)))
