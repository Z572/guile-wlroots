(define-module (wlroots types output)
  #:use-module ((system foreign)
                #:select ((uint32 . ffi:uint32)
                          (float . ffi:float)
                          (int . ffi:int)
                          (void . ffi:void)))
  #:use-module (wlroots render allocator)
  #:use-module (wlroots render renderer)
  #:use-module (wlroots backend)
  #:use-module (wlroots util addon)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (wayland util)
  #:use-module (wayland listener)
  #:use-module (oop goops)
  #:use-module (wayland list)
  #:use-module (srfi srfi-26)
  #:use-module (wayland signal)
  #:use-module (bytestructures guile)
  #:export (%wlr-output-state-struct
            wrap-wlr-output
            unwrap-wlr-output
            wlr-output-init-render
            .modes
            <wlr-output>
            wlr-output-preferred-mode
            wlr-output-set-mode
            wlr-output-enable
            wlr-output-commit
            wlr-output-name
            wlr-output-description
            wlr-output-backend
            wlr-output-physical-width
            wlr-output-physical-height
            wlr-output-refresh
            wlr-output-enabled
            wlr-output-scale
            wlr-output-width
            wlr-output-height
            wlr-output-modes
            wlr-output-data

            %pixman-region32-t-struct
            <wlr-output-cursor>
            wrap-wlr-output-cursor
            unwrap-wlr-output-cursor))

(define-wlr-types-class wlr-output)

(eval-when (expand load eval)
  (load-extension "libguile-wlroots" "scm_init_wlr_output"))
(define (wlr-output-backend o)
  (wrap-wlr-backend(%wlr-output-backend o)))
(define %pixman-box32-struct
  (bs:struct `((x1 ,int32)
               (y1 ,int32)
               (x2 ,int32)
               (y2 ,int32))))
(define %pixman-region32-t-struct
  (bs:struct `((extents ,%pixman-box32-struct)
               (data ,(bs:pointer '*)))))
(define %wlr-output-mode-struct
  (bs:struct `((width ,int32)
               (height ,int32)
               (refresh ,int32)
               (preferred ,int)
               (link ,%wl-list))))
(define %wlr-output-state-struct
  (bs:struct `((committed ,uint32)
               (damage ,%pixman-region32-t-struct)
               (enabled ,int)
               (scale ,float)
               (transform ,int)
               (adaptive-sync-enabled ,int)
               (render-format ,uint32)
               (buffer ,(bs:pointer '*))
               (mode-type ,int)
               (mode ,(bs:pointer '*))
               (custom-mode ,(bs:struct
                              `((width ,int32)
                                (height ,int32)
                                (refresh ,int32))))
               (gamma-lut ,(bs:pointer '*))
               (gamma-lut-size ,size_t))))
(define-class <wlr-output-mode> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))
(define (wrap-wlr-output-mode p)
  (make <wlr-output-mode> #:pointer p))
(define (unwrap-wlr-output-mode o)
  (.pointer o))
;; (define-class <wlr-output> ()
;;   (bytestructure #:accessor .bytestructure #:init-keyword #:bytestructure)
;;   (modes #:allocation #:virtual
;;          #:accessor .modes
;;          #:slot-ref (lambda (a) (wrap-wl-list (bytestructure-ref (.bytestructure a) 'modes)))
;;          #:slot-set! (lambda (instance new-val)
;;                        (bytestructure-set!
;;                         (.bytestructure instance)
;;                         'modes new-val))))
(define (wlr-output-modes o)
  (wrap-wl-list (%wlr-output-modes o)))
(define .modes wlr-output-modes)

(define wlr-output-init-render
  (let ((proc (wlr->procedure ffi:int "wlr_output_init_render" '(* * *))))
    (lambda (output allocator renderer)
      (proc (unwrap-wlr-output output)
            (unwrap-wlr-allocator allocator)
            (unwrap-wlr-renderer renderer)))))
(define-wlr-procedure (wlr-output-preferred-mode output)
  ('* "wlr_output_preferred_mode" '(*))
  (wrap-wlr-output-mode (% (unwrap-wlr-output output))))

(define-wlr-procedure (wlr-output-set-mode output mode)
  (ffi:void "wlr_output_set_mode" '(* *) )
  (% (unwrap-wlr-output output) (unwrap-wlr-output-mode mode)))

(define-wlr-procedure (wlr-output-enable output enable)
  (ffi:void "wlr_output_enable" (list '* ffi:int))
  (% (unwrap-wlr-output output) (if enable 1 0)))

(define-wlr-procedure (wlr-output-commit output)
  (ffi:int "wlr_output_commit" '(*))
  (case (% (unwrap-wlr-output output))
    ((1) #t)
    ((0) #f)
    (else #t)))

(define-wlr-types-class wlr-output-cursor)

