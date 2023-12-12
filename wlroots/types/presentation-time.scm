(define-module (wlroots types presentation-time)
  #:use-module (wayland server display)
  #:use-module (wayland list)
  #:use-module (bytestructures guile)
  #:use-module (wayland server listener)
  #:use-module (wayland signal)
  #:use-module (wlroots backend)
  #:use-module (wlroots types)
  #:use-module (wlroots types output)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (wlr-presentation-create
            wrap-wlr-presentation
            unwrap-wlr-presentation
            wrap-wlr-presentation-feedback
            unwrap-wlr-presentation-feedback
            wrap-wlr-presentation-event
            unwrap-wlr-presentation-event
            .output
            .output-committed
            .output-commit-seq
            .tv-sec
            .tv-nsec
            .refresh
            .seq
            .flags
            .zero-copy))

(define-wlr-types-class wlr-presentation ()
  #:descriptor %wlr-presentation-struct)

(define-wlr-types-class wlr-presentation-feedback ()
  (output            #:accessor .output           )
  (output-committed  #:accessor .output-committed )
  (output-commit-seq #:accessor .output-commit-seq)
  (zero-copy         #:getter   .zero-copy)
  #:descriptor %wlr-presentation-feedback-struct)

(define-wlr-types-class wlr-presentation-event ()
  (output  #:allocation #:bytestructure #:accessor .output )
  (tv-sec  #:allocation #:bytestructure #:accessor .tv-sec )
  (tv-nsec #:allocation #:bytestructure #:accessor .tv-nsec)
  (refresh #:allocation #:bytestructure #:accessor .refresh)
  (seq     #:allocation #:bytestructure #:accessor .seq    )
  (flags   #:allocation #:bytestructure #:accessor .flags  )
  #:descriptor %wlr-presentation-event-struct)

(define-wlr-procedure (wlr-presentation-create display backend)
  ('* "wlr_presentation_create" '(* *))
  (wrap-wlr-presentation (% (unwrap-wl-display display) (unwrap-wlr-backend backend))))
