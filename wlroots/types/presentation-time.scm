(define-module (wlroots types presentation-time)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (bytestructures guile)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wlroots backend)
  #:use-module (wlroots types)
  #:use-module (wlroots types output)
  #:use-module (wlroots utils)
  #:export (%wlr-presentation-struct
            %wlr-presentation-feedback-struct
            %wlr-presentation-event-struct
            wlr-presentation-create
            wrap-wlr-presentation
            unwrap-wlr-presentation
            wrap-wlr-presentation-feedback
            unwrap-wlr-presentation-feedback
            wrap-wlr-presentation-event
            unwrap-wlr-presentation-event
            .clock
            .output
            .output-committed
            .output-commit-seq
            .tv-sec
            .tv-nsec
            .refresh
            .seq
            .flags))
(define %wlr-presentation-struct
  (bs:struct `((global ,(bs:pointer '*))
               (clock ,int32) ;; clockid_t
               (events ,(bs:struct `((destroy ,%wl-listener-struct))))
               (display-destroy ,%wl-listener-struct))))
(define %wlr-presentation-feedback-struct
  (bs:struct `((resources ,%wl-list-struct)
               (output ,%wlr-output-struct)
               (output-committed ,bool)
               (output-commit-seq ,uint32)
               (output-commit ,%wl-listener-struct)
               (output-present ,%wl-listener-struct)
               (output-destroy ,%wl-listener-struct))))
(define %wlr-presentation-event-struct
  (bs:struct `((output ,%wlr-output-struct)
               (tv-sec ,uint64)
               (tv-nsec ,uint32)
               (refresh ,uint32)
               (seq ,uint64)
               (flags ,uint32))))

(define-wlr-types-class wlr-presentation ()
  (clock #:allocation #:bytestructure #:accessor .clock)
  #:descriptor %wlr-presentation-struct)

(define-wlr-types-class wlr-presentation-feedback ()
  (output            #:allocation #:bytestructure #:accessor .output           )
  (output-committed  #:allocation #:bytestructure #:accessor .output-committed )
  (output-commit-seq #:allocation #:bytestructure #:accessor .output-commit-seq)
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
