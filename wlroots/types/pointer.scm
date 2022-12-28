(define-module (wlroots types pointer)
  #:use-module (wlroots types)
  #:use-module (wlroots types input-device)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:re-export (%wlr-event-pointer-motion-struct
               %wlr-event-pointer-axis-struct
               %wlr-event-pointer-motion-absolute-struct)
  #:export (wrap-wlr-event-pointer-motion
            unwrap-wlr-event-pointer-motion
            wrap-wlr-event-pointer-axis
            unwrap-wlr-event-pointer-axis
            wrap-event-pointer-motion-absolute
            unwrap-event-pointer-motion-absolute
            wlr-event-pointer-axis-time-msec
            wlr-event-pointer-axis-orientation
            wlr-event-pointer-axis-delta
            wlr-event-pointer-axis-delta-discrete
            wlr-event-pointer-axis-source
            .device
            .time-msec
            .delta-x
            .delta-y
            .x
            .y
            .unaccel-dx
            .unaccel-dy))



(define-wlr-types-class wlr-event-pointer-motion ()
  (device #:accessor .device #:allocation #:bytestructure)
  (time-msec #:accessor .time-msec #:allocation #:bytestructure)
  (delta-x #:accessor .delta-x #:allocation #:bytestructure)
  (delta-y #:accessor .delta-y #:allocation #:bytestructure)
  (unaccel-dx #:accessor .unaccel-dx #:allocation #:bytestructure)
  (unaccel-dy #:accessor .unaccel-dy #:allocation #:bytestructure)
  #:descriptor %wlr-event-pointer-motion-struct)


(define-wlr-types-class-public wlr-event-pointer-motion-absolute ()
  (device #:accessor .device #:allocation #:bytestructure)
  (time-msec #:accessor .time-msec #:allocation #:bytestructure)
  (x #:accessor .x #:allocation #:bytestructure)
  (y #:accessor .y #:allocation #:bytestructure)
  #:descriptor %wlr-event-pointer-motion-absolute-struct)


(define-wlr-types-class-public wlr-event-pointer-button ())

(define-wlr-types-class wlr-event-pointer-axis ()
  (time-msec #:accessor wlr-event-pointer-axis-time-msec #:allocation #:bytestructure)
  (source #:accessor wlr-event-pointer-axis-source #:allocation #:bytestructure)
  (delta #:accessor wlr-event-pointer-axis-delta #:allocation #:bytestructure)
  (delta-discrete #:accessor wlr-event-pointer-axis-delta-discrete #:allocation #:bytestructure)
  (orientation #:accessor wlr-event-pointer-axis-orientation #:allocation #:bytestructure)
  #:descriptor %wlr-event-pointer-axis-struct)
