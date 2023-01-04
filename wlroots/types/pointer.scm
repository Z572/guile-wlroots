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
            .device
            .time-msec
            .source
            .delta
            .delta-discrete
            .orientation
            .delta-x
            .delta-y
            .x
            .y
            .unaccel-dx
            .unaccel-dy
            .button
            .state))

(define-wlr-types-class wlr-pointer ()
  #:descriptor %wlr-pointer-struct)

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


(define-wlr-types-class-public wlr-event-pointer-button ()
  (device    #:allocation #:bytestructure #:accessor .device   )
  (time-msec #:allocation #:bytestructure #:accessor .time-msec)
  (button    #:allocation #:bytestructure #:accessor .button   )
  (state     #:allocation #:bytestructure #:accessor .state    )
  #:descriptor %wlr-event-pointer-button-struct)

(define-wlr-types-class wlr-event-pointer-axis ()
  (device #:accessor .device)
  (time-msec #:accessor .time-msec)
  (source #:accessor .source)
  (delta #:accessor .delta)
  (delta-discrete #:accessor .delta-discrete)
  (orientation #:accessor .orientation)
  #:descriptor %wlr-event-pointer-axis-struct)
