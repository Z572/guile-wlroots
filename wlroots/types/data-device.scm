(define-module (wlroots types data-device)
  #:use-module (wayland server display)
  #:use-module (wayland util)
  #:use-module (wayland signal)
  #:use-module (wayland server listener)
  #:use-module (wayland list)
  #:autoload (wlroots types seat) (unwrap-wlr-seat)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots types)
  #:use-module (wlroots types compositor)
  #:use-module (wlroots utils)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:re-export (%wlr-drag-struct
               %wlr-data-source-struct
               %wlr-drag-icon-struct)
  #:export (<wlr-drag>
            <wlr-drop-icon>
            wrap-wlr-data-source
            unwrap-wlr-data-source
            wrap-wlr-data-device-manager
            unwrap-wlr-data-device-manager
            wrap-wlr-drag-icon
            unwrap-wlr-drag-icon
            wrap-wlr-drag
            unwrap-wlr-drag
            wlr-data-device-manager-create
            wlr-data-source-destroy
            wlr-seat-start-pointer-drag
            .drag
            .surface
            .mapped
            .data
            .grab-type
            .keyboard-grab
            .pointer-grab
            .touch-grab
            .seat
            .seat-client
            .focus-client
            .icon
            .focus
            .source
            .started
            .dropped
            .cancelling
            .grab-touch-id
            .touch-id
            ))

(define-wlr-types-class wlr-drag ()
  (grab-type      #:allocation #:bytestructure #:accessor .grab-type    )
  (keyboard-grab  #:allocation #:bytestructure #:accessor .keyboard-grab)
  (pointer-grab   #:allocation #:bytestructure #:accessor .pointer-grab )
  (touch-grab     #:allocation #:bytestructure #:accessor .touch-grab   )
  (seat           #:allocation #:bytestructure #:accessor .seat         )
  (seat-client    #:allocation #:bytestructure #:accessor .seat-client  )
  (focus-client   #:allocation #:bytestructure #:accessor .focus-client )
  (icon           #:allocation #:bytestructure #:accessor .icon         )
  (focus          #:allocation #:bytestructure #:accessor .focus        )
  (source         #:allocation #:bytestructure #:accessor .source       )
  (started        #:allocation #:bytestructure #:accessor .started      )
  (dropped        #:allocation #:bytestructure #:accessor .dropped      )
  (cancelling     #:allocation #:bytestructure #:accessor .cancelling   )
  (grab-touch-id  #:allocation #:bytestructure #:accessor .grab-touch-id)
  (touch-id       #:allocation #:bytestructure #:accessor .touch-id     )
  (data           #:allocation #:bytestructure #:accessor .data         )
  #:descriptor %wlr-drag-struct)

(define-wlr-types-class wlr-data-source ()
  #:descriptor %wlr-data-source-struct)

(define-wlr-types-class wlr-drag-icon ()
  (drag    #:allocation #:bytestructure #:accessor .drag   )
  (surface #:allocation #:bytestructure #:accessor .surface)
  (mapped  #:allocation #:bytestructure #:accessor .mapped )
  (data    #:allocation #:bytestructure #:accessor .data   )
  #:descriptor %wlr-drag-icon-struct)

(define-wlr-types-class wlr-data-device-manager ()
  #:descriptor %wlr-data-device-manager-struct)


(define-wlr-procedure (wlr-data-device-manager-create display)
  ('* "wlr_data_device_manager_create" '(*))
  (wrap-wlr-data-device-manager (% (unwrap-wl-display display))))

(define-wlr-procedure (wlr-data-source-destroy source)
  (ffi:void "wlr_data_source_destroy" '(*))
  (% (unwrap-wlr-data-source source)))

(define-wlr-procedure (wlr-seat-start-pointer-drag seat drag serial)
  (ffi:void "wlr_seat_start_pointer_drag" `(* * ,ffi:uint32))
  (% (unwrap-wlr-seat seat) (unwrap-wlr-drag drag) serial))
