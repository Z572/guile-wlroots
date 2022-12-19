(define-module (wlroots types output-management)
  #:use-module (oop goops)
  #:use-module (wayland display)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module (wayland util)
  #:use-module (wlroots types)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:export (%wlr-output-manager-v1-struct
            <wlr-output-manager-v1>
            wrap-wlr-output-manager-v1
            unwrap-wlr-output-manager-v1

            <wlr-output-configuration-v1>
            wrap-wlr-output-configuration-v1
            unwrap-wlr-output-configuration-v1

            wlr-output-manager-v1-create))

(define %wlr-output-manager-v1-struct
  (bs:struct `((display ,(bs:pointer '*))
               (global ,(bs:pointer '*))
               (resources ,%wl-list)
               (heads ,%wl-list)
               (serial ,uint32)
               (current-configuration-dirty ,int8) ;; bool
               (events ,(bs:struct `((apply ,%wl-signal-struct)
                                     (test ,%wl-signal-struct)
                                     (destroy ,%wl-signal-struct))))
               (display-destroy ,%wl-listener)
               (data ,(bs:pointer 'void)))))


(define-wlr-types-class wlr-output-manager-v1)
(define-wlr-types-class wlr-output-configuration-v1)


(define-method (get-event-signal (b <wlr-output-manager-v1>) (signal-name <symbol>))
  (let* ((a (bytestructure-ref
             (pointer->bytestructure
              (unwrap-wlr-output-manager-v1 b)
              %wlr-output-manager-v1-struct)
             'events)))
    (wrap-wl-signal (bytestructure+offset->pointer
                     (bytestructure-ref a signal-name)))))

(define-wlr-procedure (wlr-output-manager-v1-create display)
  ('* "wlr_output_manager_v1_create" (list '*))
  (wrap-wlr-output-manager-v1 (% (unwrap-wl-display display))))
