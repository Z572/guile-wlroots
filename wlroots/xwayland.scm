(define-module (wlroots xwayland)
  #:use-module (oop goops)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:use-module (wlroots types)
  #:use-module (wlroots types surface)
  #:use-module (wlroots types compositor)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module (wayland display)
  #:use-module (wayland listener)
  #:use-module (wayland signal)
  #:use-module ((system foreign ) #:prefix ffi:)
  #:use-module (wlroots utils)
  #:use-module (bytestructures guile)
  #:export (wrap-wlr-xwayland-surface
            unwrap-wlr-xwayland-surface
            wlr-xwayland-display-name
            wlr-xwayland-surface-y
            wlr-xwayland-surface-x
            wlr-xwayland-surface-width
            wlr-xwayland-surface-height
            wlr-xwayland-surface-close
            wlr-xwayland-surface-class
            wlr-xwayland-surface-override-redirect
            wlr-xwayland-surface-title
            wlr-xwayland-surface-role
            wlr-xwayland-surface-instance
            wlr-xwayland-surface-surface
            wlr-xwayland-surface-set-fullscreen
            wlr-xwayland-surface-mapped?
            wlr-xwayland-surface-from-wlr-surface
            wlr-xwayland-surface-configure
            wlr-xwayland-create
            wlr-xwayland-surface-activate
            .display-name
            .wl-display
            .compositor
            .seat
            .window-id
            .surface-id
            .link
            .stack-link
            .unpaired-link
            .surface
            .x
            .y
            .width
            .height
            .override-redirect
            .mapped
            .title
            .class
            .instance
            .role
            .startup-id
            .pid
            .has-utf8-title
            .children
            .parent
            .parent-link
            .pinging
            .ping-timer
            .modal
            .fullscreen
            .maximized-vert
            .maximized-horz
            .minimized
            .has-alpha
            .surface-destroy
            .data))

(define-wlr-types-class-public wlr-xwayland ()
  (display-name #:accessor .display-name)
  (wl-display #:accessor .wl-display)
  (compositor #:accessor .compositor)
  (seat #:accessor .seat)
  (data #:accessor .data)
  #:descriptor %wlr-xwayland-struct)

(define-wlr-types-class wlr-xwayland-surface ()
  (window-id #:accessor .window-id)
  (surface-id #:accessor .surface-id)
  (link #:accessor .link)
  (stack-link #:accessor .stack-link)
  (unpaired-link #:accessor .unpaired-link)
  (surface #:accessor .surface)
  (x #:accessor .x)
  (y #:accessor .y)
  (width #:accessor .width)
  (height #:accessor .height)
  (override-redirect #:accessor .override-redirect)
  (mapped #:accessor .mapped)
  (title #:accessor .title)
  (class #:accessor .class)
  (instance #:accessor .instance)
  (role #:accessor .role)
  (startup-id #:accessor .startup-id)
  (pid #:accessor .pid)
  (has-utf8-title #:accessor .has-utf8-title)
  (children #:accessor .children)
  (parent #:accessor .parent)
  (parent-link #:accessor .parent-link)
  (pinging #:accessor .pinging)
  (ping-timer #:accessor .ping-timer)
  (modal #:accessor .modal)
  (fullscreen #:accessor .fullscreen)
  (maximized-vert #:accessor .maximized-vert)
  (maximized-horz #:accessor .maximized-horz)
  (minimized #:accessor .minimized)
  (has-alpha #:accessor .has-alpha)
  (surface-destroy #:accessor .surface-destroy)
  (data #:accessor .data)
  #:descriptor %wlr-xwayland-surface-struct)

(define (wlr-xwayland-display-name x)
  (.display-name x))
(define (wlr-xwayland-surface-surface x)
  (.surface x))

(define (wlr-xwayland-surface-class x)
  (.class x))

(define (wlr-xwayland-surface-override-redirect x)
  (.override-redirect x))

(define (wlr-xwayland-surface-x s)
  (.x s))
(define (wlr-xwayland-surface-y s)
  (.y s))
(define (wlr-xwayland-surface-width s)
  (.width s))
(define (wlr-xwayland-surface-height s)
  (.height s))

(define (wlr-xwayland-surface-mapped? x)
  (.mapped x))

(define (wlr-xwayland-surface-title x)
  (.title x))


(define (wlr-xwayland-surface-instance x)
  (.instance x))

(define (wlr-xwayland-surface-role x)
  (.role x))
;; wlr_xwayland_surface_set_fullscreen
(define-wlr-procedure (wlr-xwayland-create display compositor lazy?)
  ('* "wlr_xwayland_create" (list '* '* ffi:int))
  (wrap-wlr-xwayland (% (unwrap-wl-display display)
                        (unwrap-wlr-compositor compositor)
                        (if lazy? 1 0))))

(define-wlr-procedure (wlr-xwayland-surface-activate surface activated)
  (ffi:void "wlr_xwayland_surface_activate" `(* ,ffi:int8))
  (% (unwrap-wlr-xwayland-surface surface) (if activated 1 0)))

(define-wlr-procedure (wlr-xwayland-surface-close surface)
  (ffi:void "wlr_xwayland_surface_close" (list '*))
  "Request that this xdg toplevel closes."
  (% (unwrap-wlr-xwayland-surface surface)))
(define-wlr-procedure (wlr-xwayland-surface-set-fullscreen surface fullscreen)
  (ffi:void "wlr_xwayland_surface_set_fullscreen" (list '* ffi:int))
  (% (unwrap-wlr-xwayland-surface surface) (if fullscreen 1 0)))

(define-wlr-procedure (wlr-xwayland-surface-from-wlr-surface surface)
  ('* "wlr_xwayland_surface_from_wlr_surface" '(*))
  (wrap-wlr-xwayland-surface (% (unwrap-wlr-surface surface))))

(define-wlr-procedure (wlr-xwayland-surface-configure surface x y width height)
  (ffi:void "wlr_xwayland_surface_configure" (list '*
                                                   ffi:int16
                                                   ffi:int16
                                                   ffi:uint16
                                                   ffi:uint16))
  (% (unwrap-wlr-xwayland-surface surface) x y width height))
