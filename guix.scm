(use-modules
 ((guix licenses) #:prefix license:)
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages build-tools)
 (gnu packages file)
 (gnu packages freedesktop)
 (gnu packages gl)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages wm)
 (gnu packages xdisorg)
 (gnu packages xorg)
 (gnu packages)
 (gnu packages)
 (guix build-system gnu)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (guix utils) (guix packages)
 )
(define libdrm-next
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.110")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0dwpry9m5l27dlhq48j4bsiqwm0247cxdqwv3b7ddmkynk2f9kpf"))))))
(define wayland-next
  (package
    (inherit wayland)
    (name "wayland")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wayland.freedesktop.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09c7rpbwavjg4y16mrfa57gk5ix6rnzpvlnv1wp7fnbh9hak985q"))))))
(define wayland-protocols-next
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.25")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "0q0laxdvf8p8b7ks2cbpqf6q0rwrjycqrp8pf8rxm86hk5qhzzzi"))))
    (inputs
     (modify-inputs (package-inputs wayland-protocols)
                    (replace "wayland" wayland-next)))))
(define wlroots-next
  (package
    (inherit wlroots)
    (name "wlroots")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h"))))
    (arguments (substitute-keyword-arguments (package-arguments wlroots)
                 ((#:configure-flags flags ''())
                  `(cons "-Dbackends=['drm','libinput','x11']" ,flags))))

    (propagated-inputs
     (modify-inputs (package-propagated-inputs wlroots)
                    (prepend libdrm-next libglvnd xcb-util-renderutil)
                    (replace "wayland" wayland-next)
                    (replace "wayland-protocols" wayland-protocols-next)))))

(define guile-wlroots
  (package
    (name "guile-wlroots")
    (version "0.1")
    (source (local-file "." "guile-wlroots-checkout"
                        #:recursive? #t
                        #:select? (git-predicate (dirname (current-filename)))))
    (build-system gnu-build-system)
    (arguments (list
                #:make-flags '(list "GUILE_AUTO_COMPILE=0")
                #:phases
                #~(modify-phases %standard-phases
                    (add-after 'build 'load-extension
                      (lambda* (#:key outputs #:allow-other-keys)
                        (substitute*
                            (find-files "." ".*\\.scm")
                          (("\\(load-extension \"libguile-wlroots\" *\"(.*)\"\\)" _ o)
                           (string-append
                            (object->string
                             `(or (false-if-exception (load-extension "libguile-wlroots" ,o))
                                  (load-extension
                                   ,(string-append
                                     (assoc-ref outputs "out")
                                     "/lib/libguile-wlroots.so")
                                   ,o)))))))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           texinfo
           ))
    (inputs (list guile-3.0 wlroots-next))
    (propagated-inputs
     (list guile-bytestructures
           (primitive-load
            (string-append (dirname (dirname (current-filename)))
                           "/guile-wayland/guix.scm")) ))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
guile-wlroots
;; (list
;;  info-reader
;;  bear)
