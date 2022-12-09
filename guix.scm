(use-modules
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
 (gnu packages pciutils)
 (gnu packages texinfo)
 (gnu packages wm)
 (gnu packages xdisorg)
 (gnu packages xorg)
 (gnu packages)
 (guix build-system gnu)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (guix utils)
 )
(define libdrm-next
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.114")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/mesa/drm")
             (commit (string-append "libdrm-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vxfha0anvxf2i1lirh0m35avkv9rdmhd5c3s6fp6g432963vgrh"))))))
(define wayland-next
  (package
    (inherit wayland)
    (name "wayland")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wayland/wayland")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fwad6w5jm32c04wh4gca7d1ixdj4b9dnsiy1h6qd9nxs0w47wwy"))))))
(define wayland-protocols-next
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wayland/wayland-protocols")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kpyvnzlwfj9d57v43z5fhk7fliz6224m4hw1xj425c8vrjbw0nx"))))
    (inputs
     (modify-inputs (package-inputs wayland-protocols)
                    (replace "wayland" wayland-next)))))
(define libinput-next
  (package
    (inherit libinput)
    (version "1.19.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://freedesktop.org/software/libinput/"
                                  "libinput-" version ".tar.xz"))
              (sha256
               (base32
                "0h5lz54rrl48bhi3vki6s08m6rn2h62rlf08dhgchdm9nmqaaczz"))))))
(define wlroots-next
  (package
    (inherit wlroots)
    (name "wlroots")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18rfr3wfm61dv9w8m4xjz4gzq2v3k5vx35ymbi1cggkgbk3lbc4k"))))
    (arguments (substitute-keyword-arguments (package-arguments wlroots)
                 ((#:configure-flags flags ''())
                  `(cons "-Dbackends=['drm','libinput','x11']" ,flags))
                 ((#:phases phases)
                  #~(modify-phases #$phases
                      (add-before 'configure 'sub-hwdata
                        (lambda* (#:key native-inputs inputs #:allow-other-keys)
                          (substitute* "backend/drm/meson.build"
                            (("/usr/share/hwdata/pnp.ids")
                             (search-input-file (or native-inputs inputs)
                                                "share/hwdata/pnp.ids")))))))))

    (propagated-inputs
     (modify-inputs (package-propagated-inputs wlroots)
                    (prepend (list hwdata "pnp") libdrm-next libglvnd xcb-util-renderutil)
                    (replace "wayland" wayland-next)
                    (replace "libinput-minimal" libinput-next)
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
           guile-3.0-latest))
    (inputs (list guile-3.0-latest wlroots-next))
    (propagated-inputs
     (list guile-bytestructures
           (primitive-load
            (string-append (dirname (dirname (current-filename)))
                           "/guile-wayland/guix.scm"))
           (primitive-load
            (string-append (dirname (dirname (current-filename)))
                           "/guile-bytestructure-class/guix.scm"))
           (primitive-load
            (string-append (dirname (dirname (current-filename)))
                           "/util572/guix.scm"))
           (primitive-load
            (string-append (dirname (dirname (current-filename)))
                           "/guile-xkbcommon/guix.scm"))
           (primitive-load
            (string-append (dirname (dirname (current-filename)))
                           "/guile-libinput/guix.scm"))))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
guile-wlroots
;; (list
;;  info-reader
;;  bear)
