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
 (guix utils))

(define-public guile-bytestructure-class
  (package
    (name "guile-bytestructure-class")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Z572/guile-bytestructure-class")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y3sryy79arp3f5smyxn8w7zra3j4bb0qdpl1p0bld3jicc4s86a"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~'("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           guile-3.0-latest))
    (inputs (list guile-3.0-latest))
    (propagated-inputs (list guile-bytestructures))
    (synopsis "bytestructure and goops")
    (description "This package combines bytestructure with goops,
and provide 4 new bytestructure-descriptor:
bs:unknow, cstring-pointer*, bs:enum, stdbool.")
    (home-page "https://github.com/Z572/guile-bytestructure-class")
    (license license:gpl3+)))

(define guile-wayland
  (package
    (name "guile-wayland")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/guile-wayland/guile-wayland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g20rvl540aqb6l4mi62k1mkvgipvvrlr7v40gs5kly450jinvmq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags '(list "GUILE_AUTO_COMPILE=0")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'load-extension
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* (find-files "." ".*\\.scm")
                (("\\(load-extension \"libguile-wayland\" *\"(.*)\"\\)" _ o)
                 (string-append
                  (object->string
                   `(or (false-if-exception
                         (load-extension "libguile-wayland" ,o))
                        (load-extension
                         ,(string-append
                           #$output
                           "/lib/libguile-wayland.so")
                         ,o)))))))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           texinfo
           guile-3.0-latest))
    (inputs (list guile-3.0-latest wayland wayland-protocols))
    (propagated-inputs
     (list
      guile-bytestructure-class
      guile-bytestructures))
    (synopsis "")
    (description "")
    (home-page "https://github.com/guile-wayland/guile-wayland")
    (license license:gpl3+)))

(define pixman-0.42.0
  (package
    (inherit pixman)
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://www.cairographics.org/releases/pixman-"
                version ".tar.gz"))
              (sha256
               (base32 "04vcyi9kmhdj1cx7nna8bdn7x4xp81zv6y45n2r3x974jn6lrxq7"))))))

(define wlroots-0.17.0
  (package
    (inherit wlroots)
    (name "wlroots")
    (version "0.17.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/wlroots/wlroots")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11vb6xjvsjz7j2jkx00ygjp5xi63ni8ydd8wf3s0200ldr4ffjjm"))))
    (propagated-inputs
     (modify-inputs
      (package-propagated-inputs wlroots)
      (replace "pixman" pixman-0.42.0)))))
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
    (inputs (list guile-3.0-latest wlroots-0.17.0))
    (propagated-inputs
     (list guile-bytestructures
           guile-wayland
           guile-bytestructure-class
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
