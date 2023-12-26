(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages gtk)
 (gnu packages autotools)
 (gnu packages python)
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
 (guix build-system meson)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (gnu packages hardware)
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
  (let ((commit "1110b82295509e2deec9fdacae2a434bb1a60a6f"))
    (package
      (name "guile-wayland")
      (version (git-version "0.0.2" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/guile-wayland/guile-wayland")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1k4r2cii1fv6047dpvav3w2andh7wqwa63b59ynsx0qqzk0ag8w2"))))
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
      (license license:gpl3+))))

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


(define-public libliftoff
  (package
    (name "libliftoff")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/emersion/libliftoff")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ikjp638d655ycaqkdnzhb12d29kkbb3a46lqhbhsfc8vsqj3z1l"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config pkg-config-for-build))
    (inputs (list libdrm))
    (home-page "https://gitlab.freedesktop.org/emersion/libliftoff")
    (synopsis "")
    (description "")
    (license #f)))

(define-public hwdata
  (package
    (name "hwdata")
    (version "0.365")                   ;updated monthly
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vcrhonek/hwdata")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00gqx24dyy9l98ygnvx8i087xq8pl9d2393h4d2cm4d5nnvr51d4"))))
    (build-system gnu-build-system)
    (arguments
     ;; Tests require pciutils, python, podman. Disable to avoid recursive dep.
     (list
      #:tests? #f
      ;; Do not cross-compile, since the package only contains data.
      #:target #f
      #:configure-flags #~(list (string-append "--datadir=" #$output "/share"))))
    (home-page "https://github.com/vcrhonek/hwdata")
    (synopsis "Hardware identification and configuration data")
    (description "@code{hwdata} contains various hardware identification and
configuration data, such as the @file{pci.ids} and @file{usb.ids} databases.
Each database is contained in a specific package output, such as the
@code{pci} output for @file{pci.ids}, the @code{usb} output for
@file{usb.ids}, etc.")
    (license (list license:gpl2+
                   license:expat))))

(define-public libdisplay-info
  (package
    (name "libdisplay-info")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/emersion/libdisplay-info")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ffq7w1ig1y44rrmkv1hvfjylzgq7f9nlnnsdgdv7pmcpfh45pgf"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config pkg-config-for-build hwdata
                         python-minimal
                         ;; for test
                         edid-decode))
    (arguments (list #:tests? #f))
    (home-page "https://gitlab.freedesktop.org/emersion/libdisplay-info")
    (synopsis "")
    (description "")
    (license #f)))


(define wlroots-0.17
  (package
    (inherit wlroots)
    (name "wlroots")
    (version "0.17.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/wlroots/wlroots")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hj4gq5vx8in65622yvjm8bwqkw2vpc556k9my997a0hn0ricj37"))))
    (propagated-inputs
     (modify-inputs
      (package-propagated-inputs wlroots)
      (replace "pixman" pixman-0.42.0)
      (append cairo)
      (append hwdata)
      (append libliftoff)
      (append libdisplay-info)))))
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
    (inputs (list guile-3.0-latest wlroots-0.17))
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
