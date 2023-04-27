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
    (inputs (list guile-3.0-latest wlroots))
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
