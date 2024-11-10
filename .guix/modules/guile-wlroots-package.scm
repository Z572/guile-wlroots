(define-module (guile-wlroots-package)
  #:use-module (guile-wayland packages guile-wayland)
  #:use-module (guile-wayland packages guile-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages python)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages hardware)
  #:use-module (guix utils))

(define-public guile-wlroots
  (package
    (name "guile-wlroots")
    (version "0.0.1-git")
    (source (local-file "../.." (git-file-name name version)
                        #:recursive? #t
                        #:select?
                        (or (git-predicate
                             (string-append
                              (current-source-directory)
                              "/../.."))
                            (const #t))))
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
           guile-util572
           guile-xkbcommon
           guile-libinput))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))

guile-wlroots
