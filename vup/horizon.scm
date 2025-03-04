(define-module (vup horizon)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages engineering)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public horizon
  (package
    (name "horizon")
    (version (string-append "2.6.0-1.bfbe5b1"))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/horizon-eda/horizon")
             (commit "bfbe5b12b6441390e1ce3a517a05a6d496be4509")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yqcx692231cx2xcms8z72pc9sr49c4pwr3v39jlg2wzckkp75wl"))))
    (build-system meson-build-system)
    (native-inputs (list cmake pkg-config))
    (inputs (list util-linux
                  yaml-cpp
                  sqlite
                  gtkmm-3
                  curl
                  glib
                  (list glib "bin")
                  libzip
                  libgit2
                  glm
                  librsvg
                  zeromq
                  python
                  boost
                  opencascade-occt
                  cppzmq
                  podofo
                  coreutils
                  hicolor-icon-theme
                  librsvg
                  libsigc++
                  glibmm
                  libarchive
                  libspnav))
    (arguments
     (list
      #:glib-or-gtk? #t))
    (synopsis "Horizon is a free EDA package")
    (description
     "Horizon EDA is an Electronic Design Automation package supporting an integrated end-to-end workflow for printed circuit board design including parts management and schematic entry.")
    (home-page "https://horizon-eda.org/")
    (license license:gpl3)))
