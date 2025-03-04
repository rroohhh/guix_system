(define-module (vup fpga)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((gnu packages fpga)
                #:prefix guix:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages)
  #:use-module (vup prjoxide)
  #:use-module (vup python-xyz))

;; kept in lockstep with yosys upstream for reproducability
(define-public abc-for-yosys
  (package
    (inherit guix:abc)
    (version "v0.50")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yosyshq/abc")
             (commit "v0.50")))
       (file-name (git-file-name (package-name guix:abc) version))
       (sha256
        (base32 "1r1qcfm4c16v0582zxi1n0jaqb8ah8alzmwjsr49y8j3wlnx6han"))))))

(define-public yosys-git
  ((package-input-rewriting/spec `(("abc" unquote
                                    (const abc-for-yosys))))
   (package
     (inherit guix:yosys)
     (version "0.50-0.0778195")
     (source
      (origin
        (inherit (package-source guix:yosys))
        (uri (git-reference
              (url "https://github.com/yosyshq/yosys.git")
              (commit "077819572222094b30b3cc08e62132456095ca0d")))
        (sha256
         (base32 "0zwkyppzc4z6c7xxhp5z9hi4swhgg7dm1ikm2p9xgk5va6n0qp6k"))
        (file-name (git-file-name (package-name guix:yosys) version))))
     (arguments
      (append (package-arguments guix:yosys)
              (list #:tests? #f))))))

(define-public icestorm
  (package
    (inherit guix:icestorm)
    (version "0.0-1.7fbf8c0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yosyshq/icestorm")
             (commit "7fbf8c0afbcf7665c45499b090409859b1815184")))
       (file-name (git-file-name (package-name guix:icestorm) version))
       (sha256
        (base32 "0vwl0j5sw2awsv82s18zby522h2b6wa5jyc6dkpqfxybak19kv7f"))))))

(define-public trellis
  (package
    (name "trellis")
    (version "1.4-0.14ac883")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yosyshq/prjtrellis")
             (commit "14ac883fa639b11fdc98f3cdef87a5d01f79e73d")
             (recursive? #t))) ;for prjtrellis-db
       (file-name (git-file-name name version))
       (sha256
        (base32 "002d7fmr1g7680n11m2fzgvqxhrxhg87z7x99ik2xvngr34r70pr"))))
    (build-system cmake-build-system)
    (inputs (list python boost))
    (arguments
     `(#:configure-flags (list (string-append "-DCURRENT_GIT_VERSION="
                                              ,version))
       #:out-of-source? #f
       #:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'pre-configure
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (system* "source" "environment.sh")
                      (chdir "libtrellis") #t)))))
    (synopsis "Documentation and bitstream tools for Lattice ECP5 FPGAs")
    (description "Project Trellis documents the Lattice ECP5 architecture
to enable development of open-source tools. Its goal is
to provide sufficient information to develop a free and
open Verilog to bitstream toolchain for these devices.")
    (home-page "https://github.com/symbiflow/prjtrellis")
    (license license:isc)))

(define-public mistral
  (package
    (name "mistral")
    (version "0.0-1.d509238")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Ravenslofty/mistral")
             (commit "d509238a203aadbb76291ab06543a401df91cf54")
             (recursive? #t))) ;for prjmistral-db
       (file-name (git-file-name name version))
       (sha256
        (base32 "10bn05akcy700fd5y2371f27dw3ga7g2fs16105b3mihr8czx0c1"))))
    (build-system cmake-build-system)
    (inputs (list python))
    (arguments
     '(#:tests? #f))
    (synopsis "Mistral - A Cyclone V bitstream library")
    (description
     "It's the very first version of a library/command line utility to compile and decompile Cyclone V bitstreams, as used in the de-10 nano (used in MiSTer) and the future Analogue Pocket.")
    (home-page "https://github.com/Ravenslofty/mistral")
    (license license:bsd-3)))

(define python-crc
  (package
    (name "python-crc")
    (version "7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "crc" version))
       (sha256
        (base32 "0zf0cqfs8lhh6d7szv2g06pzkrxlxkw1si355kvf8ym3144m9pcr"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core))
    (arguments
     '(#:tests? #f))
    (home-page "https://github.com/Nicoretti/crc")
    (synopsis "Pure Python CRC library")
    (description "Pure Python CRC library.")
    (license license:bsd-2)))

(define-public python-apycula
  (package
    (name "apicula")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Apycula" version))
       (sha256
        (base32 "0f450jjvlhf6ndrcnivsbiqiwz5r317ngrjcch3yfnqsylg6f3h6"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-crc))
    (native-inputs (list python-setuptools python-setuptools-scm python-wheel))
    (arguments
     '(#:tests? #f))
    (home-page "https://github.com/YosysHQ/apicula")
    (synopsis "Open Source tools for Gowin FPGAs")
    (description "Open Source tools for Gowin FPGAs")
    (license #f)))

(define-public nextpnr
  (package
    (name "nextpnr")
    (version "0.7-0.f3a5024")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yosyshq/nextpnr")
             (commit "f3a5024de293a307057be7ec557b0ab26c9623c8")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08s86i53dkb5apj3l281l2dcjhbyhk4xgq4jq9l27whq8d9hq4zb"))))
    (build-system cmake-build-system)
    (inputs (list python
                  boost
                  qtbase-5
                  trellis
                  icestorm
                  rust-prjoxide
                  python-apycula
                  tcl
                  zlib
                  capnproto
                  eigen
                  ))
    (native-inputs (list pkg-config))
    (arguments
     (list
      #:cmake cmake-3.30
      #:configure-flags #~(list
                                "-DARCH=generic;ice40;ecp5;nexus;machxo2" ;TODO(robin): fpga_interchange
                                "-DBUILD_TESTS=ON"
                                "-DUSE_OPENMP=ON"
                                "-DBUILD_GUI=ON"
                                "-DSERIALIZE_CHIPDBS=FALSE" ;high memory requirements
                                (string-append "-DICESTORM_INSTALL_PREFIX="
                                               #$(this-package-input "icestorm"))
                                (string-append "-DTRELLIS_INSTALL_PREFIX="
                                               #$(this-package-input "trellis"))
                                (string-append "-DMISTRAL_ROOT="
                                               #$(package-source mistral))
                                (string-append "-DOXIDE_INSTALL_PREFIX="
                                               #$(this-package-input "prjoxide"))
                                (string-append "-DGOWIN_BBA_EXECUTABLE="
                                               #$(this-package-input "apicula")
                                               "/bin/gowin_bba"))))
    (synopsis "nextpnr -- a portable FPGA place and route tool")
    (description
     "nextpnr aims to be a vendor neutral, timing driven, FOSS FPGA place and route tool.")
    (home-page "https://github.com/yosyshq/nextpnr")
    (license license:isc)))

(define-public gtkwave-gtk4
  (package
    (name "gtkwave-gtk4")
    (version "3.3.116-1.4435a9e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gtkwave/gtkwave")
             (commit "4435a9ef89892757eaf703085606f10040da6bfe")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09b5j8m33jirk9ndihy3399b3w08lg5qlhpirzwmb5q2ai8x98r4"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config
                         gperf
                         flex
                         `(,glib "bin")
                         desktop-file-utils
                         `(,gtk "bin")))
    (inputs (list glib gtk gtk+ bzip2 gobject-introspection))
    (synopsis "Waveform viewer for FPGA simulator trace files")
    (description "This package is a waveform viewer for FPGA
simulator trace files (@dfn{FST}).")
    (home-page "http://gtkwave.sourceforge.net/")
    ;; Exception against free government use in tcl_np.c and tcl_np.h.
    (license (list license:gpl2+ license:expat license:tcl/tk))))

;; (define-public super-prove
;;   (let ((commit "c7a4df4f60b9fe847f673b566c9a4fb4f71f4181"))
;;     (package
;;       (name "super-prove")
;;       (version (string-append "0.1.0-" (string-take commit 9)))
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/sterin/super-prove-build")
;;                       (commit commit)
;;                       (recursive? #t)))
;;                 (file-name (git-file-name name version))
;;                 (patches (search-patches "super-prove.patch"))
;;                 (sha256
;;                  (base32
;;                   "1xlbz619kz4q1502sr0kgqg82qig4l30y596aqskakqlfd0z9ikh"))))
;;       (inputs (list python-wrapper ncurses readline git zlib))
;;       (build-system cmake-build-system)
;;       (synopsis "super-prove")
;;       (description "super-prove")
;;       (home-page "https://github.com/sterin/super-prove-build")
;;       (license #f))))
