(define-module (vup fpga)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages fpga) #:prefix guix:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages qt)
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
  (let ((commit "cac8f99eaa220a5e3db5caeb87cef0a975c953a2")
        (revision "1"))
    (package
      (inherit guix:abc)
      (version (string-append "0.0-" revision "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/abc")
                      (commit commit)))
                (file-name (git-file-name (package-name guix:abc) version))
                (sha256
                 (base32
                  "1r1qcfm4c16v0582zxi1n0jaqb8ah8alzmwjsr49y8j3wlnx6han")))))))


(define-public yosys-git
  ((package-input-rewriting/spec `(("abc" . ,(const abc-for-yosys))))
     (package
      (inherit guix:yosys)
      (version "0.48-0.4f0dae1")
      (source (origin
               (inherit (package-source guix:yosys))
               (uri (git-reference
                     (url "https://github.com/yosyshq/yosys.git")
                     (commit "4f0dae17adb7a8df490398851307e08a5532e3a2")))
               (sha256
                (base32
                 "1njnrlyf5jwhmyiwq4r53x3mw9pjgqc66m68mrmcnmjd8yfxkix9"))
               (file-name (git-file-name (package-name guix:yosys) version))))
      (arguments (append (package-arguments guix:yosys) (list #:tests? #f))))))


(define-public icestorm
  (let ((commit "d20a5e9001f46262bf0cef220f1a6943946e421d")
        (revision "8"))
    (package
      (inherit guix:icestorm)
      (version (string-append "0.0-" revision "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/icestorm")
                      (commit commit)))
                (file-name (git-file-name (package-name guix:icestorm) version))
                (sha256
                 (base32
                  "0v8l427f9a3pdv109mrfhwiiwl0q94vr8hrcazagyidyxp26ch3l")))))))

(define-public trellis
  (let ((commit "b9120de2453e71b68b344452ef12040ee53d47ab"))
    (package
      (name "trellis")
      (version (string-append "1.3-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/prjtrellis")
                      (commit commit)
                      (recursive? #t))) ; for prjtrellis-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1z46y191c1068wp56wf8fwnay2jaq5faiw031y9q1w185w2k6lcs"))))
      (build-system cmake-build-system)
      (inputs `(("python" ,python) ("boost" ,boost)))
      (arguments
       `(#:configure-flags (list (string-append "-DCURRENT_GIT_VERSION=" ,version))
         #:out-of-source? #f
         #:phases (modify-phases %standard-phases
                    (delete 'check) ; no test
                    (add-before 'configure 'pre-configure
                      (lambda* (#:key outputs inputs #:allow-other-keys)
                        (system* "source" "environment.sh")
                        (chdir "libtrellis")
                        #t)))))
      (synopsis "Documentation and bitstream tools for Lattice ECP5 FPGAs")
      (description "Project Trellis documents the Lattice ECP5 architecture
to enable development of open-source tools. Its goal is
to provide sufficient information to develop a free and
open Verilog to bitstream toolchain for these devices.")
      (home-page "https://github.com/symbiflow/prjtrellis")
      (license license:isc))))

(define-public mistral
  (let ((commit "d6bd02cd1eccb4b8f410d074ea96c31966fb1079"))
    (package
      (name "mistral")
      (version (string-append "0.0-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Ravenslofty/mistral")
                      (commit commit)
                      (recursive? #t))) ; for prjmistral-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "02xxnj36xhhzrgjgw58kdxqk04yw1293m82k9gc7pwl64p1gjhxk"))))
      (build-system cmake-build-system)
      (inputs (list python))
      (arguments `(#:phases (modify-phases %standard-phases
                              (delete 'check))))
      (synopsis "Mistral - A Cyclone V bitstream library")
      (description "It's the very first version of a library/command line utility to compile and decompile Cyclone V bitstreams, as used in the de-10 nano (used in MiSTer) and the future Analogue Pocket.")
      (home-page "https://github.com/Ravenslofty/mistral")
      (license license:bsd-3))))


(define-public python-crcmod
  (package
    (name "python-crcmod")
    (version "1.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "crcmod" version))
        (sha256
          (base32
            "07k0hgr42vw2j92cln3klxka81f33knd7459cn3d8aszvfh52w6w"))))
    (build-system python-build-system)
    (home-page "http://crcmod.sourceforge.net/")
    (synopsis "CRC Generator")
    (description "CRC Generator")
    (license license:expat)))

(define-public python-apycula
  (package
    (name "python-apycula")
    (version "0.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Apycula" version))
        (sha256
          (base32
            "1ihah1hikaxhfn0vb5pqknh5rzv4nx81rpcdmdly49nyzaj06xl7"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
      `(("python-crcmod" ,python-crcmod)
        ("python-numpy" ,python-numpy)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page "https://github.com/YosysHQ/apicula")
    (synopsis "Open Source tools for Gowin FPGAs")
    (description "Open Source tools for Gowin FPGAs")
    (license #f)))


(define-public nextpnr
  (let ((commit "b36e8a3013ac70a9fbe71d2163f660dafe3b8b2f"))
    (package
      (name "nextpnr")
      (version (string-append "0.5-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/nextpnr")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1d7mnm15z9fvxg5vw0k5zs71yfahsksdf23g0cw942prllqsjac4"))))
      (build-system cmake-build-system)
      (inputs `(("python" ,python)
                ("boost" ,boost)
                ("qtbase" ,qtbase-5)
                ("trellis" ,trellis)
                ("icestorm" ,icestorm)
                ("prjoxide" ,rust-prjoxide)
                ("apicula" ,python-apycula)
                ("mistral" ,(package-source mistral))
                ("tcl" ,tcl)
                ("zlib" ,zlib)
                ("capnproto" ,capnproto)
                ("pkgconfig" ,pkg-config)
                ("eigen" ,eigen)))
      (arguments
       `(#:configure-flags (list
                            "-DARCH=generic;ice40;ecp5;nexus;gowin;machxo2;mistral" ; TODO(robin): fpga_interchange
                            "-DBUILD_TESTS=ON"
                            "-DUSE_OPENMP=ON"
                            "-DBUILD_GUI=ON"
                            "-DSERIALIZE_CHIPDBS=FALSE" ; high memory requirements
                            (string-append "-DICESTORM_INSTALL_PREFIX=" (assoc-ref %build-inputs "icestorm"))
                            (string-append "-DTRELLIS_INSTALL_PREFIX=" (assoc-ref %build-inputs "trellis"))
                            (string-append "-DMISTRAL_ROOT=" (assoc-ref %build-inputs "mistral"))
                            (string-append "-DOXIDE_INSTALL_PREFIX=" (assoc-ref %build-inputs "prjoxide"))
                            (string-append "-DGOWIN_BBA_EXECUTABLE=" (assoc-ref %build-inputs "apicula") "/bin/gowin_bba"))))
      (synopsis "nextpnr -- a portable FPGA place and route tool")
      (description "nextpnr aims to be a vendor neutral, timing driven, FOSS FPGA place and route tool.")
      (home-page "https://github.com/yosyshq/nextpnr")
      (license license:isc))))

(define-public gtkwave-gtk4
  (let ((commit "0a800de96255f7fb11beadb6729fdf670da76ecb"))
   (package
    (name "gtkwave-gtk4")
    (version (string-append "3.3.116-" (string-take commit 9)))
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gtkwave/gtkwave")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "113psdcv6a1npfv40gs1cjf9ck342fp31pf4y8msvc2dhcr72l1b"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config gperf flex `(,glib "bin") desktop-file-utils `(,gtk "bin")))
    (inputs (list glib gtk gtk+ bzip2 gobject-introspection))
    (synopsis "Waveform viewer for FPGA simulator trace files")
    (description "This package is a waveform viewer for FPGA
simulator trace files (@dfn{FST}).")
    (home-page "http://gtkwave.sourceforge.net/")
    ;; Exception against free government use in tcl_np.c and tcl_np.h.
    (license (list license:gpl2+ license:expat license:tcl/tk)))))



(define-public super-prove
  (let ((commit "c7a4df4f60b9fe847f673b566c9a4fb4f71f4181"))
    (package
      (name "super-prove")
      (version (string-append "0.1.0-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sterin/super-prove-build")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (patches (search-patches "super-prove.patch"))
                (sha256
                 (base32
                  "1xlbz619kz4q1502sr0kgqg82qig4l30y596aqskakqlfd0z9ikh"))))
      (inputs (list python-wrapper ncurses readline git zlib))
      (build-system cmake-build-system)
      (synopsis "super-prove")
      (description "super-prove")
      (home-page "https://github.com/sterin/super-prove-build")
      (license #f))))
