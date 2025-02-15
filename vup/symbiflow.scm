(define-module (vup symbiflow)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (vup python-xyz)
  #:use-module (vup fpga)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages python)
  #:use-module (gnu packages java)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages node)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages embedded)
  #:use-module ((gnu packages fpga) #:prefix guix:))


(define-public vtr-verilog-to-routing
  (let*
      ((commit "b3b34e77aa1bad7371bfe05087e01e04e2495179")
       (version (string-append "8.0.0-g" (string-take commit 9))))
    (package
       (name "vtr-verilog-to-routing")
       (version version)
       (source
        (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/verilog-to-routing/vtr-verilog-to-routing")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1k3dkh2kaa7ay93bz7gwz8zhnrm27dmqdk1wjc1wwv6ajiysfsqw"))))
       (native-inputs `(("pkg-config" ,pkg-config) ("bison" ,bison) ("flex" ,flex)))
       (inputs `(("zlib" ,zlib) ("tbb" ,tbb) ("eigen3" ,eigen) ("gtk" ,gtk+) ("glib" ,glib "bin")))
       (build-system cmake-build-system)
       (home-page "https://verilogtorouting.org/")
       (synopsis "Verilog to Routing -- Open Source CAD Flow for FPGA Research")
       (description "Verilog to Routing -- Open Source CAD Flow for FPGA Research")
       (license #f))))

(define-public abc-for-symbiflow-yosys
  (let ((commit "4f5f73d18b137930fb3048c0b385c82fa078db38")
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
                  "0z1kp223kix7i4r7mbj2bzawkdzc55nsgc41m85dmbajl9fsj1m0")))))))

(define-public yosys-for-symbiflow
 (let ((commit "0fb4224ebca86156a1296b9210116d9a9cbebeed")
       (version "0.9+3962"))
   ((package-input-rewriting/spec `(("abc" . ,(const abc-for-symbiflow-yosys))))
    (package
      (inherit guix:yosys)
      (version (string-append version "+" (string-take commit 9)))
      (source (origin
                (inherit (package-source guix:yosys))
                (uri (git-reference
                      (url "https://github.com/yosyshq/yosys.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1g95nk4d0jnym8l0rxc67q8glpgfxlzrabrlsg5xskjkab1l39db"))
                (file-name (git-file-name (package-name guix:yosys) version))))
      (inputs (append (package-inputs guix:yosys) `(("zlib" ,zlib))))))))

(define-public python-tinyfpgab
  (package
    (name "python-tinyfpgab")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tinyfpgab" version))
        (sha256
          (base32
            "1dmpcckz7ibkl30v58wc322ggbcw7myyakb4j6fscm6xav23k4bg"))))
    (build-system python-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                  (delete 'check))))
    (home-page "http://tinyfpga.com")
    (synopsis "Programmer for the TinyFPGA B2 boards (http://tinyfpga.com)")
    (description
      "Programmer for the TinyFPGA B2 boards (http://tinyfpga.com)")
    (license license:gpl2)))

(define (setuptools-scm-version-setter v)
  `(lambda* (#:key inputs #:allow-other-keys)
     (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,v)
     #t))

(define-public python-tinyprog
  (package
    (name "python-tinyprog")
    (version "1.0.23")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/tinyfpga/tinyfpga-bootloader")
             (commit "a8e02505124768740429ef6da01e81326a4123cd")))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0zbrvvb957z2lwbfd39ixqdsnd2w4wfjirwkqdrqm27bjz308731"))))
    (build-system python-build-system)
    (native-inputs `(("python-setuptools-scm" ,python-setuptools-scm)))
    (arguments `(#:phases
                 (modify-phases %standard-phases
                  (add-before 'build 'set-setuptools-scm-version
                    ,(setuptools-scm-version-setter version))
                  (add-after 'unpack 'chdir
                    (lambda _
                      (begin
                        (chdir "programmer")
                        #t)))
                  (delete 'check))))
    (home-page "http://tinyfpga.com")
    (synopsis "Programmer for FPGA boards using the TinyFPGA USB Bootloader (http://tinyfpga.com)")
    (description
      "Programmer for FPGA boards using the TinyFPGA USB Bootloader (http://tinyfpga.com)")
    (license license:gpl2)))

(define-public python-fasm
  (package
   (name "python-fasm")
   (version "0.0.2.post70")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (recursive? #t)
           (url "https://github.com/symbiflow/fasm")
           (commit "99f199f9e32fd30c8adffcc73c13caf95a951c35")))
     (file-name (git-file-name name version))
     (patches '("vendor_utf8cpp.patch"))
     (sha256
      (base32 "0aqxjfqknfjymg2f7vg2x1zcr61cczxbcivfmbif1fxm36bn5msw"))))
   (build-system gnu-build-system)
   (arguments `(#:make-flags '("build")
                #:phases
                (modify-phases %standard-phases
                    (delete 'configure)
                    (add-after 'unpack 'ensure-no-mtimes-pre-1980
                      (lambda _
                        (use-modules (ice-9 ftw))
                        (let ((early-1980 631148400))
                          (ftw "." (lambda (file stat flag)
                                     (unless (<= early-1980 (stat:mtime stat))
                                       (utime file early-1980 early-1980))
                                     #t))
                          #t)))
                    (delete 'set-SOURCE-DATE-EPOCH)
                    (delete 'check)
                    (replace 'install
                      (lambda* (#:key outputs (configure-flags '()) use-setuptools?
                                        #:allow-other-keys)
                        "Install a given Python package."
                        (let* ((out (assoc-ref outputs "out"))
                               (params (append (list (string-append "--prefix=" out))
                                               (if use-setuptools?
                                                   ;; distutils does not accept these flags
                                                   (list "--single-version-externally-managed"
                                                          "--root=/")
                                                   '())
                                               configure-flags)))
                          (apply invoke "python" "./setup.py" "install" params)
                          #t)))
                    (add-after 'unpack 'patch-makefile-and-make-writable
                      (lambda* (#:key inputs #:allow-other-keys)
                        (map (lambda (f) (chmod f #o666)) '("setup.cfg"))
                        (substitute* "Makefile"
                          (("-include.*conda.mk") ""))
                        (substitute* "src/CMakeLists.txt"
                          (("include\\(ExternalAntlr4Cpp\\)") "add_subdirectory(../third_party/antlr4/runtime/Cpp build)"))

                        (substitute* "third_party/antlr4/runtime/Cpp/CMakeLists.txt"
                          (("because of ExternalProject") "because of ExternalProject
list(INSERT CMAKE_MODULE_PATH 0 \"${CMAKE_CURRENT_SOURCE_DIR}/runtime/CMake\")"))

                        (substitute* "third_party/antlr4/runtime/Cpp/runtime/CMakeLists.txt"
                          (("utf8cpp QUIET") "utf8cpp"))

                        (mkdir-p "third_party/antlr4/runtime/Cpp/runtime/CMake")
                        (call-with-output-file "third_party/antlr4/runtime/Cpp/runtime/CMake/Findutf8cpp.cmake"
                          (lambda (port)
                             (format port "~a" "find_path(utf8cpp_INCLUDE_DIR
  NAMES utf8.h
  PATH_SUFFIXES utf8cpp
  DOC \"utf8cpp include directory\")
mark_as_advanced(utf8cpp_INCLUDE_DIR)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(utf8cpp
  REQUIRED_VARS utf8cpp_INCLUDE_DIR)

if (utf8cpp_FOUND)
  set(utf8cpp_INCLUDE_DIRS \"${utf8cpp_INCLUDE_DIR}\")

  if (NOT TARGET utf8cpp::utf8cpp)
    add_library(utf8cpp::utf8cpp INTERFACE IMPORTED)
    set_target_properties(utf8cpp::utf8cpp PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES \"${utf8cpp_INCLUDE_DIR}\")
  endif ()
endif ()")))
                        #t)))))
   (inputs `(("cython" ,python-cython)
             ("textx" ,python-textx)))
             ;; ("antlr" ,antlr4)

   (native-inputs `(("cmake" ,cmake) ("python" ,python-wrapper)
                    ("python-wheel" ,python-wheel)
                    ("git" ,git) ("jdk" ,icedtea) ("utfcpp" ,utfcpp)
                    ("pkg-config" ,pkg-config) ("uuid" ,util-linux "lib")))
   (home-page "https://github.com/SymbiFlow/fasm")
   (synopsis "FPGA Assembly (FASM) Parser and Generation library")
   (description "FPGA Assembly (FASM) Parser and Generation library")
   (license license:isc)))

(define-public python-symbiflow-xc-fasm
  (package
   (name "python-symbiflow-xc-fasm")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/symbiflow/symbiflow-xc-fasm")
           (commit "14afc2bae24cbf6ee5e7d057a58b4cbd776358d0")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "10rn1xww7r47ghc5sfyzb52sbc2gc86j4163m1lrb9lrvanbkl4j"))))
   (build-system python-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (delete 'check))))
   (inputs `(("fasm" ,python-fasm)))
   (home-page "https://github.com/SymbiFlow/symbiflow-xc-fasm")
   (synopsis "Library to convert FASM files to bitstream.")
   (description
    "Library to convert FASM files to bitstream.")
   (license license:isc)))

(define-public python-quicklogic-fasm
  (package
   (name "python-quicklogic-fasm")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/quicklogic-corp/quicklogic-fasm")
           (commit "57b6e60574a9d483dc94710d0d3ff42a62b4ec41")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0q87vs36sd4w8sxhipirvnjdrdn65yclgjfp00k7d65wk85p0l71"))))
   (build-system python-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (delete 'check))))
   (inputs `(("fasm" ,python-fasm) ("python-quicklogic-fasm-utils" ,python-quicklogic-fasm-utils)))
   (home-page "https://github.com/quicklogic-corp/quicklogic-fasm")
   (synopsis "QuickLogic-FASM")
   (description "QuickLogic-FASM")
   (license license:asl2.0)))

(define-public python-quicklogic-fasm-utils
  (package
   (name "python-quicklogic-fasm-utils")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/quicklogic-corp/quicklogic-fasm-utils")
           (commit "3d6a375ddb6b55aaa5a59d99e44a207d4c18709f")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0kr94220mp4c6ks955ll8qksn1vypg7pncxcc9nnc24sw59qwx60"))))
   (build-system python-build-system)
   (inputs `(("fasm" ,python-fasm)))
   (home-page "https://github.com/quicklogic-corp/quicklogic-fasm-utils")
   (synopsis "QuickLogic-FASM-utils")
   (description "QuickLogic-FASM-utils")
   (license #f)))

(define-public python-ql-fasm
  (package
   (name "python-ql-fasm")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/quicklogic-corp/ql_fasm")
           (commit "e5d09154df9b0c6d1476ac578950ec95abb8ed86")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1xld2dfggiaf2n9ryq89s4ydpkvxh1lsnkhhsdwpa49zq629gj4d"))))
   (build-system python-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (delete 'check))))
   (inputs `(("fasm" ,python-fasm)))
   (home-page "https://github.com/quicklogic-corp/ql_fasm")
   (synopsis "QuickLogic-FASM")
   (description "QuickLogic-FASM")
   (license #f)))

(define-public python-quicklogic-timings-importer
  (package
   (name "python-quicklogic-timings-importer")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/quicklogic-corp/quicklogic-timings-importer")
           (commit "eec0737d3345ebcb54e3c080a838e0e5f5526783")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1biqcikj08z6lkzqr10rgfl45ni9widk5ccpaa0gpgx686bc1aph"))))
   (build-system python-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (delete 'check))))
   (home-page "https://github.com/quicklogic-corp/quicklogic-timings-importer")
   (synopsis "FASM to/from bitstream converter for QuickLogic qlf FPGA device family")
   (description "FASM to/from bitstream converter for QuickLogic qlf FPGA device family")
   (license license:asl2.0)))

(define-public prjxray
  (package
   (name "prjxray")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (recursive? #t)
           (url "https://github.com/symbiflow/prjxray")
           (commit "086f9a1714e96323947d7c2526f5e4b97063e79f")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "15f0d2rcpbp56nj00wa49f9mg4s4bvwmx2ys87s8wls19yiq87w4"))))
   (build-system cmake-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (delete 'check))))
   (home-page "https://github.com/symbiflow/prjxray")
   (synopsis "Documenting the Xilinx 7-series bit-stream format.")
   (description "Documenting the Xilinx 7-series bit-stream format.")
   (license #f)))

(define-public prjxray-db
  (package
   (name "prjxray-db")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (recursive? #t)
           (url "https://github.com/symbiflow/prjxray-db")
           (commit "2e51ad3ff7a188b8aca752dcf6a0c83641a04c72")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1ngn8zjy9n7bnmj31fbnc1ffq4chxq1fb48z9n3c0ky16iw7106q"))))
   (build-system copy-build-system)
   (home-page "https://github.com/symbiflow/prjxray-db")
   (synopsis "Project X-Ray Database: XC7 Series")
   (description "Project X-Ray Database: XC7 Series")
   (license #f)))

(define-public python-zipfile2
  (package
    (name "python-zipfile2")
    (version "0.0.12")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zipfile2" version))
        (sha256
          (base32
            "0256m134qs045j1c8mmgii8ipkwhww9sjbc6xyawhykid34zfxkk"))))
    (build-system python-build-system)
    (home-page "")
    (synopsis "An improved ZipFile class.")
    (description "An improved ZipFile class.")
    (license #f)))

(define-public python-okonomiyaki
  (package
    (name "python-okonomiyaki")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "okonomiyaki" version))
        (sha256
          (base32
            "1dw9di7s92z201lwq7aqy5h9h53af73ffx6pnl5iz3lnfi0zf85p"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-attrs" ,python-attrs)
        ("python-distro" ,python-distro)
        ("python-jsonschema" ,python-jsonschema)
        ("python-six" ,python-six)
        ("python-zipfile2" ,python-zipfile2)))
    (arguments `(#:phases
                 (modify-phases %standard-phases
                  (delete 'check))))
    (home-page "")
    (synopsis
      "('Self-contained library to deal with metadata in Enthought-specific eggs',)")
    (description
      "('Self-contained library to deal with metadata in Enthought-specific eggs',)")
    (license license:bsd-3)))

(define-public python-simplesat
  (package
    (name "python-simplesat")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "simplesat" version))
        (sha256
          (base32
            "0n6qm2gzwji19ykp3i6wm6vjw7dnn92h2flm42708fxh6lkz6hqr"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-attrs" ,python-attrs)
        ("python-okonomiyaki" ,python-okonomiyaki)
        ("python-six" ,python-six)))
    (arguments `(#:phases
                 (modify-phases %standard-phases
                  (delete 'check))))
    (home-page
      "https://github.com/enthought/sat-solvers")
    (synopsis
      "Simple SAT solvers for use in Enstaller")
    (description
      "Simple SAT solvers for use in Enstaller")
    (license #f)))

(define-public python-ipyxact
  (package
    (name "python-ipyxact")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ipyxact" version))
        (sha256
          (base32
            "18mvgwnj7ccnrqx7x3cg70fyklrkymf9nadb0sh3is5csmz8gj7b"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pyyaml" ,python-pyyaml)))
    (home-page "https://github.com/olofk/ipyxact")
    (synopsis "Python IP-Xact handling library")
    (description "Python IP-Xact handling library")
    (license license:expat)))

(define-public edalize
  (package
    (name "edalize")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/lowRISC/edalize")
             (commit "ot")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p6gf75y89qfchpxhfmv1zs2gb9nl4apcj93ww19jljsjnnzpqy6"))))
    (build-system python-build-system)
    (native-inputs `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs `(("python-jinja2" ,python-jinja2)))
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (delete 'check)
                   (add-before 'build 'set-setuptools-scm-version
                     ,(setuptools-scm-version-setter version)))))
    (home-page "https://github.com/lowRISC/edalize")
    (synopsis "")
    (description "")
    (license #f)))

(define-public fusesoc
  (package
    (name "fusesoc")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/lowRISC/fusesoc")
             (commit "ot")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03nvk0lbnrlj6ks0qkpn3jlszxw32psjly2fapcmn668l0r2xlkd"))))
    (build-system python-build-system)
    (native-inputs `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-simplesat" ,python-simplesat)
       ("python-pyyaml" ,python-pyyaml)
       ("python-ipyxact" ,python-ipyxact)
       ("python-edalize" ,edalize)))
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (delete 'check)
                   (add-before 'build 'set-setuptools-scm-version
                     ,(setuptools-scm-version-setter version)))))
    (home-page "https://github.com/lowRISC/fusesoc")
    (synopsis "")
    (description "")
    (license #f)))

(define-public python-pyjson
  (package
    (name "python-pyjson")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyjson" version))
        (sha256
          (base32
            "0a4nkmc9yjpc8rxkqvf3cl3w9hd8pcs6f7di738zpwkafrp36grl"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/leeyoshinari/Small_Tool/tree/master/pyjson")
    (synopsis
      "Compare the similarities between two JSONs.")
    (description
      "Compare the similarities between two JSONs.")
    (license #f)))

(define-public python-vtr-xml-utils
  (package
    (name "python-vtr-xml-utils")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/symbiflow/vtr-xml-utils")
             (commit "d6ba1f17513ea2e83bc47fcec4248f6650f42857")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fr7q180m0masr6i2agndydkrb9cjgnzrvfwxccyb6l4r2m866fd"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (native-inputs
     `(("pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-lxml" ,python-lxml)))
    (home-page "https://github.com/symbiflow/vtr-xml-utils")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public python-symbiflow-v2x
  (package
    (name "python-symbiflow-v2x")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/symbiflow/python-symbiflow-v2x")
             (commit "105647c036fe386fef4d13f630e608cd322aa465")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0789rsfz1rdgh3gy3pnyj538lbj0pzd3n98s9k8aknwnvnrijx48"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (native-inputs
     `(("pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-lxml" ,python-lxml) ("python-pyjson", python-pyjson)
       ("python-vtr-xml-utils" ,python-vtr-xml-utils)))
    (home-page "https://github.com/symbiflow/pyhon-symbiflow-v2x")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public python-sdf-timing
  (package
    (name "python-sdf-timing")
    (version "0.0.post117")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/symbiflow/python-sdf-timing")
             (commit "5740ac42d730941de3b0fd04a00f17816c73bb5e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05d61azgz1n1wnr0shz35ysas56in3mm6wxwjrp7v8m5ns13c8sb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (propagated-inputs
      `(("python-ply" ,python-ply)
        ("python-pyjson" ,python-pyjson)
        ("pytest-runner" ,python-pytest-runner)))
    (home-page
      "https://github.com/SymbiFlow/python-sdf-timing")
    (synopsis
      "Python library for working Standard Delay Format                 (SDF) Timing Annotation files.")
    (description
      "Python library for working Standard Delay Format                 (SDF) Timing Annotation files.")
    (license #f)))

(define-public python-pythondata-software-compiler-rt
  (package
    (name "python-pythondata-software-compiler-rt")
    (version "0.0.post6206")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri
               "pythondata-software-compiler_rt"
               version))
        (sha256
          (base32
            "1blrkj9pw0rnb15c68w7j1r757b4k2p6hihkx9c5pmyg9x6668w9"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page
      "https://github.com/litex-hub/pythondata-software-compiler_rt")
    (synopsis
      "Python module containing data files for LLVM Compiler RT Module software.")
    (description
      "Python module containing data files for LLVM Compiler RT Module software.")
    (license #f)))

(define-public python-migen
  (package
    (name "python-migen")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/m-labs/migen")
             (commit "27dbf03edd75c32dc1706e2a1316950c3a8d452a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7c7bbc05cb8xvxd612cxr7mvsxhaim0apfh7ax32wi9ykpl1ad"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-colorama" ,python-colorama)))
    (home-page "https://m-labs.hk")
    (synopsis
      "Python toolbox for building complex digital hardware")
    (description
      "Python toolbox for building complex digital hardware")
    (license license:bsd-3)))

(define-public python-pythondata-cpu-vexriscv
  (package
    (name "python-pythondata-cpu-vexriscv")
    (version "1.0.1.post360")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pythondata-cpu-vexriscv" version))
        (sha256
          (base32
            "1g2gbb1kxangjmmb2kmrz0fj2yqi0gcmywccyd4iimnyr5z0sz8y"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page
      "https://github.com/litex-hub/pythondata-cpu-vexriscv")
    (synopsis
      "Python module containing verilog files for VexRISCV cpu.")
    (description
      "Python module containing verilog files for VexRISCV cpu.")
    (license #f)))

(define-public python-pythondata-cpu-lm32
  (package
    (name "python-pythondata-cpu-lm32")
    (version "0.0.post152")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pythondata-cpu-lm32" version))
        (sha256
          (base32
            "0raaqkcjs2bk33cg9phsb45p505wwbpl4v09j3c089apajkfd929"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page
      "https://github.com/litex-hub/pythondata-cpu-lm32")
    (synopsis
      "Python module containing verilog files for LatticeMico32 cpu.")
    (description
      "Python module containing verilog files for LatticeMico32 cpu.")
    (license #f)))

(define-public python-pythondata-cpu-picorv32
  (package
    (name "python-pythondata-cpu-picorv32")
    (version "1.0.post134")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pythondata-cpu-picorv32" version))
        (sha256
          (base32
            "0nkmdnxkhl7q47wf8r4czyrkvyp6khv1vi5rngcdfxyshajlzpc8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page
      "https://github.com/litex-hub/pythondata-cpu-picorv32")
    (synopsis
      "Python module containing verilog files for PicoRV32 cpu.")
    (description
      "Python module containing verilog files for PicoRV32 cpu.")
    (license #f)))

(define-public python-pythondata-cpu-mor1kx
  (package
    (name "python-pythondata-cpu-mor1kx")
    (version "5.0.post182")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pythondata-cpu-mor1kx" version))
        (sha256
          (base32
            "19fqjh837cnrc87w04xl41nkwm2d7vrmqz1v6nlgb4ima059i7dk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page
      "https://github.com/litex-hub/pythondata-cpu-mor1kx")
    (synopsis
      "Python module containing verilog files for OpenRISC1000 cpu.")
    (description
      "Python module containing verilog files for OpenRISC1000 cpu.")
    (license #f)))

(define-public python-pythondata-cpu-minerva
  (package
    (name "python-pythondata-cpu-minerva")
    (version "0.0.post194")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pythondata-cpu-minerva" version))
        (sha256
          (base32
            "18r6bhm0xcmgrvsj2m0ds58zxqxi3jmj96qii54jxyhjliawmd0z"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (propagated-inputs
     `(("python-nmigen" ,python-nmigen)
       ("yosys" ,yosys-git)))
    (home-page
      "https://github.com/litex-hub/pythondata-cpu-minerva")
    (synopsis
      "Python module containing sources files for Minerva cpu.")
    (description
      "Python module containing sources files for Minerva cpu.")
    (license #f)))

(define-public litedram
  (package
    (name "litedram")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/enjoy-digital/litedram")
             (commit "203cc73cebec56faa0ed5c8900e15ac2c1dfe32b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16zds51178aabb6qx8p9zx09qkbxfd3wrcdb5q52lqw220r97jfi"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-migen" ,python-migen)))
    (home-page
     "https://github.com/enjoy-digital/litedram")
    (synopsis
     "Small footprint and configurable DRAM core")
    (description
     "Small footprint and configurable DRAM core")
    (license #f)))

(define-public liteeth
  (package
    (name "liteeth")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/enjoy-digital/liteeth")
             (commit "2a8cac96babce6a2eb1d02576ee49542c56ca4ad")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vmpf4rnh8cirns37cqkprn2l9bhgzxwl1if8wiabhwy96an4z0r"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (propagated-inputs
     `(("python-migen" ,python-migen)))
    (home-page
     "https://github.com/enjoy-digital/liteeth")
    (synopsis
     "Small footprint and configurable Ethernet core")
    (description
     "Small footprint and configurable Ethernet core")
    (license #f)))

(define-public liteiclink
  (package
    (name "liteiclink")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/enjoy-digital/liteiclink")
             (commit "3d8ecdbcf9f0260292221ff63b0ad3f5e409a955")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0blvgg26pf174nl8gi91a3aahalav0l4fcqdyphhc2i49iz63rnr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page
     "https://github.com/enjoy-digital/liteiclink")
    (synopsis
     "Small footprint and configurable Inter-Chip communication cores")
    (description
     "Small footprint and configurable Inter-Chip communication cores")
    (license #f)))

(define-public litepcie
  (package
    (name "litepcie")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/enjoy-digital/litepcie")
             (commit "68334fd93d078c77521a8000cb41311ab360a256")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l9jxm10171cxjmyd887ng158kalk6a384cra6n90s5ph0g3fxpx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)))
    (home-page
     "https://github.com/enjoy-digital/litepcie")
    (synopsis
     "Small footprint and configurable PCIe core")
    (description
     "Small footprint and configurable PCIe core")
    (license #f)))

(define-public litex-boards
  (package
    (name "litex-boards")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/litex-hub/litex-boards")
             (commit "615b97e205104296323049d3b752ad57d2dd2ab7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1si75npjni9q2j13pkbdwgcsmj7vvbq265mqm52k91s3qm4lk3p5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (home-page
     "https://github.com/litex-hub/litex-boards")
    (synopsis
     "LiteX boards files")
    (description
     "LiteX boards files")
    (license #f)))

(define-public litex
  (package
    (name "litex")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri  (git-reference
              (recursive? #t)
              (url "https://github.com/enjoy-digital/litex")
              ;; (commit "79ac09316ae00714f8d53d91bb7ab3419f2f74b2")

             ;; this is the commit hardcoded into symbiflow atm
              (commit "32989c17b66e81bd895f7812fa068938cee8040c")))

       (file-name (git-file-name name version))
       (sha256
        (base32 "1z7zf7x9nj6qk28kd0r2zwxr2qwl3bgknx9j0rvhsn26d77b8zrw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (propagated-inputs
     `(("python-pythondata-software-compiler-rt" ,python-pythondata-software-compiler-rt)
       ("python-requests" ,python-requests)
       ("python-pyserial" ,python-pyserial)
       ("python-migen" ,python-migen)
       ("python-pythondata-cpu-minverva" ,python-pythondata-cpu-minerva)
       ("python-pythondata-cpu-mor1kx" ,python-pythondata-cpu-mor1kx)
       ("python-pythondata-cpu-vexriscv" ,python-pythondata-cpu-vexriscv)
       ("python-pythondata-cpu-lm32" ,python-pythondata-cpu-lm32)
       ("python-pythondata-cpu-picorv32" ,python-pythondata-cpu-picorv32)
       ("litedram" ,litedram)
       ("liteeth" ,liteeth)
       ("litepcie" ,litepcie)
       ("liteiclink" ,liteiclink)
       ("litex-boards" ,litex-boards)))
    (home-page
     "https://github.com/enjoy-digital/litex")
    (synopsis
     "LiteX - Build your hardware, easily!")
    (description
     "LiteX - Build your hardware, easily!")
    (license #f)))

(define-public symbiflow-arch-defs
  (let*
      ((commit "958839cea6a3d1e839b20eaf021841fa3bb4c884")
       (version (string-append "0.0-g" (string-take commit 9))))
    (package
      (name "symbiflow-arch-defs")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (recursive? #t)
               (url "https://github.com/symbiflow/symbiflow-arch-defs")
               (commit commit)))
         (file-name (git-file-name name version))
         (patches '("symbiflow_arch_defs.patch"))
         (sha256
          (base32 "1g96dj429ydckyn306rsyzilw0fshhrfv3iy3wqck7cb0i297kvi"))))
      (arguments
       `(#:configure-flags
         (list (string-append "-DPRJXRAY_DB_DIR=" (assoc-ref %build-inputs "prjxray-db")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-makefile-and-make-writable
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((yosys (assoc-ref inputs "yosys"))
                      (openocd (assoc-ref inputs "openocd"))
                      (vpr (assoc-ref inputs "vpr")))
                 (substitute* "CMakeLists.txt"
                   (("set\\(YOSYS_DATADIR .*\\)") (string-append "set(YOSYS_DATADIR " yosys "/share/yosys)"))
                   (("set\\(OPENOCD_DATADIR .*\\)") (string-append "set(OPENOCD_DATADIR " openocd "/share/openocd)"))
                   (("set\\(VPR_CAPNP_SCHEMA_DIR .*\\)") (string-append "set(VPR_CAPNP_SCHEMA_DIR " vpr "/capnp)"))))))
           (add-after 'unpack 'set-cache-dir
             (lambda _
               (setenv "XDG_CACHE_HOME" ".cache"))))))
      (inputs `(("vpr", vtr-verilog-to-routing) ("yapf" ,python-yapf) ("python" ,python)
                ("yosys" ,yosys-for-symbiflow) ("node" ,node-lts) ("pytest" ,python-pytest)
                ("xmllint" ,libxml2) ("openocd" ,openocd) ("icestorm" ,guix:icestorm)
                ("tinyfpgab" ,python-tinyfpgab) ("tinyprog" ,python-tinyprog)
                ("flake8" ,python-flake8) ("iverilog" ,guix:iverilog)
                ("xcfasm" ,python-symbiflow-xc-fasm) ("qlfasm" ,python-quicklogic-fasm)
                ("qlf_fasm", python-ql-fasm) ("python-quicklogic-timings-importer" ,python-quicklogic-timings-importer)
                ("prjxray" ,prjxray) ("prjxray-db" ,prjxray-db) ("python-simplejson" ,python-simplejson)
                ("python-intervaltree" ,python-intervaltree) ("python-pyyaml" ,python-pyyaml)
                ("fusesoc" ,fusesoc) ("python-mako" ,python-mako) ("python-lxml" ,python-lxml)
                ("python-progressbar2" ,python-progressbar2) ("python-symbiflow-v2x", python-symbiflow-v2x)
                ("python-sdf-timing" ,python-sdf-timing) ("python-termcolor" ,python-termcolor)
                ("litex" ,litex) ("litex" ,litex-boards)
                ("riscv-gcc" ,(cross-gcc "riscv64-linux-gnu"))
                ("riscv-binutils" ,(cross-binutils "riscv64-linux-gnu"))))
      (home-page "https://symbiflow.github.io/")
      (build-system cmake-build-system)
      (synopsis "FOSS architecture definitions of FPGA hardware useful for doing PnR device generation.")
      (description "FOSS architecture definitions of FPGA hardware useful for doing PnR device generation.")
      (license #f))))
