(define-module (vup python-xyz)
  #:use-module (guix utils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix-science packages python)
  #:use-module (vup fpga)
  #:use-module (vup linux)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public python-pyvcd
  (package
    (name "python-pyvcd")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvcd" version))
       (sha256
        (base32 "15nnydvr1a4ykh8cagi484sfgvdg0dnjxaw6c0ivhjbrbblpaqnw"))))
    (build-system python-build-system)
    (inputs (list))
    (propagated-inputs (list))
    (home-page "http://pyvcd.readthedocs.io/en/latest/")
    (synopsis "Python VCD file support.")
    (description "Python VCD file support.")
    (license license:expat)))

(define-public python-orthopy
  (package
    (name "python-orthopy")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://zenodo.org/records/5751934/files/nschloe/orthopy-v"
             version ".zip"))
       (sha256
        (base32 "0ghcahjy7cjx95hdccbqvcds3vmcj3gpr852fqy92czfsnpc0lna"))))
    (build-system pyproject-build-system)
    (propagated-inputs `(("python-numpy" ,python-numpy)
                         ("python-scipy" ,python-scipy)
                         ("python-sympy" ,python-sympy)
                         ("python-importlib-metadata" ,python-importlib-metadata)))
    (native-inputs `(("python-setuptools" ,python-setuptools)
                     ("python-wheel" ,python-wheel)
                     ("unzip" ,unzip)))
    (home-page "https://github.com/nschloe/orthopy")
    (synopsis "Tools for orthogonal polynomials, Gaussian quadrature")
    (description "Tools for orthogonal polynomials, Gaussian quadrature")
    (license #f)
    (arguments
     '(#:tests? #f))))

(define-public python-ndim
  (package
    (name "python-ndim")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sigma-py/ndim/archive/refs/tags/v" version
             ".tar.gz"))
       (sha256
        (base32 "0z9pvj79n72x4ls9ndq3dm5znwflk38pkrd05afh5algakvh18wm"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-sympy))
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/nschloe/ndim")
    (synopsis "Compute multidimensional volumes and monomial integrals")
    (description "Compute multidimensional volumes and monomial integrals")
    (license #f)
    (arguments
     '(#:tests? #f))))

(define-public python-quadpy
  (package
    (name "python-quadpy")
    (version "0.16.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://zenodo.org/records/5541216/files/nschloe/quadpy-v"
             version ".zip"))
       (sha256
        (base32 "1f989dipv7lqxvalfrvvlmhlxyl67a87lavyyqrr1mh88glhl592"))))
    (build-system pyproject-build-system)
    (propagated-inputs `(("python-numpy" ,python-numpy)
                         ("python-orthopy" ,python-orthopy)
                         ("python-scipy" ,python-scipy)
                         ("python-sympy" ,python-sympy)
                         ("python-sympy" ,python-ndim)))
    (native-inputs `(("python-setuptools" ,python-setuptools)
                     ("python-wheel" ,python-wheel)
                     ("unzip" ,unzip)))
    (home-page "https://github.com/nschloe/quadpy")
    (synopsis "Numerical integration, quadrature for various domains")
    (description "Numerical integration, quadrature for various domains")
    (license #f)
    (arguments
     '(#:tests? #f))))

(define-public python-xarray
  (package
    (name "python-xarray")
    (version "2025.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xarray" version))
       (sha256
        (base32 "0hj3hd55sf380j6wqnngvakqf2rirpnc55xj66x98am91k812c0s"))))
    (build-system python-build-system)
    (propagated-inputs (list python-numpy python-packaging python-pandas))
    (arguments
     `(#:tests? #f)) ;no tests
    (home-page "https://github.com/pydata/xarray")
    (synopsis "N-D labeled arrays and datasets in Python")
    (description "N-D labeled arrays and datasets in Python")
    (license #f)))

(define-public python-sparse
  (package
    (name "python-sparse")
    (version "0.15.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sparse" version))
       (sha256
        (base32 "111bqz2xqr17rrc7svd20z94xf3zkfs9anjvzpr683zz4iywbcfl"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numba python-numpy python-scipy))
    (native-inputs (list python-dask
                         python-pre-commit
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-setuptools-scm-next
                         python-wheel))
    ;; (arguments '(#:tests? #f))
    (home-page "https://github.com/pydata/sparse/")
    (synopsis "Sparse n-dimensional arrays")
    (description "Sparse n-dimensional arrays")
    (license #f)))

(define-public python-colorhash
  (package
    (name "python-colorhash")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colorhash" version))
       (sha256
        (base32 "0651wh4iwr9p1vf4hl4fnz1n4q2p6p67m0xz8n9kn7rypnk1w5zq"))))
    (arguments
     `(#:tests? #f)) ;no tests
    (build-system pyproject-build-system)
    (native-inputs (list python-black python-hatchling python-pillow
                         python-pre-commit python-pytest))
    (home-page "https://bitbucket.org/fk/python-color-hash")
    (synopsis "Generate a color based on a value")
    (description "Generate a color based on a value")
    (license license:expat)))

; used by symbiflow
(define-public python-textx
  (package
    (name "python-textx")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "textX" version))
       (sha256
        (base32 "1ymhz9lf7nzwmpvfgd2l73vsahjw4xq3fc7hya5iz9x41q4pvyz2"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel python-click))
    (propagated-inputs (list python-arpeggio))
    (home-page "https://github.com/textX/textX")
    (synopsis "Meta-language for DSL implementation inspired by Xtext")
    (description "Meta-language for DSL implementation inspired by Xtext")
    (license license:expat)))

(define (make-linux-module-builder linux)
  (package
    (inherit linux)
    (name (string-append (package-name linux) "-module-builder"))
    (inputs `(("linux" ,linux)))
    (arguments
     (substitute-keyword-arguments (package-arguments linux)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda _
                (invoke "make" "modules_prepare")))
            (delete 'strip) ;faster
            (replace 'install
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((out-lib-build (string-append #$output
                                                    "/lib/modules/build")))
                  ;; Delete some huge items that we probably don't need.
                  ;; TODO: Only preserve the minimum, i.e. [Kbuild], Kconfig,
                  ;; scripts, include, ".config".
                  (copy-recursively "." out-lib-build)
                  (for-each (lambda (name)
                              (when (file-exists? name)
                                (delete-file-recursively name)))
                            (map (lambda (name)
                                   (string-append out-lib-build "/" name))
                                 '("arch" ;137 MB
                                   ;; "tools"       ; 44 MB built by our 'build phase
                                   "tools/testing" ;14 MB
                                   "tools/perf" ;17 MB
                                   "drivers" ;600 MB
                                   "Documentation" ;52 MB
                                   "fs" ;43 MB
                                   "net" ;33 MB
                                   "samples" ;2 MB
                                   "sound"))) ;40 MB
                  ;; Reinstate arch/**/dts since "scripts/dtc" depends on it.
                  ;; Reinstate arch/**/include directories.
                  ;; Reinstate arch/**/Makefile.
                  ;; Reinstate arch/**/module.lds.
                  (for-each (lambda (name)
                              (mkdir-p (dirname (string-append out-lib-build
                                                               "/" name)))
                              (copy-recursively name
                                                (string-append out-lib-build
                                                               "/" name)))
                            (append (find-files "arch" "^(dts|include)$"
                                                #:directories? #t)
                                    (find-files "arch"
                                                "^(Makefile|module.lds)$")))
                  (let* ((linux #$(this-package-input "linux")))
                    (install-file (string-append linux "/System.map")
                                  out-lib-build)
                    (let ((source (string-append linux "/Module.symvers")))
                      (when (file-exists? source)
                        (install-file source out-lib-build)))))))))))))

(define-public python-chipsec
  (package
    (name "python-chipsec")
    (version "1.13.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chipsec/chipsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "176snnph7xc6z5s3a5aqrc5rxy95ly097maz8mc755nl2rhlga97"))))
    (build-system python-build-system)
    (inputs `(("linux" ,linux-nonfree-stable)
              ("nasm" ,nasm)
              ("linux-module-builder" ,(make-linux-module-builder
                                        linux-nonfree-stable))))
    (home-page "https://github.com/chipsec/chipsec")
    (synopsis "CHIPSEC: Platform Security Assessment Framework")
    (description "CHIPSEC: Platform Security Assessment Framework")
    (license #f)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'set-kernel-src-dir
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((kernel-dir (assoc-ref inputs
                                                    "linux-module-builder"))
                             (kernel-src-dir (string-append kernel-dir
                                              "/lib/modules/build")))
                        (setenv "KSRC" kernel-src-dir))))
                  (delete 'check))))))
; broken for some reason

(define-public python-pdm-backend-2.3
  (package
    (name "python-pdm-backend")
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pdm_backend" version))
       (sha256
        (base32 "156m4ljaa4akksid3r69xdfsn6zhvhlb51msl3bm6hy8iri6yqd8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Depends on pytest, which we cannot import into this module.
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'set-pythonpath
                     (lambda _
                       (setenv "PYTHONPATH"
                               (string-append (getcwd) "/src")))))))
    (home-page "https://pdm-backend.fming.dev/")
    (synopsis "PEP 517 build backend for PDM")
    (description
     "PDM-Backend is a build backend that supports the latest packaging
standards, which includes PEP 517, PEP 621 and PEP 660.")
    (license license:expat)))

(define-public python-jschon
  (package
    (name "python-jschon")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jschon" version))
       (sha256
        (base32 "17qlpwaq5xv5szm93bpkg9waf166bvbixf96sw1llsgin7m0pjn0"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-rfc3986))
    (native-inputs (list python-coverage
                         python-hypothesis
                         python-pytest
                         python-pytest-benchmark
                         python-pytest-httpserver
                         python-requests
                         python-setuptools
                         python-wheel
                         python-tox))
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/marksparkza/jschon")
    (synopsis "A JSON toolkit for Python developers.")
    (description "This package provides a JSON toolkit for Python developers.")
    (license license:expat)))

(define (pdm-scm-version-setter v)
  `(lambda _
     (setenv "PDM_BUILD_SCM_VERSION"
             ;; local version in "+" seperated in python
             ,(string-replace-substring v "-" "+")) #t))

(define-public python-amaranth
  (package
    (name "python-amaranth")
    (version "0.6.0.dev0-1.590cba1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/amaranth-lang/amaranth")
             (commit "590cba1d6c00dffba7abb5f399680ac50e252adf")))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (sha256
        (base32 "0wy396faclmxw4wvg0x59w81sjg407wlyy3qnrghkr4vd5id9609"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pdm-backend-2.3 python-pytest))
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'pretend-version
                     #$(pdm-scm-version-setter version)))))
    (inputs (list yosys-git symbiyosys))
    (propagated-inputs (list python-jinja2 python-pyvcd python-paramiko
                             python-jschon))
    (home-page "https://amaranth-lang.org/")
    (synopsis "Python toolbox for building complex digital hardware")
    (description "Python toolbox for building complex digital hardware")
    (license license:bsd-3)))

(define-public python-amaranth-boards
  (package
    (name "python-amaranth-boards")
    (version "0.0-0.9d97c48")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/amaranth-lang/amaranth-boards")
             (commit "9d97c4816288c9c2cc304d9280c2c63178d50d2f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12lb7r8snkfp389cchzn63sj7vy6vlcgrk43wv797xx4v1cd9qkk"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pdm-backend-2.3))
    (propagated-inputs (list python-amaranth))
    (arguments
     (list
      #:tests? #f ;broken atm
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'pretend-version
                     #$(pdm-scm-version-setter version)))))
    ;; (arguments `(#:tests? #f))                  ; kind of super broken?
    (home-page "https://amaranth-lang.org/")
    (synopsis "Board and connector definitions for Amaranth")
    (description "Board and connector definitions for Amaranth")
    (license license:bsd-3)))

(define-public python-amaranth-stdio
  (package
    (name "python-amaranth-stdio")
    (version "0.0-0.618a13f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/amaranth-lang/amaranth-stdio")
             (commit "618a13fb7108f69197d698df4d64c203553deaae")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xmzn1rz66cc58ys38hggqkv22wain3if63p0f8id9k58sy8aja7"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pdm-backend-2.3))
    (propagated-inputs (list python-amaranth))
    (arguments
     (list
      #:tests? #f ;broken atm
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'pretend-version
                     #$(pdm-scm-version-setter version)))))
    (home-page "https://amaranth-lang.org/")
    (synopsis "Industry standard I/O for Amaranth")
    (description "Industry standard I/O for Amaranth")
    (license license:bsd-3)))

(define-public python-amaranth-soc
  (package
    (name "python-amaranth-soc")
    (version "0.1a-0.5c43cf5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/amaranth-lang/amaranth-soc")
             (commit "5c43cf58f15d9cd9c69ff83c97997708d386b2dc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pq5ww9kb1np8fgby683k37j7dqqhpvfil56qcpvlmz6z4gn7p53"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pdm-backend-2.3))
    (propagated-inputs (list python-amaranth))
    (arguments
     (list
      #:tests? #f ;broken atm
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'pretend-version
                     #$(pdm-scm-version-setter version)))))
    (home-page "https://amaranth-lang.org/")
    (synopsis "System on Chip toolkit for Amaranth")
    (description "System on Chip toolkit for Amaranth")
    (license license:bsd-3)))

(define-public symbiyosys
  (package
    (name "symbiyosys")
    (version "0.48-0.26b3874")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/YosysHQ/SymbiYosys")
             (commit "26b387466d224cd9e83a6f97d0b69d9dd59e56b5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18fs32k1lwj45hkr0varqgxbjms56m6lcr76xzx63djhiqbqfhvs"))))
    (inputs (list python yosys-git))
    (propagated-inputs (list yices python-click))
    (arguments
     `(#:make-flags `(,(string-append "PREFIX=" %output))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'fix-permissions
                    (lambda _
                      (for-each make-file-writable
                                (find-files "." ".*")) #t))
                  (add-before 'install 'patch-yosys
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (yosys (assoc-ref inputs "yosys")))
                        (substitute* '("sbysrc/sby.py" "sbysrc/sby_core.py")
                          (("##yosys-sys-path##")
                           (string-append
                            "sys.path += [p + \"/share/yosys/python3/\" for p in [\""
                            out "\", \"" yosys "\"]]"))
                          (("/usr/bin/env\", \"bash")
                           (which "bash")))) #t))
                  (delete 'configure) ;no configure
                  (delete 'build)))) ;no compilation
    
    (build-system gnu-build-system)
    (home-page "https://github.com/YosysHQ/SymbiYosys")
    (synopsis
     "SymbiYosys (sby) -- Front-end for Yosys-based formal verification flows")
    (description
     "SymbiYosys (sby) is a front-end driver program for Yosys-based formal hardware verification flows.")
    (license license:isc)))

(define-public python-bluepy
  (package
    (name "python-bluepy")
    (version "1.3.0-0.10f1cee")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edwios/bluepy")
             (commit "10f1cee90afb416f139949b86b491e4cfa98c886")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zgrpnjvi51xd0q2zb3pxpqsni2r59cvpl3f3i6xdi0lqk3drp8f"))))
    (build-system python-build-system)
    (home-page "https://github.com/IanHarvey/bluepy")
    (inputs `(("glib" ,glib)
              ("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'build 'set-HOME
                    (lambda _
                      (begin
                        (map (lambda (f)
                               (chmod f #o666))
                             (find-files "." ".*")) #t)))
                  (add-after 'unpack 'set-cc
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "CC" "gcc")))
                  (delete 'check)))) ;broken for some reason
    (synopsis "Python module for interfacing with BLE devices through Bluez")
    (description
     "Python module for interfacing with BLE devices through Bluez")
    (license #f)))

(define-public python-prompt-toolkit
  (package
    (name "python-prompt-toolkit")
    (version "3.0.48")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prompt_toolkit" version))
       (sha256
        (base32 "141xl3hkvmcljai6l804y75gahqica9vrga6wrsdz03s8yq3lqnn"))))
    (build-system python-build-system)
    (propagated-inputs (list python-wcwidth))
    (home-page "https://github.com/prompt-toolkit/python-prompt-toolkit")
    (synopsis
     "Library for building powerful interactive command lines in Python")
    (description
     "Library for building powerful interactive command lines in Python")
    (license #f)))

;; broken
;; (define-public python-pymodbus
;;   (package
;;     (name "python-pymodbus")
;;     (version "3.8.3")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (pypi-uri "pymodbus" version))
;;        (sha256
;;         (base32 "1j721y30bpfpw7jxd4dv9wrqik744ff92c782w7i9k63yxmii1nq"))))
;;     (build-system pyproject-build-system)
;;     (arguments
;;      '(#:tests? #f))
;;     (propagated-inputs (list python-pyserial
;;                              python-six
;;                              python-click
;;                              python-typer
;;                              python-prompt-toolkit
;;                              python-pygments
;;                              python-aiohttp
;;                              python-pyserial-asyncio))
;;     (home-page "https://github.com/riptideio/pymodbus/")
;;     (synopsis "A fully featured modbus protocol stack in python")
;;     (description "A fully featured modbus protocol stack in python")
;;     (license #f)))

;; (define-public python-solaredge-modbus
;;   (package
;;     (name "python-solaredge-modbus")
;;     (version "0.8.0")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (pypi-uri "solaredge_modbus" version))
;;        (sha256
;;         (base32 "0w1p953mhvyljx1nb0xi9lfn1rhlv6bgs9saa1rzznk81q1v7md5"))))
;;     (build-system python-build-system)
;;     (propagated-inputs (list python-pymodbus python-pyserial-asyncio))
;;     (home-page "https://github.com/nmakel/solaredge_modbus")
;;     (synopsis "SolarEdge Modbus parser library")
;;     (description "SolarEdge Modbus parser library")
;;     (license license:expat)))

(define-public python-openant
  (package
    (name "python-openant")
    (version "1.3.2-0.67e0225")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Tigge/openant")
             (commit "67e0225efd1cafd2a7b3bb5c52dd35382b97888c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kw0hg5ivr0rpiaj02s88m0cq3sjkjc74h3wdw17y8ghrc07mdac"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyusb))
    (native-inputs (list python-setuptools python-wheel python-pytest
                         python-black python-pylint))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'sanity-check)))) ;this seems bad
    ;; (arguments
    ;; '(#:phases (modify-phases %standard-phases
    ;; (add-before 'build 'patch-install
    ;; (lambda _
    ;; (substitute* '("setup.py")
    ;; (("install_udev_rules\\(True\\)")
    ;; "install_udev_rules(False)")) #t)))))
    (home-page "https://github.com/Tigge/openant")
    (synopsis "ANT and ANT-FS Python Library")
    (description "ANT and ANT-FS Python Library")
    (license #f)))

;; (define-public python-openant
;;   (package
;;     (name "python-openant")
;;     (version "1.3.2")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (pypi-uri "openant" version))
;;        (sha256
;;         (base32 "0zwrrkngcj2zxlpp9xygcv13hjwp0ba600nihilqbr3y6m5acvyy"))))
;;     (build-system pyproject-build-system)
;;     (propagated-inputs (list python-pyusb))
;;     (native-inputs (list python-black python-pylint python-pytest
;;                          python-setuptools python-wheel))
;;     (home-page "")
;;     (synopsis "ANT, ANT-FS and ANT+ Python Library")
;;     (description "ANT, ANT-FS and ANT+ Python Library.")
;;     (license #f)))

(define-public python-openant/udev
  (package
    (inherit python-openant)
    (name "python-openant-udev")
    (build-system trivial-build-system)
    (propagated-inputs '())
    (native-inputs `(("source" ,(package-source python-openant))))
    (arguments
     '(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (install-file (string-append (assoc-ref %build-inputs
                                                           "source")
                                  "/resources/42-ant-usb-sticks.rules")
                                 (string-append %output "/lib/udev/rules.d")))))))

;; broken
;; (define-public python-antfs-cli
;;   (package
;;     (name "python-antfs-cli")
;;     (version "0.0-0.f80ebca")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/Tigge/antfs-cli")
;;              (commit "f80ebca523353a074129efd05fdf9b28f35cf674")))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "04fp1i9dnwniplq8579nc1gk6393241yz3wjy5pr3iblajg9q3h6"))))
;;     (build-system python-build-system)
;;     (propagated-inputs (list python-openant))
;;     (home-page "https://github.com/Tigge/antfs-cli")
;;     (synopsis
;;      "Extracts FIT files from ANT-FS based sport watches such as Garmin Forerunner 60, 405CX, 310XT, 610 and 910XT.")
;;     (description
;;      "Extracts FIT files from ANT-FS based sport watches such as Garmin Forerunner 60, 405CX, 310XT, 610 and 910XT.")
;;     (license #f)))

(define-public python-flameprof
  (package
    (name "python-flameprof")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flameprof" version))
       (sha256
        (base32 "18vmg22j1m22xz486kak4y7175nvv520z970y4jadfybj10nvj6v"))))
    (build-system python-build-system)
    (home-page "https://github.com/baverman/flameprof/")
    (synopsis "cProfile flamegraph generator")
    (description "cProfile flamegraph generator")
    (license license:expat)))

(define-public python-descartes
  (package
    (name "python-descartes")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "descartes" version))
       (sha256
        (base32 "0nq36w9ylvfwmwn5qd9c8fsp2jzsqpmy4xcr6pzxcpmg8qhm0nhk"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-matplotlib" ,python-matplotlib)
                         ("python-shapely" ,python-shapely)))
    (arguments
     `(#:tests? #f))
    (home-page "http://bitbucket.org/sgillies/descartes/")
    (synopsis "Use geometric objects as matplotlib paths and patches")
    (description "Use geometric objects as matplotlib paths and patches")
    (license license:bsd-3)))

(define-public python-loky
  (package
    (name "python-loky")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "loky" version))
       (sha256
        (base32 "0lq753l0q5lqskzzry255k5w99bby2wf7b42r2ci4c4cwq6kbnv6"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-cloudpickle" ,python-cloudpickle)))
    (native-inputs (list))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/joblib/loky/")
    (synopsis
     "A robust implementation of concurrent.futures.ProcessPoolExecutor")
    (description
     "A robust implementation of concurrent.futures.ProcessPoolExecutor")
    (license license:bsd-3)))

(define-public python-xdoctest-1.2
  (package
    (name "python-xdoctest")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xdoctest" version))
       (sha256
        (base32 "0qsqxp9bpjzg8kb4k8n3sppzbzdr6l6n0vkm7z9qir4ii5nwmkyq"))))
    (build-system pyproject-build-system)
    (native-inputs (list cmake
                         ninja
                         pybind11
                         python-pytest
                         python-pytest-cov
                         python-scikit-build
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/Erotemic/xdoctest")
    (synopsis "A rewrite of the builtin doctest module")
    (description
     "This package provides a rewrite of the builtin doctest module.")
    (license #f)))

(define-public python-ubelt-fixed
  (package
    (inherit python-ubelt)
    (native-inputs (modify-inputs (package-native-inputs python-ubelt)
                     (replace "python-xdoctest" python-xdoctest-1.2)))))

(define-public python-line-profiler
  (package
    (name "python-line-profiler")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "line_profiler" version))
       (sha256
        (base32 "15hs8pmv7pcilnhhp0l5pamjihmh7zlnvvpsnf046lbnz0jhzq89"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-coverage
                         python-cython-3
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-ubelt-fixed
                         python-wheel
                         python-xdoctest-1.2))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/pyutils/line_profiler")
    (synopsis "Line-by-line profiler")
    (description "Line-by-line profiler")
    (license license:bsd-3)))

(define-public python-snakeviz
  (package
    (name "python-snakeviz")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "snakeviz" version))
       (sha256
        (base32 "10vx7b0rn3gams2qk0dj5xkbiavh8x13hykm2kzk581lirpqq0h8"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-tornado))
    (native-inputs (list python-setuptools python-wheel python-pytest
                         python-requests python-ipython))
    (home-page "https://github.com/jiffyclub/snakeviz")
    (synopsis "A web-based viewer for Python profiler output")
    (description "A web-based viewer for Python profiler output")
    (license #f)))

;; (define-public pmbootstrap
;;   (package
;;     (name "pmbootstrap")
;;     (version "7954019f0ca6106a0fc20deb472ce56392615b11")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://gitlab.com/postmarketOS/pmbootstrap")
;;              (commit version)))
;;        (sha256
;;         (base32 "1ij6ad387k9iajicscpv4shjhz9q8w54yp29vvznd51c114dlnxa"))))
;;     (build-system pyproject-build-system)
;;     (arguments
;;      (list
;;       #:tests? #f ;TODO: many test fail
;;       #:phases #~(modify-phases %standard-phases
;;                    (add-after 'install 'fix-paths
;;                      (lambda* _
;;                        (let ((git (string-append #$git "/bin/"))
;;                              (procps (string-append #$procps "/bin"))
;;                              (openssl (string-append #$openssl "/bin"))
;;                              (sudo "/run/setuid-programs"))
;;                          (wrap-program (string-append #$output
;;                                                       "/bin/pmbootstrap")
;;                            `("PATH" ":" suffix
;;                              ,(list git procps openssl sudo))))))
;;                    (replace 'check
;;                      (lambda* (#:key tests? #:allow-other-keys)
;;                        (when tests?
;;                          (invoke "pytest")))))))
;;     (native-inputs (list python-setuptools python-wheel python-pytest python-pyopenssl))
;;     (inputs (list git procps openssl sudo))
;;     (home-page "https://postmarketos.org")
;;     (synopsis "Build and flash tool for postmarketOS")
;;     (description
;;      "Bootstrap program that abstracts everything in chroots and therefore
;; basically runs on top of any Linux distribution. Features:
;; @enumerate
;; @item chroot setup (distro-independent QEMU user emulation
;; @item clean chroot shutdown (umount) and zapping
;; @item build software as packages
;; @item cross-compile all armhf-packages
;; @item effective caching out of the box (survives chroot zaps)
;; @item installation targets
;; @item flasher abstractions
;; @item logging
;; @item security
;; @end enumerate")
;;     (license license:gpl3+)))

(define python-bitarray-3
  (package
    (name "python-bitarray")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bitarray" version))
       (sha256
        (base32 "00rbk5vnb7q3nfilgwy47psb1ajnmq6v45ksvxy8m0hd1z13s252"))))
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         ;; Step out of the source directory to avoid interference.
                         (with-directory-excursion "/tmp"
                           (invoke "python" "-c"
                                   "import bitarray; bitarray.test()"))))))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ilanschnell/bitarray")
    (synopsis "efficient arrays of booleans -- C extension")
    (description "efficient arrays of booleans -- C extension.")
    (license #f)))

(define-public python-bitstring
  (package
    (name "python-bitstring")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bitstring" version))
       (sha256
        (base32 "04q3xsdvhbs8fqgm86gximf3amhjfj769bnvd9qhhr8bw320p041"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-bitarray-3))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/scott-griffiths/bitstring")
    (synopsis "Simple construction, analysis and modification of binary data.")
    (description
     "Simple construction, analysis and modification of binary data.")
    (license #f)))

(define-public python-huffman
  (package
    (name "python-huffman")
    (version "0.1.2-0.bfad004")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nicktimko/huffman")
             (commit "bfad004ce7951750cc4536ae4466f87afa0f5e5d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g6ahq3pycbcayfinmm50c7j37k6vps64mfdshlbhbqx6wm9vpxl"))))
    (build-system python-build-system)
    (home-page "https://github.com/nicktimko/huffman")
    (synopsis "Generate Huffman codes with Python")
    (description "Generate Huffman codes with Python")
    (license #f)))

(define-public python-demjson
  (package
    (name "python-demjson")
    (version "2.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "demjson" version))
       (sha256
        (base32 "0ygbddpnvp5lby6mr5kz60la3hkvwwzv3wwb3z0w9ngxl0w21pii"))))
    (build-system python-build-system)
    (propagated-inputs (list python-setuptools-57))
    (arguments
     `(#:tests? #f))
    (home-page "http://deron.meranda.us/python/demjson/")
    (synopsis
     "encoder, decoder, and lint/validator for JSON (JavaScript Object Notation) compliant with RFC 7159")
    (description
     "encoder, decoder, and lint/validator for JSON (JavaScript Object Notation) compliant with RFC 7159")
    (license #f)))

(define-public python-stdlib-list
  (package
    (name "python-stdlib-list")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stdlib-list" version))
       (sha256
        (base32 "17vdn4q0sdlndc2fr9svapxx6366hnrhkn0fswp1xmr0jxqh7rd1"))))
    (build-system python-build-system)
    (native-inputs (list python-sphinx))
    (home-page "https://github.com/jackmaney/python-stdlib-list")
    (arguments
     `(#:tests? #f))
    (synopsis "A list of Python Standard Libraries (2.6-7, 3.2-9).")
    (description
     "This package provides a list of Python Standard Libraries (2.6-7, 3.2-9).")
    (license license:expat)))

(define-public python-pydeps
  (package
    (name "python-pydeps")
    (version "1.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydeps" version))
       (sha256
        (base32 "0h869792bllfvaq8r17xw9jc7wsdygxkh6a2w04h8g7rwy1rxih6"))))
    (build-system python-build-system)
    (propagated-inputs (list python-stdlib-list))
    (native-inputs (list python-pytest))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/thebjorn/pydeps")
    (synopsis "Display module dependencies")
    (description "Display module dependencies")
    (license license:bsd-3)))

(define-public python-openstep-parser
  (package
    (name "python-openstep-parser")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openstep_parser" version))
       (sha256
        (base32 "0n0npqm30rw7xxzzf533g1c7zyh93xy6c3ybf321iiwaqzcpyg7h"))))
    (build-system python-build-system)
    (native-inputs (list python-coverage python-nose))
    (home-page "http://github.com/kronenthaler/openstep-parser")
    (synopsis "OpenStep plist reader into python objects")
    (description "OpenStep plist reader into python objects")
    (license license:bsd-3)))

(define-public python-pbxproj
  (package
    (name "python-pbxproj")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pbxproj" version))
       (sha256
        (base32 "1cnkhlyv7ipf1w8rpihcc5ipqmn26f75d4dfxb3jp0sjn0mxzscx"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-docopt" ,python-docopt)
                         ("python-openstep-parser" ,python-openstep-parser)))
    (arguments
     `(#:tests? #f))
    (home-page "http://github.com/kronenthaler/mod-pbxproj")
    (synopsis "XCode Project manipulation library for Python")
    (description "XCode Project manipulation library for Python")
    (license license:expat)))

(define-public python-lxml-4.9
  (package
    (inherit python-lxml)
    (version "4.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lxml" version))
       (sha256
        (base32 "0rsvhd03cv7fczd04xqf1idlnkvjy0hixx2p6a5k6w5cnypcym94"))))))

(define-public python-docx2python
  (package
    (name "python-docx2python")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docx2python" version))
       (sha256
        (base32 "07ms7i5r1zmxyxlifabw2ywawkaxiwf80nsq2f7ya3zng0rnvas6"))))
    (build-system python-build-system)
    (propagated-inputs (list python-lxml-4.9))
    (home-page "")
    (synopsis "Extract content from docx files")
    (description "Extract content from docx files")
    (license license:expat)))

(define-public python-nose-cov
  (package
    (name "python-nose-cov")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nose-cov" version))
       (sha256
        (base32 "04j4fw01bv648gimqqj4z88606lcczbm1k326agcc74gb4sh7v4b"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-cov-core" ,python-cov-core)
                         ("python-nose" ,python-nose)))
    (home-page "http://bitbucket.org/memedough/nose-cov/overview")
    (synopsis
     "nose plugin for coverage reporting, including subprocesses and multiprocessing")
    (description
     "nose plugin for coverage reporting, including subprocesses and multiprocessing")
    (license license:expat)))

(define-public python-influxdb
  (package
    (name "python-influxdb")
    (version "5.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "influxdb" version))
       (sha256
        (base32 "0w49230p935a90593jbv8qam9fzvrjs2xqdfx63ds4ip0kv4gijq"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-msgpack python-dateutil python-pytz
                             python-requests python-six))
    (native-inputs (list python-mock
                         python-nose
                         python-nose-cov
                         python-requests-mock
                         python-setuptools
                         python-setuptools
                         python-wheel))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/influxdb/influxdb-python")
    (synopsis "InfluxDB client")
    (description "@code{InfluxDB} client.")
    (license license:expat)))

(define-public python-reactivex
  (package
    (name "python-reactivex")
    (version "4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "reactivex" version))
       (sha256
        (base32 "1s041rjqmiyqr4qrjvzj06isg3wczr9scj43vxv93ar221cyc4p9"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-typing-extensions))
    (native-inputs (list python-poetry-core python-setuptools python-wheel
                         python-pytest python-pytest-asyncio))
    (home-page "http://reactivex.io")
    (synopsis "ReactiveX (Rx) for Python")
    (description "@code{ReactiveX} (Rx) for Python.")
    (license license:expat)))

(define-public python-randomize
  (package
    (name "python-randomize")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "randomize" version))
       (sha256
        (base32 "0srvk1rvlppvhpzj7p2dims1nx8hlycrqad0j748d5wgxlsp95zy"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel python-nose))
    (home-page "https://github.com/nloadholtes/nose-randomize")
    (synopsis
     "Randomize the order of the tests within a unittest.TestCase class")
    (description
     "Randomize the order of the tests within a unittest.@code{TestCase} class.")
    (license license:lgpl2.0)))

(define-public python-dateutil
  (package
    (name "python-dateutil")
    (version "2.9.0.post0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-dateutil" version))
       (sha256
        (base32 "1lqak92ka6p96wjbf3zr9691gm7b01g7s8c8af3wvqd7ilh59p9p"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-six))
    (native-inputs (list python-setuptools
                         python-setuptools-scm
                         python-wheel
                         python-pytest
                         python-pytest-cov
                         python-freezegun))
    (home-page "https://github.com/dateutil/dateutil")
    (synopsis "Extensions to the standard Python datetime module")
    (description "Extensions to the standard Python datetime module.")
    (license #f)))

(define-public python-influxdb-client
  (package
    (name "python-influxdb-client")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "influxdb_client" version))
       (sha256
        (base32 "0nc5gksis32bc34ajswr255a0bl7m5p85qik7x2nnavxzxg5nka1"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-certifi python-dateutil python-reactivex
                             python-setuptools python-urllib3))
    (native-inputs (list python-aioresponses
                         python-coverage
                         python-flake8
                         python-httpretty
                         python-jinja2
                         python-nose
                         python-pluggy
                         python-psutil
                         python-py
                         python-pytest
                         python-pytest-cov
                         python-pytest-timeout
                         python-randomize
                         python-setuptools
                         python-sphinx
                         python-sphinx-rtd-theme
                         python-wheel))
    (home-page "https://github.com/influxdata/influxdb-client-python")
    (synopsis "InfluxDB 2.0 Python client library")
    (description "@code{InfluxDB} 2.0 Python client library.")
    (license #f)))

(define-public python-fpzip
  (package
    (name "python-fpzip")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fpzip" version))
       (sha256
        (base32 "090c022mv8lyhzb1j5zabzx506clkamf4d4ws5fxfalnkx1ncdr2"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-pip
                         python-pbr
                         python-numpy
                         python-cython
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/seung-lab/fpzip/")
    (synopsis
     "Numpy wrapper for fpzip algorithm (P. Lindstrom & M. Isenburg, 2006)")
    (description
     "Numpy wrapper for fpzip algorithm (P.  Lindstrom & M.  Isenburg, 2006)")
    (license #f)))

(define-public python-find-libpython
  (package
    (name "python-find-libpython")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "find_libpython" version))
       (sha256
        (base32 "1rh3dj81qwl1v7ckni12abgw2hbag79xx4km5lxmdnvx776wvya6"))))
    (build-system python-build-system)
    (home-page "https://github.com/ktbarrett/find_libpython")
    (synopsis
     "Finds the libpython associated with your environment, wherever it may be hiding")
    (description
     "Finds the libpython associated with your environment, wherever it may be hiding")
    (license license:expat)))

(define-public python-cocotb
  (package
    (name "python-cocotb")
    (version "1.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cocotb" version))
       (sha256
        (base32 "0lhnvfzzfkz41pchf0sg302ssj0bvipcafh861a4xcf13vsymkg4"))))
    (build-system python-build-system)
    (propagated-inputs (list python-find-libpython))
    (arguments
     `(#:tests? #f))
    (home-page "https://docs.cocotb.org")
    (synopsis
     "cocotb is a coroutine based cosimulation library for writing VHDL and Verilog testbenches in Python.")
    (description
     "cocotb is a coroutine based cosimulation library for writing VHDL and Verilog
testbenches in Python.")
    (license license:bsd-3)))

(define-public python-cocotb-bus
  (package
    (name "python-cocotb-bus")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cocotb-bus" version))
       (sha256
        (base32 "13dav1nwwy0mdlkvk4glm5nj1h1fzfrl2yw7r1lq9lha1r5sm5x1"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cocotb))
    (home-page "https://github.com/cocotb/cocotb-bus")
    (synopsis "")
    (description "")
    (license #f)))

(define-public python-cocotbext-axi
  (package
    (name "python-cocotbext-axi")
    (version "0.1.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cocotbext-axi" version))
       (sha256
        (base32 "14q5jqnqcz6q17j339s690f46in3qmdpql16d0bk7224z752vmiy"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cocotb python-cocotb-bus))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/alexforencich/cocotbext-axi")
    (synopsis "AXI, AXI lite, and AXI stream modules for cocotb")
    (description "AXI, AXI lite, and AXI stream modules for cocotb")
    (license license:expat)))

(define-public python-cocotbext-eth
  (package
    (name "python-cocotbext-eth")
    (version "0.1.22")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cocotbext-eth" version))
       (sha256
        (base32 "1csq5afcivl2scy3clf4fyzl5mr4niqaa6j0ncsjwv3pflkwqkr0"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cocotb python-cocotbext-axi))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/alexforencich/cocotbext-eth")
    (synopsis "Ethernet interface modules for cocotb")
    (description "Ethernet interface modules for cocotb")
    (license license:expat)))

(define-public python-pybtex-docutils
  (package
    (name "python-pybtex-docutils")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pybtex-docutils" version))
       (sha256
        (base32 "05m09byzhq8p1pax66g69z9n3l1jwfc3l1zh62ndb3s9dlxkbaj3"))))
    (build-system python-build-system)
    (propagated-inputs (list python-docutils python-pybtex))
    (home-page "https://github.com/mcmtroffaes/pybtex-docutils")
    (synopsis "A docutils backend for pybtex.")
    (description "This package provides a docutils backend for pybtex.")
    (license license:expat)))
(define-public python-sphinxcontrib-bibtex
  (package
    (name "python-sphinxcontrib-bibtex")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-bibtex" version))
       (sha256
        (base32 "1ahfn3szw9l80isps9w269v9maink6zjclw78gr89qp2n1fjxd3i"))))
    (build-system python-build-system)
    (propagated-inputs (list python-dataclasses
                             python-docutils
                             python-importlib-metadata
                             python-pybtex
                             python-pybtex-docutils
                             python-wheel
                             python-pytest-cov
                             python-sphinx))
    (home-page "https://github.com/mcmtroffaes/sphinxcontrib-bibtex")
    (synopsis "Sphinx extension for BibTeX style citations.")
    (description "Sphinx extension for BibTeX style citations.")
    (license license:bsd-3)))
(define-public python-tokenize-rt-5
  (package
    (name "python-tokenize-rt")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tokenize_rt" version))
       (sha256
        (base32 "0h07s44585m1f9mpdxgm82i61h8zhvm1s5w50hnk34c47q6bqq1i"))))
    (build-system python-build-system)
    (home-page "https://github.com/asottile/tokenize-rt")
    (synopsis "A wrapper around the stdlib `tokenize` which roundtrips.")
    (description
     "This package provides a wrapper around the stdlib `tokenize` which roundtrips.")
    (license license:expat)))
(define-public python-pyupgrade
  (package
    (name "python-pyupgrade")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyupgrade" version))
       (sha256
        (base32 "0kxiiangyliy8jfl8v735n63v124x6kjwjvilnaanz485mvf5hbf"))))
    (build-system python-build-system)
    (propagated-inputs (list python-tokenize-rt-5))
    (home-page "https://github.com/asottile/pyupgrade")
    (synopsis "A tool to automatically upgrade syntax for newer versions.")
    (description
     "This package provides a tool to automatically upgrade syntax for newer versions.")
    (license license:expat)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'check))))))
(define-public python-flynt
  (package
    (name "python-flynt")
    (version "0.76")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flynt" version))
       (sha256
        (base32 "0fhr2djg0z3pd622crc4pkbvm776iknrjs9z40hqr7paa2jwb6bs"))))
    (build-system python-build-system)
    (propagated-inputs (list python-astor python-tomli))
    (home-page "https://github.com/ikamensh/flynt")
    (synopsis
     "CLI tool to convert a python project's %-formatted strings to f-strings.")
    (description
     "CLI tool to convert a python project's %-formatted strings to f-strings.")
    (license license:expat)))

(define-public python-biblib-simple
  (package
    (name "python-biblib-simple")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "biblib-simple" version))
       (sha256
        (base32 "1gqnr11vdn6f2z0dip209v9r5riawbgqp9xsykq3b2a4zx6mrjkd"))))
    (build-system python-build-system)
    (home-page "https://github.com/colour-science/biblib")
    (synopsis "Simple, correct BibTeX parser and algorithms")
    (description "Simple, correct BibTeX parser and algorithms")
    (license #f)))

(define-public python-colour-science
  (package
    (name "python-colour-science")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colour_science" version))
       (sha256
        (base32 "0rlvsr6v6jhnqj0slrd24xjkaplkw0qjz53wrpmzxnh4f73d9a19"))))
    (build-system python-build-system)
    (propagated-inputs (list python-biblib-simple
                             python-black
                             python-coverage
                             python-coveralls
                             python-flake8
                             python-flynt
                             python-imageio
                             python-invoke
                             jupyter
                             python-matplotlib
                             python-mypy
                             python-networkx
                             python-numpy
                             python-pandas
                             python-pre-commit
                             python-pydata-sphinx-theme
                             python-pydocstyle
                             python-pygraphviz
                             python-pytest
                             python-pytest-cov
                             python-pyupgrade
                             python-restructuredtext-lint
                             python-scikit-learn
                             python-scipy
                             python-sphinx
                             python-sphinxcontrib-bibtex
                             python-toml
                             python-tqdm
                             python-trimesh
                             python-twine
                             python-typing-extensions))
    (arguments
     `(#:tests? #f))
    (home-page "https://www.colour-science.org/")
    (synopsis "Colour Science for Python")
    (description "Colour Science for Python")
    (license #f)))

(define-public python-git-review-2.3
  (package
    (inherit python-git-review)
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git-review" version))
       (sha256
        (base32 "0kq16qvd57jwb19jhnlbpfsp0sr8981jnplbng5yddpcdq9kis94"))))))

(define-public python-waf-visionary
  (package
    (inherit python-waf)
    (name "python-waf-visionary")
    (version "7.0-rc1-fixup3-0.71d2451")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/electronicvisions/waf")
             (commit "71d245190cf1070c8f61f20e2e1c329c653e7a11")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k6qxkyykv48lw8xrp71lh2y6pg3bmk59wlrl4vcim0ssjfvifgh"))))
    (build-system python-build-system)))

(define-public python-socketio-client
  (package
    (name "python-socketio-client")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "socketIO-client" version))
       (sha256
        (base32 "1hfjfhyxgql1ndda1bagg8niy8m28byd2r0yq4l7zycwlzxq9kb4"))))
    (build-system python-build-system)
    (propagated-inputs (list python-requests python-six
                             python-websocket-client))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (home-page "https://github.com/invisibleroads/socketIO-client")
    (synopsis "A socket.io client library")
    (description "This package provides a socket.io client library")
    (license license:expat)))

(define-public overleaf-sync
  (package
    (name "overleaf-sync")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moritzgloeckl/overleaf-sync")
             (commit "aa62165eb9eba48f8b8bf3d93358f9feed0bf5a9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h0wspvhb5qr1n7b95pgdx6a1fwcqfsd6hbn2xwa30iwn5kahq00"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyside-6 python-yaspin python-socketio
                             python-socketio-client))
    (native-inputs (list python-flit))
    (home-page "https://github.com/moritzgloeckl/overleaf-sync")
    (synopsis "Overleaf Two-Way Synchronization")
    (description "Overleaf Two-Way Synchronization")
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'check)
                  (delete 'sanity-check))))
    (license license:expat)))

(define-public python-mypy-extensions-next
  (package
    (name "python-mypy-extensions")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mypy_extensions" version))
       (sha256
        (base32 "10h7mwjjfbwxzq7jzaj1pnv9g6laa1k0ckgw72j44160bnazinvm"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python/mypy_extensions")
    (synopsis
     "Type system extensions for programs checked with the mypy type checker.")
    (description
     "Type system extensions for programs checked with the mypy type checker.")
    (license license:expat)))

(define python-types-psutil
  (package
    (name "python-types-psutil")
    (version "6.1.0.20241221")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "types_psutil" version))
       (sha256
        (base32 "119i9nwlmfkvf253q4nq19lr0k8mrzr3ygqfgy4bh3jyplv5l3v0"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/python/typeshed")
    (synopsis "Typing stubs for psutil")
    (description "Typing stubs for psutil.")
    (license #f)))

(define-public python-mypy-next
  (package
    (name "python-mypy")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mypy" version))
       (sha256
        (base32 "1mijrnmjchjb445m1d6ci77hyscj34jmybzcfn20wlcvzr283j3y"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-mypy-extensions
                         python-setuptools
                         python-tomli
                         python-types-psutil
                         python-types-setuptools
                         python-typing-extensions
                         python-wheel))
    (propagated-inputs (list python-mypy-extensions python-tomli
                             python-typing-extensions))
    (home-page "http://www.mypy-lang.org/")
    (arguments
     '(#:tests? #f))
    (synopsis "Optional static typing for Python")
    (description "Optional static typing for Python")
    (license license:expat)))

;; (define-public python-jaxopt
;;   (package
;;     (name "python-jaxopt")
;;     (version "0.8.3")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (pypi-uri "jaxopt" version))
;;        (sha256
;;         (base32 "0ih6k5i0v107qpa94p6w90d3fsb0mx2n4q4r2qlz790mz6kdy1jb"))))
;;     (build-system pyproject-build-system)
;;     (arguments
;;      '(#:tests? #f))
;;     (propagated-inputs (list python-jax python-jaxlib python-numpy
;;                              python-scipy))
;;     (home-page "https://github.com/google/jaxopt")
;;     (synopsis
;;      "Hardware accelerated, batchable and differentiable optimizers in JAX.")
;;     (description
;;      "Hardware accelerated, batchable and differentiable optimizers in JAX.")
;;     (license license:asl2.0)))
