(define-module (vup sioyek)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages web)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module ((guix licenses)
                #:prefix license:))

(define mupdf-1.24
  (package
    (inherit mupdf)
    (name "mupdf")
    (version "1.24.9")
    (source
     (origin
       (inherit (package-source mupdf))
       (uri (string-append "https://mupdf.com/downloads/archive/" "mupdf-"
                           version "-source.tar.lz"))
       (sha256
        (base32 "045pl9xkrqnwag3rvic3ff0xx6ykf39hw683608x0fllmsbs0zfq"))))
    (arguments
     (append
      `(#:imported-modules ((guix build python-build-system)
                            ,@%default-gnu-imported-modules)
        #:modules (((guix build python-build-system) #:prefix python:)
                   (guix build utils)
                   (ice-9 rdelim)
                   (ice-9 popen)
                   ,@%default-gnu-imported-modules))
     (substitute-keyword-arguments (package-arguments mupdf)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'build 'build-bindings
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (invoke "./scripts/mupdfwrap.py" "-d" "build/shared-release" "-b" "0123")
                (let* ((out (assoc-ref outputs "out"))
                       (inc (string-append out "/include/mupdf"))
                       (lib (string-append out "/lib"))
                       (python-install-dir (string-append (python:site-packages inputs outputs) "/mupdf")))
                  (mkdir-p inc)
                  (for-each (lambda (f) (install-file f inc)) (find-files "platform/c++/include/mupdf" ".h$"))
                  (mkdir-p lib)
                  (for-each (lambda (f) (install-file f lib)) (find-files "build" "libmupdfcpp.so"))
                  (mkdir-p python-install-dir)
                  (for-each (lambda (f) (install-file f python-install-dir)) (find-files "build" "_mupdf.so"))
                  (for-each (lambda (f) (copy-file f (string-append python-install-dir "/__init__.py"))) (find-files "build" "mupdf.py"))
                  (let ((libdir (string-append #$output "/lib")))
                  (for-each
                    (lambda (file)
                      (let* ((pipe (open-pipe* OPEN_READ "patchelf"
                                               "--print-rpath" file))
                             (line (read-line pipe)))
                        (and (zero? (close-pipe pipe))
                             (invoke "patchelf" "--set-rpath"
                                     (string-append lib ":" line)
                                     file))))
                    (find-files lib ".*\\.so$"))))
              )))))))

    (native-inputs (modify-inputs (package-native-inputs mupdf)
                     (append patchelf swig python-clang-13 python)))))

(define qt-url
  (@@ (gnu packages qt) qt-url))

(define qt3d
  (package
    (inherit qtsvg)
    (name "qt3d")
    (version (package-version qtsvg))
    (source
     (origin
       (method url-fetch)
       (uri (qt-url name version))
       (sha256
        (base32 "1pwagjicvqc7lbypkw7wvjznndyzqm2ihisqdqc36ccp0kcqgh4b"))))
    (native-inputs (list perl vulkan-headers))
    (inputs (list mesa vulkan-loader qtbase zlib libxkbcommon assimp))
    (arguments (substitute-keyword-arguments
     (package-arguments qtsvg)
     ((#:phases phases)
      #~(modify-phases #$phases (delete 'delete-installed-tests)))))
    (synopsis "Qt 3D")
    (description "Qt 3D")))

(define qttexttospeech
  (package
    (inherit qtsvg)
    (name "qtspeech")
    (version (package-version qtsvg))
    (source
     (origin
       (method url-fetch)
       (uri (qt-url name version))
       (sha256
        (base32 "1khl90m6jd2zg0r0fncdz3r1w2l96vwp6jihpq9rgr730ja7d36c"))))
    (inputs (list qtbase qtmultimedia qtdeclarative))
    (arguments (list #:tests? #f))
    (synopsis "Qt text to speech")
    (description "Qt text to speech")))

(define-public sioyek
  (package
    (name "sioyek")
    (version "2.0.0-1.986af1e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ahrm/sioyek")
             (commit "986af1e13b937bdafe789364d26072ac04087af0")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dv2ii2443fdcqyabk1w27g425b7423z3ff68bkgb382106n647a"))))
    (build-system qt-build-system)
    (inputs (list qt3d
                  qtwayland
                  qttexttospeech
                  qtdeclarative
                  qtsvg
                  qtspeech
                  qtmultimedia
                  mupdf-1.24
                  zlib
                  mujs
                  gumbo-parser
                  harfbuzz
                  freetype
                  jbig2dec
                  libjpeg-turbo
                  openjpeg
                  vulkan-headers
                  libxkbcommon))
    (arguments
     `(#:qtbase ,qtbase
       #:tests? #f ;No test suite.
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "qmake"
                              (string-append "PREFIX="
                                             (assoc-ref outputs "out")))))
                  (add-after 'unpack 'patch-qmake-pro-files
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (chmod "tutorial.pdf" #o444)
                      (substitute* "pdf_viewer_build_config.pro"
                        (("-lmupdf-third")
                         ""))
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "pdf_viewer/main.cpp"
                          (("/etc/sioyek")
                           (string-append out "/etc/sioyek"))
                          (("/usr/share/sioyek")
                           (string-append out "/share/sioyek")))) #t)))))
    (home-page "https://sioyek.info/")
    (synopsis "Sioyek PDF viewer")
    (description
     "Sioyek is a PDF viewer designed for reading research papers and technical books.")
    (license (list license:gpl3))))

(define python-pypdf2-3
  (package
    (name "python-pypdf2")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyPDF2" version))
       (sha256
        (base32 "0h0lxv7zwfzm65frplh4lhda6lyw0gnz8bimp5qiy9x6kgv0hi57"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-flit
                             python-pillow
                             python-typing-extensions
                             python-wheel))
    (native-inputs (list python-flit-core))
    (arguments (list #:tests? #f))
    (home-page #f)
    (synopsis
     "A pure-python PDF library capable of splitting, merging, cropping, and transforming PDF files")
    (description
     "This package provides a pure-python PDF library capable of splitting, merging,
cropping, and transforming PDF files.")
    (license #f)))

(define python-slugify-8
  (package
    (name "python-slugify")
    (version "8.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-slugify" version))
       (sha256
        (base32 "0mn86ywhff9gh88dshgymzd4b3wjiw1mw33jwylm8nyhs5qj682r"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-text-unidecode))
    (native-inputs (list python-setuptools python-wheel))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/un33k/python-slugify")
    (synopsis "A Python slugify application that also handles Unicode")
    (description
     "This package provides a Python slugify application that also handles Unicode.")
    (license license:expat)))

(define python-pymupdf
  (package
    (name "python-pymupdf")
    (version (package-version mupdf-1.24))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyMuPDF" version))
       (sha256
        (base32 "0wvlfr0y88dyw56jzpa83s1pjnfdyw3v5fnspndw03gi4klab4in"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'setup
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "PYMUPDF_SETUP_MUPDF_BUILD" "")
              (setenv "PYMUPDF_SETUP_LIBCLANG" "clang")
              (setenv "PYMUPDF_MUPDF_LIB" (string-append (assoc-ref inputs "mupdf") "/lib"))
              (setenv "PYMUPDF_MUPDF_INCLUDE" (string-append (assoc-ref inputs "mupdf") "/include"))
              )))))
    (propagated-inputs (list mupdf-1.24))
    (native-inputs (list python-setuptools python-wheel swig))
    (home-page #f)
    (synopsis
     "A high performance Python library for data extraction, analysis, conversion & manipulation of PDF (and other) documents.")
    (description
     "This package provides a high performance Python library for data extraction,
analysis, conversion & manipulation of PDF (and other) documents.")
    (license #f)))

(define-public python-sioyek
  (package
    (name "python-sioyek")
    (version "0.31.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sioyek" version))
       (sha256
        (base32 "1qsbpnmr5jyla77wbs7hrpxvcp5sr36rxbb49wmwsls2dlmz0fib"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
          (delete 'sanity-check))))
    (propagated-inputs (list python-appdirs
                             python-numpy
                             python-pymupdf
                             python-pyqt
                             python-regex))
    (native-inputs (list python-hatchling))
    (home-page "")
    (synopsis "Python tools and extensions for sioyek PDF reader")
    (description "Python tools and extensions for sioyek PDF reader.")
    (license #f)))

(list qtspeech qt3d qtdeclarative qtbase)
