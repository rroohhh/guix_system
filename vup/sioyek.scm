(define-module (vup sioyek)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages web)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mupdf-1.20
  (package
    (inherit mupdf)
    (name "mupdf")
    (version "1.20.3")
    (source
     (origin
       (inherit (package-source mupdf))
       (uri (string-append "https://mupdf.com/downloads/archive/"
                           "mupdf-" version "-source.tar.lz"))
       (sha256
        (base32 "0s0qclxxdjis04mczgz0fhfpv0j8llk48g82zlfrk0daz0zgcwvg"))))))

(define (qt-urls component version)
  "Return a list of URLs for VERSION of the Qt5 COMPONENT."
  ;; We can't use a mirror:// scheme because these URLs are not exact copies:
  ;; the layout differs between them.
  (list (string-append "https://download.qt.io/official_releases/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-opensource-src-"
                       version ".tar.xz")
        (string-append "https://download.qt.io/archive/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-opensource-src-"
                       version ".tar.xz")
        (let ((directory (string-append "qt5" (string-drop component 2))))
          (string-append "http://sources.buildroot.net/" directory "/"
                         component "-everywhere-opensource-src-" version ".tar.xz"))
        (string-append "https://distfiles.macports.org/qt5/"
                       component "-everywhere-opensource-src-" version ".tar.xz")))

(define-public qt3d
  (package
    (inherit qtsvg-5)
    (name "qt3d")
    (version "5.15.5")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1m3y7d58crn0qgfwkimxcggssn2pbs8nj5b9diwns6rwqg4aqk20"))))
    (native-inputs
     (list perl vulkan-headers))
    (inputs
     (list mesa qtbase-5 zlib))
    (synopsis "Qt 3D")
    (description "Qt 3D")))

(define-public sioyek
  (let ((commit "ceae13b12df5653d3fe9e087167081e69a2ad48c"))
    (package
      (name "sioyek")
      (version (string-append "2.0.0+" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ahrm/sioyek")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kw0m7179nid2q2clvf98gzi3mdm3iwh7h7xlp99pydns77fq3dv"))))
      (build-system qt-build-system)
      (inputs (list qt3d qtwayland-5 mupdf-1.20 zlib mujs gumbo-parser harfbuzz freetype jbig2dec
                    libjpeg-turbo openjpeg))
      (arguments
       `(#:tests? #f  ; No test suite.
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "qmake"
                       (string-append "PREFIX=" (assoc-ref outputs "out")))))
           (add-after 'unpack 'patch-qmake-pro-files
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (chmod "tutorial.pdf" #o444)
               (substitute* "pdf_viewer_build_config.pro"
                 (("-lmupdf-third") ""))
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "pdf_viewer/main.cpp"
                   (("/etc/sioyek") (string-append out "/etc/sioyek"))
                   (("/usr/share/sioyek") (string-append out "/share/sioyek"))))
               #t)))))
      (home-page "https://sioyek.info/")
      (synopsis "Sioyek PDF viewer")
      (description "Sioyek is a PDF viewer designed for reading research papers and technical books.")
      (license (list license:gpl3)))))
