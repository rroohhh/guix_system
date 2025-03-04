(define-module (vup qt-apps)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages compression))

(define-public qdirstat
  (package
    (name "qdirstat")
    (version "1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shundhammer/qdirstat")
             (commit "1.9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09amjkg0h097kahscap59imyg4wpjy43lkaiqw60ady3s6b6c1x7"))))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "qmake"
                              (string-append "INSTALL_PREFIX="
                                             (assoc-ref outputs "out"))))))))
    (inputs (list qtbase-5 zlib))
    (home-page "https://github.com/shundhammer/qdirstat")
    (synopsis
     "QDirStat - Qt-based directory statistics (KDirStat without any KDE - from the original KDirStat author)")
    (description
     "QDirStat - Qt-based directory statistics (KDirStat without any KDE - from the original KDirStat author)")
    (license license:gpl2)))

(define-public klayout
  (package
    (name "klayout")
    (version "0.29.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KLayout/klayout")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f6r7ny33ipg6dskkm14wgah8wj09ysh14z483ilbwna76lfx19h"))))
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (mkdir "build")
                      (chdir "build")
                      (invoke "qmake"
                              (string-append "PREFIX="
                                             (assoc-ref outputs "out"))
                              ;; "HAVE_QT5=1"
                              "HAVE_QT=0"
                              "HAVE_QT_NETWORK=1"
                              "HAVE_QT_UITOOLS=1"
                              "HAVE_QT_SVG=1"
                              "HAVE_QT_SQL=1"
                              "HAVE_QT_PRINTSUPPORT=1"
                              "HAVE_QT_XML=1"
                              "HAVE_QT_DESIGNER=1"
                              "HAVE_QT_MULTIMEDIA=1"
                              "HAVE_QTBINDINGS=1"
                              "HAVE_QT=1"
                              "HAVE_PYTHON=0"
                              "HAVE_RUBY=0"
                              "HAVE_64BIT_COORD=1"
                              "CONFIG+=release"
                              "-recursive"
                              "../src/klayout.pro") #t)))))
    (build-system qt-build-system)
    (inputs `(("qtbase" ,qtbase-5)
              ("qtsvg" ,qtsvg-5)
              ("qtxmlpatterns" ,qtxmlpatterns)
              ("qttools" ,qttools-5)
              ("qtmultimedia" ,qtmultimedia-5)
              ("python" ,python)
              ("zlib" ,zlib)))
    (home-page "https://github.com/KLayout/klayout")
    (synopsis "KLayout")
    (description "KLayout")
    (license #f)))
