(define-module (vup root)
  #:use-module (ice-9 string-fun)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public xrootd
  (let* ((version "4.10.0"))
    (package
      (name "xrootd")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://lcgpackages.web.cern.ch/lcgpackages/tarFiles/sources/xrootd-"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "0nwr27apz3rzqcs657bgsgydibhr6sswfws12bzyisbjgpi8azzh"))))
      (build-system cmake-build-system)
      (inputs `(("zlib" ,zlib)
                ("openssl" ,openssl)))
      (home-page "https://xrootd.slac.stanford.edu/")
      (synopsis
       "The XROOTD project aims at giving high performance, scalable fault tolerant access to data repositories of many kinds.")
      (description
       "The XROOTD project aims at giving high performance, scalable fault tolerant access to data repositories of many kinds.")
      (license licenses:lgpl2.0+)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (delete 'check))))))) ;; no tests

(define-public vdt
  (let* ((version "0.4.3"))
    (package
      (name "vdt")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://lcgpackages.web.cern.ch/lcgpackages/tarFiles/sources/vdt-"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "1qdc10p4j6jl0as3a8pfvrygxdry2x6izxm8clmihp5v5rhp8mkh"))))
      (build-system cmake-build-system)
      (inputs `(("python" ,python)))
      (arguments
       '(#:configure-flags '("-DFMA=ON" "-DAVX=ON" "-DAVX2=ON")
         #:phases (modify-phases %standard-phases
                    (delete 'check))))
      (home-page "https://github.com/dpiparo/vdt")
      (synopsis "The vdt mathematical library")
      (description "The vdt mathematical library")
      (license licenses:lgpl2.0+))))
      ;; (arguments '(#:phases
      ;;              (modify-phases %standard-phases
      ;;                (delete 'check))))))) ;; no tests

(define-public afterimage
  (let* ((version "1.20"))
    (package
      (name "afterimage")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "ftp://ftp.afterstep.org/stable/libAfterImage/libAfterImage-"
                      version ".tar.gz"))
                (patches (search-patches "libafterimage-ldflags.patch"
                                         "libafterimage-libpng15.patch"
                                         "header-install.patch"))
                (sha256
                 (base32
                  "125y119fbr3g389nr7yls4i7x5zd5pz7h8qn12k8b21b4xk1h6y5"))))
      (build-system gnu-build-system)
      (inputs `(("zlib" ,zlib)
                ("librsvg" ,librsvg)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'check))))
      (home-page "http://www.afterstep.org/afterimage/")
      (synopsis "libAfterImage is a generic image manipulation library.")
      (description "libAfterImage is a generic image manipulation library.")
      (license licenses:gpl2+))))

(define-public root
  (let* ((version "6.26.02"))
    (package
      (name "root")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/root-project/root")
                      (commit (string-append "v"
                                             (string-replace-substring version
                                              "." "-")))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hjd6fv354flg8bjvk520qwch91rni0dr3bjgz0rv3hfv3626zga"))))
      (build-system cmake-build-system)
      (inputs `(("xrootd" ,xrootd)
                ("zstd" ,zstd "lib")
                ("tbb" ,tbb)
                ("libX11" ,libx11)
                ("xxhash" ,xxhash)
                ("lz4" ,lz4)
                ("freetype" ,freetype)
                ("pcre" ,pcre)
                ("python" ,python)
                ("python-numpy" ,python-numpy)
                ("libXpm" ,libxpm)
                ("libxft" ,libxft)
                ("libXext" ,libxext)
                ("fftw" ,fftw)
                ("cfitsio" ,cfitsio)
                ("openssl" ,openssl)
                ("vdt" ,vdt)
                ("libxml2" ,libxml2)
                ("opengl" ,mesa)
                ("glu" ,glu)
                ("gsl" ,gsl)
                ("pkg-config" ,pkg-config)
                ("ftgl" ,ftgl)
                ("sqlite" ,sqlite)
                ("glew" ,glew)
                ("gl2ps" ,gl2ps)
                ("jpeg" ,libjpeg-turbo)
                ("tiff" ,libtiff)
                ("gif" ,giflib)
                ("openblas" ,openblas)
                ("afterimage" ,afterimage)))
      (arguments
       '(#:configure-flags `("-Dclad=OFF" "-Dasimage=on"
                             "-Dgnuinstall=ON"
                             "-Dmysql=off"
                             "-Doracle=off"
                             "-Dpgsql=off"
                             "-Dpythia6=off"
                             "-Dpythia8=off"
                             "-Dgfal=off"
                             "-Ddavix=off"
                             "-Dfail-on-missing=ON"
                             "-Drpath=ON"
                             "-DCMAKE_INSTALL_LIBDIR=lib"
                             "-DCMAKE_INSTALL_INCLUDEDIR=include"
                             "-Dcxxmodules=OFF"
                             "-Druntime_cxxmodules=OFF")
         #:phases (modify-phases %standard-phases
                    (add-after 'configure 'fix-permissions
                      (lambda _
                        (for-each make-file-writable
                                  (find-files "../source" ".*")) #t)))))
      (home-page "https://github.com/root-project/root")
      (synopsis
       "ROOT: analyzing, storing and visualizing big data, scientifically")
      (description
       "ROOT: analyzing, storing and visualizing big data, scientifically")
      (license licenses:lgpl2.1+))))
