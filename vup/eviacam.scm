(define-module (vup eviacam)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages image-processing)
  #:use-module ((guix licenses) #:prefix license:))

(define-public eviacam
  (let ((commit "a4032ed9c59def5399a93e74f5ea84513d2f42b1"))
   (package
    (name "eviacam")
    (version (string-append "2.1.5-" (string-take commit 8)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cmauri/eviacam.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cn99jgsb83mgcnf3bgzpr56wbh9j9icrqpmvbh323i8ap834xlm"))))
    (build-system gnu-build-system)
    (native-inputs (list libtool autoconf automake gnu-gettext pkg-config))
    (inputs (list libxext libx11 libxtst gtk+ opencv wxwidgets))
    (home-page
      "https://github.com/cmauri/eviacam")
    (synopsis "webcam based mouse emulator")
    (description
      "webcam based mouse emulator")
    (license license:gpl3))))
