(define-module (vup x)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public x2x
  (let ((commit "ec10215d558f4b227547522c660f35db8ba6901e"))
    (package
      (name "x2x")
      (version (string-append "2020.03.02-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dottedmag/x2x")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19nmpis2qk8wz4mlcncjb9wn9r96lnhvpic2sy43vgdc3pww7vn9"))))
      (build-system gnu-build-system)
      (inputs `(("autoconf" ,autoconf)
                ("automake" ,automake)
                ("pkg-config" ,pkg-config)
                ("libx11" ,libx11)
                ("libxext" ,libxext)
                ("libxtst" ,libxtst)))
      (home-page "https://github.com/dottedmag/x2x")
      (synopsis
       "x2x allows the keyboard, mouse on one X display to be used to control another X display.")
      (description
       "x2x allows the keyboard, mouse on one X display to be used to control another X display.")
      (license #f))))
