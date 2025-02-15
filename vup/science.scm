(define-module (vup science)
  #:use-module (gnu packages astronomy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ccfits
  (let ((version "2.5"))
    (package
      (name "ccfits")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://heasarc.gsfc.nasa.gov/fitsio/CCfits/CCfits-"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "0h27lh24137arry3i9ghwlyp5p3f8410gdfjp0czarcy4cjwv3lk"))))
      (inputs `(("cfitsio" ,cfitsio2)))
      (build-system gnu-build-system)
      (home-page "https://heasarc.gsfc.nasa.gov/fitsio/CCfits/")
      (synopsis
       "CCfits is an object oriented interface to the cfitsio library")
      (description
       "CCfits is an object oriented interface to the cfitsio library")
      (license #f))))

(define-public cfitsio2
  (package
    (inherit cfitsio)
    (version "3.37")
    (source (origin
              (method url-fetch)
              (uri
               "http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio3370.tar.gz")
              (sha256
               (base32
                "0fn6361mnm76y33zzczrv2inl8z88n776nnkj4ny9pz4vb39fa09"))))
    (arguments
     `(#:make-flags `("shared")
       #:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-paths
                    (lambda _
                      (substitute* "Makefile.in"
                        (("/bin/")
                         "")) #t)))))))
