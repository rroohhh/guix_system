(define-module (vup ml)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix licenses:)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake))

(define-public ctranslate2
  (package
    (name "CTranslate2")
    (version "4.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenNMT/CTranslate2")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i5ighnybd405bqyp11081x9pyhdlh460iwdc3yzyhg2wjvyp6fr"))))
    (build-system cmake-build-system)
    (inputs (list oneapi-dnnl))
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DWITH_DNNL=YES"
                               ,"-DWITH_MKL=NO"
                               ,"-DOPENMP_RUNTIME=COMP")))
    (home-page "https://opennmt.net/CTranslate2/")
    (synopsis "Fast inference engine for Transformer models")
    (description "Fast inference engine for Transformer models")
    (license licenses:expat)))
