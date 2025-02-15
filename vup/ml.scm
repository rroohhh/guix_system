(define-module (vup ml)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix licenses:)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake))

(define-public ctranslate2
  (let* ((version "3.17.1"))
    (package
      (name "CTranslate2")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/OpenNMT/CTranslate2")
                      (commit (string-append "v" version))
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mqx21scrjyd4nnipb9ia3vjgirba68sh0aqgwcwh2p1xgrh89k9"))))
      (build-system cmake-build-system)
      (arguments `(#:configure-flags (list "-DWITH_DNNL=YES", "-DWITH_MKL=NO", "-DOPENMP_RUNTIME=COMP")))
      (home-page "https://opennmt.net/CTranslate2/")
      (synopsis
       "Fast inference engine for Transformer models")
      (description
       "Fast inference engine for Transformer models")
      (license licenses:expat))))
