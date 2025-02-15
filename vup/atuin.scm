(define-module (vup atuin)
  #:use-module (vup rust-import-lock)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix import utils) #:select (beautify-description)))


(define-public atuin
  (package
    (name "rust-atuin")
    (version "18.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/atuinsh/atuin")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1zi7ar999ycvig9c9crylab540xdgr0h6v99q9j8ypk9i1fviyiz"))))
    (build-system cargo-build-system)
    (inputs (import-rust-lock source))
    (arguments
     (list #:tests? #f
           #:install-source? #f
           #:phases #~(modify-phases %standard-phases
                        (add-after 'patch-cargo-checksums 'chdir
                          (lambda _
                            (chdir "crates/atuin"))))))
    (home-page "{'workspace': True}")
    (synopsis "atuin - magical shell history")
    (description
      (beautify-description "atuin - magical shell history"))
    (license (list license:expat))))


atuin
