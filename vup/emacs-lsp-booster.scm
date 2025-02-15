(define-module (vup emacs-lsp-booster)
  #:use-module (vup rust-import-lock)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix import utils)
               #:select (beautify-description)))

(define-public emacs-lsp-booster
  (package
    (name "rust-emacs-lsp-booster")
    (version "0.2.1-0.5f702a2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/blahgeek/emacs-lsp-booster")
                    (commit "5f702a2699f306a3958ff1996a2b1a625f0cee0b")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vsz78kl0x85fddg0v5yv7hn9mcs5yfvv7qzfpc5gz4s562gxns7"))))
    (build-system cargo-build-system)
    (inputs (append (list emacs) (import-rust-lock source)))
    (home-page "FILLMEIN")
    (synopsis "FILLMEIN")
    (description
      (beautify-description "FILLMEIN"))
    (license #f)))

emacs-lsp-booster
