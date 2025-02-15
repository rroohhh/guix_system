(define-module (vup rust-apps)
  #:use-module (vup rust-import-lock)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ra-multiplex
  (package
    (name "ra-multiplex")
    (version "0.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pr2502/ra-multiplex")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bz22ksgw26m5zzqpjakc8a4z8xpac6sp1z9n30746cc1zvff6k8"))))
    (build-system cargo-build-system)
    (inputs (import-rust-lock source))
    (home-page "https://github.com/pr2502/ra-multiplex")
    (synopsis
     "share one rust-analyzer server instance between multiple LSP clients to save resources")
    (description
     "share one rust-analyzer server instance between multiple LSP clients to save
resources")
    (license license:expat)))

ra-multiplex
