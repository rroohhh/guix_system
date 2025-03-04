(define-module (vup vaultwarden)
  #:use-module (vup rust-import-lock)
  #:use-module (vup rust-nightly)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix base32)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages c)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix import utils)
                #:select (beautify-description)))

(define-public web-vault
  (package
    (name "bw_web")
    (version "2025.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/dani-garcia/bw_web_builds/releases/download/v2025.1.1/bw_web_v2025.1.1.tar.gz"))
       (sha256
        (base32 "0ifjrxkq48ww0rdr3gwin6pzsllaz0x6an0zi3hsjqylkgsyy09z"))))
    (build-system copy-build-system)
    (home-page "https://github.com/dani-garcia/bw_web_builds")
    (synopsis "Web vault builds for vaultwarden")
    (description "Web vault builds for vaultwarden")
    (license license:gpl3)))

(define-public vaultwarden
  (package
    (name "rust-vaultwarden")
    (version "1.33.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dani-garcia/vaultwarden")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12knqdn2l43pcq3nx1dxg4y6g780q18mxdvgfaflgr72ajlzzv9f"))))
    (build-system cargo-build-system)
    (native-inputs (list perl pkg-config))
    (inputs (append (list openssl)
                    (import-rust-lock source
                                      `(("yubico" unquote
                                         (nix-base32-string->bytevector
                                          "00id2cm99806am8x3y9sfknfhpaba4nd9x5ybb08nq3sbys1w0jz"))))))
    (arguments
     (list
      #:tests? #f
      #:features ''("sqlite") ;"vendored_openssl"
      #:rust rust-nightly
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'patch-cargo-toml
            (lambda _
              (substitute* "Cargo.toml"
                (("yubico = \\{ git")
                 "# ")))))))
    (home-page "None")
    (synopsis
     "Alternative implementation of the Bitwarden server API, compatible with the official clients")
    (description
     "Alternative implementation of the Bitwarden server API, compatible with the official clients")
    (license #f)))

vaultwarden
