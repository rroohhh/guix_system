(define-module (vup vaultwarden)
 #:use-module (vup rust-import-lock)
 #:use-module (guix build-system cargo)
 #:use-module (guix build-system copy)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (guix gexp)
 #:use-module (guix base32)
 #:use-module (gnu packages perl)
 #:use-module (gnu packages c)
 #:use-module (gnu packages tls)
 #:use-module (gnu packages pkg-config)
 #:use-module (guix download)
 #:use-module (guix git-download)
 #:use-module ((guix import utils) #:select (beautify-description)))

(define-public web-vault
  (let* ((version "v2024.6.2c"))
    (package
      (name "web-vault")
      (version version)
      (source
        (origin
          (method url-fetch)
          (uri (string-append "https://github.com/dani-garcia/bw_web_builds/releases/download/" version "/bw_web_" version ".tar.gz"))
          (sha256
            (base32
              "0m68wjq0awmbarknrmgcz1csm6d556q824w9jgdxfrzndy5jc9pz"))))
      (build-system copy-build-system)
      (home-page "https://github.com/dani-garcia/bw_web_builds")
      (synopsis "Web vault builds for vaultwarden")
      (description "Web vault builds for vaultwarden")
      (license license:gpl3))))

(define-public vaultwarden
  (package
    (name "rust-vaultwarden")
    (version "1.32.7")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
        (url "https://github.com/dani-garcia/vaultwarden")
        (commit version)))
       (file-name (git-file-name name version))
         (sha256
           (base32
             "11ryf8hkinl61fhc7q7vrmpfdgvg8igl80cspmr3kwx74vam05lv"))))
    (build-system cargo-build-system)
    (native-inputs (list perl pkg-config))
    (inputs
     (append (list openssl)
             (import-rust-lock source `(("yubico" . ,(nix-base32-string->bytevector "00id2cm99806am8x3y9sfknfhpaba4nd9x5ybb08nq3sbys1w0jz"))))))
    (arguments
     (list #:tests? #f
           #:features ''("sqlite") ;  "vendored_openssl"
           #:phases #~(modify-phases %standard-phases
                        (add-before 'build 'patch-cargo-toml
                          (lambda _
                            (substitute* "Cargo.toml"
                              (("yubico = \\{ git")
                               "# ")))))))
    (home-page "None")
    (synopsis "Alternative implementation of the Bitwarden server API, compatible with the official clients")
    (description "Alternative implementation of the Bitwarden server API, compatible with the official clients")
    (license #f)))

vaultwarden
