(define-module (vup prjoxide)
  #:use-module (vup rust-import-lock)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix import utils)
                #:select (beautify-description))
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public rust-prjoxide
  (package
    (name "prjoxide")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gatecat/prjoxide")
             (commit "30712ff988a3ea7700fa11b87ae2d77e55c7c468")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x8idpjnq9wg6ski691qqqamr3nz40k3j6fbjnw6ylxp14m63ydb"))))
    (build-system cargo-build-system)
    (inputs (append (list m4)
                    (import-rust-lock (local-file "prjoxide-Cargo.lock"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'change-directory
                    (lambda _
                      (delete-file "libprjoxide/Cargo.toml")
                      (chdir "libprjoxide/prjoxide")
                      (setenv "CONFIG_SHELL"
                              (which "sh")) #t)))))
    (home-page "https://github.com/gatecat/prjoxide")
    (synopsis "Documenting Lattice's 28nm FPGA parts")
    (description "Documenting Lattice's 28nm FPGA parts")
    (license license:isc)))
