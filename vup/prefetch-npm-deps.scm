(define-module (vup prefetch-npm-deps)
  #:use-module (vup rust-import-lock)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix import utils) #:select (beautify-description)))

(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define nixpkgs-version "25.05-pre")

(define nixpkgs
  (let ((version nixpkgs-version))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/nixos/nixpkgs")
            (commit version)))
      (file-name (string-append "nixpkgs-" version "-checkout"))
      (sha256
       (base32 "0b96wqvk3hs98dhfrmdhqmx9ibac4kjpanpd1pig19jaglanqnxr")))))

(define-public prefetch-npm-deps
  (package
    (name "prefetch-npm-deps")
    (version nixpkgs-version)
    (source
     (origin
       (method computed-origin-method)
       (file-name (string-append "prefetch-npm-deps-" version))
       (sha256 #f)
       (uri
        (delay
          (with-imported-modules '((guix build utils))
            #~(begin
                (use-modules (guix build utils))
                (mkdir #$output)
                (copy-recursively
                 (string-append #$nixpkgs "/pkgs/build-support/node/fetch-npm-deps")
                 #$output)
                ))))))
    (build-system cargo-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (append (list curl nghttp2 openssl zlib)
             (import-rust-lock source)))
    (home-page "FILLMEIN")
    (synopsis "FILLMEIN")
    (description
     (beautify-description "FILLMEIN"))
    (license #f)))

(define-public prefetch-npm-deps-bin (file-append prefetch-npm-deps "/bin/prefetch-npm-deps"))

prefetch-npm-deps
