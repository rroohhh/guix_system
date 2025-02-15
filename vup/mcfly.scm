(define-module (vup mcfly)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-bindgen-0.60
  (package
    (name "rust-bindgen")
    (version "0.60.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rl8pzzbxsgkx0v20bvvbwrlqhbifzw2p3ikwrns9b543fydsb86"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cexpr" ,rust-cexpr-0.6)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lazycell" ,rust-lazycell-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-libsqlite3-sys-0.25
  (package
    (name "rust-libsqlite3-sys")
    (version "0.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ym7x39ihcf2s0iyd3iqk6i283kgxcrdc7hxig94cybi7p83by19"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.60)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Native bindings to the libsqlite3 library")
    (description "Native bindings to the libsqlite3 library")
    (license license:expat)))

(define-public rust-rusqlite-0.28
  (package
    (name "rust-rusqlite")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aj2jvlcdy1miahy6wsia50ak26q3ziynl8yx0raqffb7sy17qh1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
                       ("rust-fallible-streaming-iterator" ,rust-fallible-streaming-iterator-0.1)
                       ("rust-hashlink" ,rust-hashlink-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.25)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Ergonomic wrapper for SQLite")
    (description "Ergonomic wrapper for SQLite")
    (license license:expat)))

(define-public rust-path-dedot-3
  (package
    (name "rust-path-dedot")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "path-dedot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15wkx8q3vra34fslzlg1lkq7liyxwqrpbxiz44a28wa7w3bhmfh7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://magiclen.org/path-dedot")
    (synopsis
     "A library for extending `Path` and `PathBuf` in order to parse the path which contains dots.")
    (description
     "This package provides a library for extending `Path` and `@code{PathBuf`} in
order to parse the path which contains dots.")
    (license license:expat)))

(define-public rust-path-absolutize-3
  (package
    (name "rust-path-absolutize")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "path-absolutize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xc36c5lz187wy452qph3lrr41x8ffgxk1clj2s9b8czwwgkibz4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-path-dedot" ,rust-path-dedot-3))))
    (home-page "https://magiclen.org/path-absolutize")
    (synopsis
     "A library for extending `Path` and `PathBuf` in order to get an absolute path and remove the containing dots.")
    (description
     "This package provides a library for extending `Path` and `@code{PathBuf`} in
order to get an absolute path and remove the containing dots.")
    (license license:expat)))

(define-public rust-chrono-systemd-time-0.3
  (package
    (name "rust-chrono-systemd-time")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono-systemd-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v6xf3amp8nyjsf0bkih3vmm2yy4q2kx92x3i81cb1pvla9sx7s2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/Odoh/chrono-systemd-time")
    (synopsis
     "Library which parses systemd.time style timestamps into chrono types")
    (description
     "Library which parses systemd.time style timestamps into chrono types")
    (license (list license:expat license:asl2.0))))

(define-public rust-cocoa-0.20
  (package
    (name "rust-cocoa")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y0wd1lyiz8cgbsf0fwyw06gb1akg6rvg5jr3wah8mvdqdpyhj8c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-core-foundation" ,rust-core-foundation-0.7)
                       ("rust-core-graphics" ,rust-core-graphics-0.19)
                       ("rust-foreign-types" ,rust-foreign-types-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa for macOS")
    (description "Bindings to Cocoa for @code{macOS}")
    (license (list license:expat license:asl2.0))))

(define-public rust-autopilot-0.4
  (package
    (name "rust-autopilot")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "autopilot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n4mk2y69p0n0a8b64n68qhbwg0sckkfgk4v84ahplqcklsx0brs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cocoa" ,rust-cocoa-0.20)
                       ("rust-core-foundation" ,rust-core-foundation-0.7)
                       ("rust-core-graphics" ,rust-core-graphics-0.19)
                       ("rust-image" ,rust-image-0.22)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-x11" ,rust-x11-2))))
    (home-page "https://www.autopy.org")
    (synopsis "A simple, cross-platform GUI automation library for Rust.")
    (description
     "This package provides a simple, cross-platform GUI automation library for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public mcfly
  (package
    (name "mcfly")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mcfly" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sz8xpdxam6sl3gxiq4w9fm89nrvl5dqarxp7w431j8wy5yhp3dh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autopilot" ,rust-autopilot-0.4)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-systemd-time" ,rust-chrono-systemd-time-0.3)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-crossterm" ,rust-crossterm-0.26)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-directories-next" ,rust-directories-next-2)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-path-absolutize" ,rust-path-absolutize-3)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rusqlite" ,rust-rusqlite-0.28)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-shellexpand" ,rust-shellexpand-2)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/cantino/mcfly")
    (synopsis
     "McFly replaces your default ctrl-r shell history search with an intelligent search engine that takes into account your working directory and the context of recently executed commands. McFly's suggestions are prioritized in real time with a small neural network.")
    (description
     "@code{McFly} replaces your default ctrl-r shell history search with an
intelligent search engine that takes into account your working directory and the
context of recently executed commands. @code{McFly's} suggestions are
prioritized in real time with a small neural network.")
    (license license:expat)))
