(define-module (home lights)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-crypto)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (lights))

(define-public lights
  (package
    (name "lights")
    (version "0.1.0")
    (source
      (local-file "./lights" #:recursive? #t))
      ;; (origin
      ;;   (method url-fetch)
      ;;   (uri (crate-uri "atuin" version))
      ;;   (file-name
      ;;     (string-append name "-" version ".tar.gz"))
      ;;   (sha256
      ;;     (base32
      ;;       "19kviks3561d2qdqc0667c74wx5s9rvfkpzk51r083vl098k591z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:tests? #f
      #:cargo-inputs
      (("rust-rumqttc" ,rust-rumqttc-0.24)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde-json" ,rust-serde-json-1)
       ("rust-rand" ,rust-rand-0.8))))
    (home-page "--")
    (synopsis "lights control daemon")
    (description "lights control daemon")
    (license (list license:gpl2))))

(define rust-color-backtrace-0.5
  (package
    (name "rust-color-backtrace")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "color-backtrace" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11fn3snykx90w3nznzrcf4r164zmhk790asx0kzryf4r7i308v6d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/athre0z/color-backtrace")
    (synopsis "Colorful panic backtraces")
    (description "Colorful panic backtraces")
    (license (list license:expat license:asl2.0))))

(define rust-pharos-0.5
  (package
    (name "rust-pharos")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pharos" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "055lg1dzrxnryfy34a9cyrg21b7cl6l2frfx2p7fdvkz864p6mp9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-rustc-version" ,rust-rustc-version-0.4))))
    (home-page "https://github.com/najamelan/pharos")
    (synopsis
     "Observer pattern which generates a futures 0.3 stream of events")
    (description
     "Observer pattern which generates a futures 0.3 stream of events")
    (license license:unlicense)))

(define rust-async-io-stream-0.3
  (package
    (name "rust-async-io-stream")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async_io_stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k5rv51935p3il74q59hwaaid6sy9kv05vz3lw48jpgkrpgbkmxn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-pharos" ,rust-pharos-0.5)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/najamelan/async_io_stream")
    (synopsis "IntoAsyncRead on steriods")
    (description "@code{IntoAsyncRead} on steriods")
    (license license:unlicense)))

(define rust-ws-stream-tungstenite-0.13
  (package
    (name "rust-ws-stream-tungstenite")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ws_stream_tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "028nx60c24mcihjdz6i2zb636vgl1kxb1z5zq7y1kyw3y0ag9651"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-tungstenite" ,rust-async-tungstenite-0.25)
                       ("rust-async-io-stream" ,rust-async-io-stream-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-pharos" ,rust-pharos-0.5)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tungstenite" ,rust-tungstenite-0.21))))
    (home-page "https://github.com/najamelan/ws_stream_tungstenite")
    (synopsis "Provide AsyncRead/AsyncWrite over Tungstenite WebSockets")
    (description
     "Provide @code{AsyncRead/AsyncWrite} over Tungstenite @code{WebSockets}")
    (license license:unlicense)))

(define rust-tokio-1
  (package
    (name "rust-tokio")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c89p36zbd4abr1z3l5mipp43x7z4c9b4vp4s6r8y0gs2mjmya31"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio-macros" ,rust-tokio-macros-2)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://tokio.rs")
    (synopsis
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

(define rust-rustls-webpki-0.102
  (package
    (name "rust-rustls-webpki")
    (version "0.102.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "041ncshpw8wsvi8p74a3yw9c0r17lhyk1yjsxyrbkv8bfii0maps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aws-lc-rs" ,rust-aws-lc-rs-1)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-untrusted" ,rust-untrusted-0.9))))
    (home-page "https://github.com/rustls/webpki")
    (synopsis "Web PKI X.509 Certificate Verification.")
    (description "Web PKI X.509 Certificate Verification.")
    (license license:isc)))

(define rust-rustls-pki-types-1
  (package
    (name "rust-rustls-pki-types")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pki-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a0g7453h07701vyxjj05gv903a0shi43mf7hl3cdd08hsr6gpjy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rustls/pki-types")
    (synopsis "Shared types for the rustls PKI ecosystem")
    (description "Shared types for the rustls PKI ecosystem")
    (license (list license:expat license:asl2.0))))

(define rust-rustls-pemfile-2
  (package
    (name "rust-rustls-pemfile")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1awxak91qgraqrsk7bwxyn2aijhzyrs7flmaddajmxbgbrl750gl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description "Basic .pem file parser for keys and certificates")
    (license (list license:asl2.0 license:isc license:expat))))

(define rust-flume-0.11
  (package
    (name "rust-flume")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flume" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10girdbqn77wi802pdh55lwbmymy437k7kklnvj12aaiwaflbb2m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-nanorand" ,rust-nanorand-0.7)
                       ("rust-spin" ,rust-spin-0.9))))
    (home-page "https://github.com/zesterer/flume")
    (synopsis "A blazingly fast multi-producer channel")
    (description
     "This package provides a blazingly fast multi-producer channel")
    (license (list license:asl2.0 license:expat))))

(define rust-bytes-1
  (package
    (name "rust-bytes")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08w2i8ac912l8vlvkv3q51cd4gr09pwlg3sjsjffcizlrb0i5gd2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tokio-rs/bytes")
    (synopsis "Types and traits for working with bytes")
    (description "Types and traits for working with bytes")
    (license license:expat)))

(define rust-tungstenite-0.21
  (package
    (name "rust-tungstenite")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qaphb5kgwgid19p64grhv2b9kxy7f1059yy92l9kwrlx90sdwcy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-http" ,rust-http-1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.22)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description "Lightweight stream-based @code{WebSocket} implementation")
    (license (list license:expat license:asl2.0))))

(define rust-smallvec-1
  (package
    (name "rust-smallvec")
    (version "1.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smallvec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mzk9j117pn3k1gabys0b7nz8cdjsx5xc6q7fwnm8r0an62d7v76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/servo/rust-smallvec")
    (synopsis
     "'Small vector' optimization: store up to a small number of items on the stack")
    (description
     "Small vector optimization: store up to a small number of items on the stack")
    (license (list license:expat license:asl2.0))))

(define rust-glib-macros-0.19
  (package
    (name "rust-glib-macros")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f13v3cxmkcsrjpsp6k82wzfg8p0ml5qarj6ggf4qgm84z59fn0g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GLib library, proc macros crate")
    (description "Rust bindings for the GLib library, proc macros crate")
    (license license:expat)))

(define rust-glib-0.19
  (package
    (name "rust-glib")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lx0j1migbvm448g6r5m389r4h2lmbbffk5d0plh512x1da8d7mb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib-macros" ,rust-glib-macros-0.19)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GLib library")
    (description "Rust bindings for the GLib library")
    (license license:expat)))

(define rust-gobject-sys-0.19
  (package
    (name "rust-gobject-sys")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ya9wn6x4md9wsyhgvz08c1h69n8lniqsjcbqp88shdrh082npn8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgobject-2.0")
    (description "FFI bindings to libgobject-2.0")
    (license license:expat)))

(define rust-glib-sys-0.19
  (package
    (name "rust-glib-sys")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m6fzq9lh5snmgchmrm0py5wjzdlay0lxpsq6axs1iypfdvhj3v3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libglib-2.0")
    (description "FFI bindings to libglib-2.0")
    (license license:expat)))

(define rust-gio-sys-0.19
  (package
    (name "rust-gio-sys")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r0wp34aacf4jgs79fic0qra1s6162q0glskcxir9clv47cy3y5w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgio-2.0")
    (description "FFI bindings to libgio-2.0")
    (license license:expat)))

(define rust-gio-0.19
  (package
    (name "rust-gio")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00jsn1ivcng4g6rsh160dxg2kfnyhxijr08dxli7xlkdgfr11bif"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gio-sys" ,rust-gio-sys-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Gio library")
    (description "Rust bindings for the Gio library")
    (license license:expat)))

(define rust-async-tls-0.13
  (package
    (name "rust-async-tls")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0plsx2ysd8rbmzfllxib7pkbp3zp7m1yl7gywjh75m49pag3rbmj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/async-std/async-tls")
    (synopsis "Asynchronous TLS/SSL streams using Rustls.")
    (description "Asynchronous TLS/SSL streams using Rustls.")
    (license (list license:expat license:asl2.0))))

(define rust-async-native-tls-0.5
  (package
    (name "rust-async-native-tls")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v2v82crqm4fgj1s32gik56m7cwx0ygqjdqc5pw9zrq7rxddqhwk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures
")
    (description "Native TLS using futures")
    (license (list license:expat license:asl2.0))))

(define rust-async-tungstenite-0.25
  (package
    (name "rust-async-tungstenite")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p9z2b55ff648kngghrb1n8w9n95qr1g4qjlpqppalckxxj8s3zg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-native-tls" ,rust-async-native-tls-0.5)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-tls" ,rust-async-tls-0.13)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-tungstenite" ,rust-tungstenite-0.21)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/sdroege/async-tungstenite")
    (synopsis
     "Async binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "Async binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation")
    (license license:expat)))

(define rust-async-http-proxy-1
  (package
    (name "rust-async-http-proxy")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-http-proxy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14hm41jdx1sbzywlb4rg228ah5a391avl1bmpm4609h8sgaabyi9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/LinkTed/async-http-proxy")
    (synopsis "Lightweight asynchronous HTTP proxy client library")
    (description "Lightweight asynchronous HTTP proxy client library")
    (license license:bsd-3)))

(define rust-rumqttc-0.24
  (package
    (name "rust-rumqttc")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rumqttc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aclgx4lldir2d4c968cr6ac9ldvigs22fpd83wldmdjz8aqwmp1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-http-proxy" ,rust-async-http-proxy-1)
                       ("rust-async-tungstenite" ,rust-async-tungstenite-0.25)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-flume" ,rust-flume-0.11)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-url" ,rust-url-2)
                       ("rust-ws-stream-tungstenite" ,rust-ws-stream-tungstenite-0.13))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-color-backtrace" ,rust-color-backtrace-0.5)
                                   ("rust-matches" ,rust-matches-0.1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
                                   ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bytebeamio/rumqtt")
    (synopsis "An efficient and robust mqtt client for your connected devices")
    (description
     "An efficient and robust mqtt client for your connected devices")
    (license license:asl2.0)))

lights
