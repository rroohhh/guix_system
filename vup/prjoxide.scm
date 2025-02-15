(define-module (vup prjoxide)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix import utils) #:select (beautify-description))
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-adler_1_0_2
  (package
    (name "rust-adler")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "adler" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A simple clean-room implementation of the Adler-32 checksum")
    (description
      (beautify-description "A simple clean-room implementation of the Adler-32 checksum"))
    (license license:asl2.0)))

(define-public rust-aho-corasick_0_7_18
  (package
    (name "rust-aho-corasick")
    (version "0.7.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vv50b3nvkhyy7x7ip19qnsq11bqlnffkmj2yx2xlyk5wzawydqy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr_2_4_1))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      (beautify-description "Fast multiple substring searching."))
    (license (list license:unlicense
               license:expat))))

(define-public rust-anyhow_1_0_44
  (package
    (name "rust-anyhow")
    (version "1.0.44")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anyhow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ha2lam408ni6vb5zc64lirwz3f60a5qzmzx54r5q79fhs7llq31"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description
      (beautify-description "Flexible concrete Error type built on std::error::Error"))
    (license license:expat)))

(define-public rust-atty_0_2_14
  (package
    (name "rust-atty")
    (version "0.2.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hermit-abi" ,rust-hermit-abi_0_1_19)        
        ("rust-libc" ,rust-libc_0_2_104)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
      (beautify-description "A simple interface for querying atty"))
    (license license:expat)))

(define-public rust-autocfg_1_0_1
  (package
    (name "rust-autocfg")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jj6i9zn4gjl03kjvziqdji6rwx8ykz8zk2ngpc331z2g3fk3c6d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Automatic cfg for Rust compiler features")
    (description
      (beautify-description "Automatic cfg for Rust compiler features"))
    (license license:asl2.0)))

(define-public rust-base64_0_10_1
  (package
    (name "rust-base64")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13k6bvd3n6dm7jqn9x918w65dd9xhx454bqphbnv0bkd6n9dj98b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder_1_4_3))))
    (home-page "None")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description
      (beautify-description "encodes and decodes base64 as bytes or utf8"))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-bitflags_1_3_2
  (package
    (name "rust-bitflags")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
      (beautify-description "A macro to generate structures which behave like bitflags."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-byteorder_1_4_3
  (package
    (name "rust-byteorder")
    (version "1.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byteorder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0456lv9xi1a5bcm32arknf33ikv76p3fr9yzki4lb2897p2qkh8l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/byteorder")
    (synopsis "Library for reading/writing numbers in big-endian and little-endian.")
    (description
      (beautify-description "Library for reading/writing numbers in big-endian and little-endian."))
    (license license:expat)))

(define-public rust-capnp_0_14_3
  (package
    (name "rust-capnp")
    (version "0.14.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "capnp" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z79pr9y3zq3dyi3aqfz0m3cynz17rayxd62vmwm42s235qqm6xf"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "runtime library for Cap\u0027n Proto data encoding")
    (description
      (beautify-description "runtime library for Cap\u0027n Proto data encoding"))
    (license license:expat)))

(define-public rust-capnpc_0_14_4
  (package
    (name "rust-capnpc")
    (version "0.14.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "capnpc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zhis7h4wbhd4d7zc3jnpgkjm4nj9dcfsipp71f8nlb2260wwyxl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-capnp" ,rust-capnp_0_14_3))))
    (home-page "None")
    (synopsis "Cap\u0027n Proto code generation")
    (description
      (beautify-description "Cap\u0027n Proto code generation"))
    (license license:expat)))

(define-public rust-cfg-if_1_0_0
  (package
    (name "rust-cfg-if")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted.")
    (description
      (beautify-description "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-crc32fast_1_2_1
  (package
    (name "rust-crc32fast")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc32fast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06ivjlkzcxxxk7nyshc44aql4zjpmvirq46vmzrakdjax3n6y5c1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
    (description
      (beautify-description "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation"))
    (license license:expat)))

(define-public rust-ctor_0_1_21
  (package
    (name "rust-ctor")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ctor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1am0a8m1gkaa0fii3w3s5wsymjljvg4sv5c50bscssl2kf5a9h6c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80))))
    (home-page "None")
    (synopsis "__attribute__((constructor)) for Rust")
    (description
      (beautify-description "__attribute__((constructor)) for Rust"))
    (license license:asl2.0)))

(define-public rust-either_1_6_1
  (package
    (name "rust-either")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mwl9vngqf5jvrhmhn9x60kr5hivxyjxbmby2pybncxfqhf4z3g7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      (beautify-description "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-flate2_1_0_22
  (package
    (name "rust-flate2")
    (version "1.0.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flate2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gy5iwfqylb2f0dd9n7r8w2xwbzlrqlsairvyj2w9jf1jzl8hs8y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-crc32fast" ,rust-crc32fast_1_2_1)        
        ("rust-libc" ,rust-libc_0_2_104)        
        ("rust-miniz_oxide" ,rust-miniz_oxide_0_4_4))))
    (home-page "https://github.com/rust-lang/flate2-rs")
    (synopsis "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.\nSupports miniz_oxide, miniz.c, and multiple zlib implementations. Supports\nzlib, gzip, and raw deflate streams.")
    (description
      (beautify-description "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.\nSupports miniz_oxide, miniz.c, and multiple zlib implementations. Supports\nzlib, gzip, and raw deflate streams."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-getopts_0_2_21
  (package
    (name "rust-getopts")
    (version "0.2.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getopts" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mgb3qvivi26gs6ihqqhh8iyhp3vgxri6vwyrwg28w0xqzavznql"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-width" ,rust-unicode-width_0_1_9))))
    (home-page "https://github.com/rust-lang/getopts")
    (synopsis "getopts-like option parsing.")
    (description
      (beautify-description "getopts-like option parsing."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-ghost_0_1_2
  (package
    (name "rust-ghost")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ghost" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yalg3g1g3cz63n3phy7cdhh7p2qd220mrpxy96alwxbpqdwynqs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80))))
    (home-page "None")
    (synopsis "Define your own PhantomData")
    (description
      (beautify-description "Define your own PhantomData"))
    (license license:expat)))

(define-public rust-glob_0_3_0
  (package
    (name "rust-glob")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x25wfr7vg3mzxc9x05dcphvd3nwlcmbnxrvwcvrrdwplcrrk4cv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/glob")
    (synopsis "Support for matching file paths against Unix shell style patterns.")
    (description
      (beautify-description "Support for matching file paths against Unix shell style patterns."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-hashbrown_0_11_2
  (package
    (name "rust-hashbrown")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vkjsf5nzs7qcia5ya79j9sq2p1caz4crrncr1675wwyj3ag0pmb"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A Rust port of Google\u0027s SwissTable hash map")
    (description
      (beautify-description "A Rust port of Google\u0027s SwissTable hash map"))
    (license (list license:asl2.0
               license:expat))))

(define-public rust-heck_0_3_3
  (package
    (name "rust-heck")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b0kkr790p66lvzn9nsmfjvydrbmh9z5gb664jchwgw64vxiwqkd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-segmentation" ,rust-unicode-segmentation_1_8_0))))
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      (beautify-description "heck is a case conversion library."))
    (license license:expat)))

(define-public rust-hermit-abi_0_1_19
  (package
    (name "rust-hermit-abi")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hermit-abi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_104))))
    (home-page "None")
    (synopsis "hermit-abi is small interface to call functions from the unikernel RustyHermit.\nIt is used to build the target `x86_64-unknown-hermit`.")
    (description
      (beautify-description "hermit-abi is small interface to call functions from the unikernel RustyHermit.\nIt is used to build the target `x86_64-unknown-hermit`."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-include_dir_0_6_2
  (package
    (name "rust-include_dir")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "include_dir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1arcwvyrpwm3kjxy0kz2hylc1l3hw089y0qgklgdd1v1gqa6xd94"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-glob" ,rust-glob_0_3_0)        
        ("rust-include_dir_impl" ,rust-include_dir_impl_0_6_2)        
        ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Embed the contents of a directory in your binary")
    (description
      (beautify-description "Embed the contents of a directory in your binary"))
    (license license:expat)))

(define-public rust-include_dir_impl_0_6_2
  (package
    (name "rust-include_dir_impl")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "include_dir_impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pw2nv1cy5483jjic5v8i9n3rgbb743wf122rrxsnjyshl68j30a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow_1_0_44)        
        ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80))))
    (home-page "None")
    (synopsis "Implementation crate for include_dir")
    (description
      (beautify-description "Implementation crate for include_dir"))
    (license license:expat)))

(define-public rust-indexmap_1_7_0
  (package
    (name "rust-indexmap")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19b2zwfajhsfcgny0clv8y4jppy704znfhv8nv2dw9a18l2kcqxw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_0_1)        
        ("rust-hashbrown" ,rust-hashbrown_0_11_2))))
    (home-page "None")
    (synopsis "A hash table with consistent order and fast iteration.\n\nThe indexmap is a hash table where the iteration order of the key-value\npairs is independent of the hash values of the keys. It has the usual\nhash table functionality, it preserves insertion order except after\nremovals, and it allows lookup of its elements by either hash table key\nor numerical index. A corresponding hash set type is also provided.\n\nThis crate was initially published under the name ordermap, but it was renamed to\nindexmap.")
    (description
      (beautify-description "A hash table with consistent order and fast iteration.\n\nThe indexmap is a hash table where the iteration order of the key-value\npairs is independent of the hash values of the keys. It has the usual\nhash table functionality, it preserves insertion order except after\nremovals, and it allows lookup of its elements by either hash table key\nor numerical index. A corresponding hash set type is also provided.\n\nThis crate was initially published under the name ordermap, but it was renamed to\nindexmap."))
    (license (list license:asl2.0
               license:expat))))

(define-public rust-indoc_0_3_6
  (package
    (name "rust-indoc")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indoc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n2fd2wm1h005hd7pjgx4gv5ymyq4sxqn8z0ssw6xchgqs5ilx27"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-indoc-impl" ,rust-indoc-impl_0_3_6)        
        ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Indented document literals")
    (description
      (beautify-description "Indented document literals"))
    (license license:expat)))

(define-public rust-indoc-impl_0_3_6
  (package
    (name "rust-indoc-impl")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indoc-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w58yg249kmzsn75kcj34qaxqh839l1hsaj3bzggy3q03wb6s16f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80)        
        ("rust-unindent" ,rust-unindent_0_1_7))))
    (home-page "None")
    (synopsis "Indented document literals")
    (description
      (beautify-description "Indented document literals"))
    (license license:expat)))

(define-public rust-instant_0_1_12
  (package
    (name "rust-instant")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "instant" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b2bx5qdlwayriidhrag8vhy10kdfimfhmb3jnjmsz2h9j1bwnvs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "A partial replacement for std::time::Instant that works on WASM too.")
    (description
      (beautify-description "A partial replacement for std::time::Instant that works on WASM too."))
    (license license:bsd-3)))

(define-public rust-inventory_0_1_10
  (package
    (name "rust-inventory")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inventory" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zzz5sgrkxv1rpim4ihaidzf6jgha919xm4svcrmxjafh3xpw3qg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ctor" ,rust-ctor_0_1_21)        
        ("rust-ghost" ,rust-ghost_0_1_2)        
        ("rust-inventory-impl" ,rust-inventory-impl_0_1_10))))
    (home-page "None")
    (synopsis "Typed distributed plugin registration")
    (description
      (beautify-description "Typed distributed plugin registration"))
    (license license:expat)))

(define-public rust-inventory-impl_0_1_10
  (package
    (name "rust-inventory-impl")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inventory-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lgs8kia3284s34g7078j820cn2viyb6cij86swklwhn93lr9h3m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80))))
    (home-page "None")
    (synopsis "Implementation of macros for the `inventory` crate")
    (description
      (beautify-description "Implementation of macros for the `inventory` crate"))
    (license license:expat)))

(define-public rust-itertools_0_8_2
  (package
    (name "rust-itertools")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1154j48aw913v5jnyhpxialxhdn2sfpl4d7bwididyb1r05jsspm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-either" ,rust-either_1_6_1))))
    (home-page "None")
    (synopsis "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      (beautify-description "Extra iterator adaptors, iterator methods, free functions, and macros."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-itoa_0_4_8
  (package
    (name "rust-itoa")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m1dairwyx8kfxi7ab3b5jc71z1vigh9w4shnhiajji9avzr26dp"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast functions for printing integer primitives to an io::Write")
    (description
      (beautify-description "Fast functions for printing integer primitives to an io::Write"))
    (license license:expat)))

(define-public rust-lazy_static_1_4_0
  (package
    (name "rust-lazy_static")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy_static" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A macro for declaring lazily evaluated statics in Rust.")
    (description
      (beautify-description "A macro for declaring lazily evaluated statics in Rust."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-libc_0_2_104
  (package
    (name "rust-libc")
    (version "0.2.104")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1knzxi1fk75yfz6zqf160yc55awh7gdpx6viwwlikkz1038rcbvv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
      (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license license:expat)))

(define-public rust-lock_api_0_4_5
  (package
    (name "rust-lock_api")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "028izfyraynijd9h9x5miv1vmg6sjnw1v95wgm7f4xlr7h4lsaki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-scopeguard" ,rust-scopeguard_1_1_0))))
    (home-page "None")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      (beautify-description "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std."))
    (license (list license:asl2.0
               license:expat))))

(define-public rust-log_0_4_14
  (package
    (name "rust-log")
    (version "0.4.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04175hv0v62shd82qydq58a48k3bjijmk54v38zgqlbxqkkbpfai"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "A lightweight logging facade for Rust")
    (description
      (beautify-description "A lightweight logging facade for Rust"))
    (license license:expat)))

(define-public rust-memchr_2_4_1
  (package
    (name "rust-memchr")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0smq8xzd40njqpfzv5mghigj91fzlfrfg842iz8x0wqvw2dw731h"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis "Safe interface to memchr.")
    (description
      (beautify-description "Safe interface to memchr."))
    (license (list license:unlicense
               license:expat))))

(define-public rust-miniz_oxide_0_4_4
  (package
    (name "rust-miniz_oxide")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jsfv00hl5rmx1nijn59sr9jmjd4rjnjhh4kdjy8d187iklih9d9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-adler" ,rust-adler_1_0_2)        
        ("rust-autocfg" ,rust-autocfg_1_0_1))))
    (home-page "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (description
      (beautify-description "DEFLATE compression and decompression library rewritten in Rust based on miniz"))
    (license license:asl2.0)))

(define-public rust-multimap_0_8_3
  (package
    (name "rust-multimap")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "multimap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sicyz4n500vdhgcxn4g8jz97cp1ijir1rnbgph3pmx9ckz4dkp5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde_1_0_130))))
    (home-page "None")
    (synopsis "A multimap implementation.")
    (description
      (beautify-description "A multimap implementation."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-num-bigint_0_4_2
  (package
    (name "rust-num-bigint")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-bigint" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dfm16ipm63i37bim3821p4hd5wqrwjvnc6d7cds8fgvypgnirvl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_0_1)        
        ("rust-num-integer" ,rust-num-integer_0_1_44)        
        ("rust-num-traits" ,rust-num-traits_0_2_14))))
    (home-page "https://github.com/rust-num/num-bigint")
    (synopsis "Big integer implementation for Rust")
    (description
      (beautify-description "Big integer implementation for Rust"))
    (license license:expat)))

(define-public rust-num-integer_0_1_44
  (package
    (name "rust-num-integer")
    (version "0.1.44")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-integer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nq152y3304as1iai95hqz8prqnc94lks1s7q05sfjdmcf56kk6j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_0_1)        
        ("rust-num-traits" ,rust-num-traits_0_2_14))))
    (home-page "https://github.com/rust-num/num-integer")
    (synopsis "Integer traits and functions")
    (description
      (beautify-description "Integer traits and functions"))
    (license license:expat)))

(define-public rust-num-traits_0_2_14
  (package
    (name "rust-num-traits")
    (version "0.2.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-traits" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "144j176s2p76azy2ngk2vkdzgwdc0bc8c93jhki8c9fsbknb2r4s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_0_1))))
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description
      (beautify-description "Numeric traits for generic mathematics"))
    (license license:expat)))

(define-public rust-os_str_bytes_2_4_0
  (package
    (name "rust-os_str_bytes")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os_str_bytes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11agh8n3x2l4sr3sxvx6byc1j3ryb1g6flb1ywn0qhq7xv1y3cmg"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Utilities for converting between byte sequences and platform-native strings")
    (description
      (beautify-description "Utilities for converting between byte sequences and platform-native strings"))
    (license license:expat)))

(define-public rust-parking_lot_0_11_2
  (package
    (name "rust-parking_lot")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16gzf41bxmm10x82bla8d6wfppy9ym3fxsmdjyvn61m66s0bf5vx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-instant" ,rust-instant_0_1_12)        
        ("rust-lock_api" ,rust-lock_api_0_4_5)        
        ("rust-parking_lot_core" ,rust-parking_lot_core_0_8_5))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license (list license:asl2.0
               license:expat))))

(define-public rust-parking_lot_core_0_8_5
  (package
    (name "rust-parking_lot_core")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05ij4zxsylx99srbq8qd1k2wiwaq8krkf9y4cqkhvb5wjca8wvnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-instant" ,rust-instant_0_1_12)        
        ("rust-libc" ,rust-libc_0_2_104)        
        ("rust-redox_syscall" ,rust-redox_syscall_0_2_10)        
        ("rust-smallvec" ,rust-smallvec_1_7_0)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license (list license:asl2.0
               license:expat))))

(define-public rust-paste_0_1_18
  (package
    (name "rust-paste")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10587zrlmzhq66yhd0z36fzglf32m1nlhi9bxxm6dgl0gp3j1jj5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-paste-impl" ,rust-paste-impl_0_1_18)        
        ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Macros for all your token pasting needs")
    (description
      (beautify-description "Macros for all your token pasting needs"))
    (license license:expat)))

(define-public rust-paste-impl_0_1_18
  (package
    (name "rust-paste-impl")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dlqzk05cx74522s4iyhyzzhszig4n401pp6r1qg6zmr02r7snnr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Implementation detail of the `paste` crate")
    (description
      (beautify-description "Implementation detail of the `paste` crate"))
    (license license:expat)))

(define-public rust-prjoxide
  (package
    (name "rust-prjoxide")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/gatecat/prjoxide")
              (commit "168bfe54b550aee65648f40e6cca0c88b53d337a")
              (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "19ilxdswaj2clnd6nqm32i3j1s0nndnhss69c84bkyrl05gnm0lj"))))
    (build-system cargo-build-system)
    (inputs `(("m4" ,m4)))
    (arguments
     `(#:cargo-inputs
       (("rust-capnp" ,rust-capnp_0_14_3)        
        ("rust-capnpc" ,rust-capnpc_0_14_4)        
        ("rust-clap" ,rust-clap-3)
        ("rust-flate2" ,rust-flate2_1_0_22)
        ("rust-include_dir" ,rust-include_dir_0_6_2)        
        ("rust-itertools" ,rust-itertools_0_8_2)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-log" ,rust-log_0_4_14)        
        ("rust-multimap" ,rust-multimap_0_8_3)        
        ("rust-num-bigint" ,rust-num-bigint_0_4_2)        
        ("rust-pulldown-cmark" ,rust-pulldown-cmark_0_6_1)        
        ("rust-regex" ,rust-regex_1_5_4)        
        ("rust-ron" ,rust-ron_0_5_1)        
        ("rust-serde" ,rust-serde_1_0_130)        
        ("rust-serde_json" ,rust-serde_json_1_0_68))
       #:phases (modify-phases %standard-phases
                   (add-after 'unpack 'change-directory
                     (lambda _
                       (delete-file "libprjoxide/Cargo.toml")
                       (chdir "libprjoxide/prjoxide")
                       (setenv "CONFIG_SHELL" (which "sh"))
                       #t)))))
    (home-page "https://github.com/gatecat/prjoxide")
    (synopsis "Documenting Lattice's 28nm FPGA parts")
    (description "Documenting Lattice's 28nm FPGA parts")
    (license license:isc)))

(define-public rust-proc-macro-error_1_0_4
  (package
    (name "rust-proc-macro-error")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-error-attr" ,rust-proc-macro-error-attr_1_0_4)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80)        
        ("rust-version_check" ,rust-version_check_0_9_3))))
    (home-page "None")
    (synopsis "Almost drop-in replacement to panics in proc-macros")
    (description
      (beautify-description "Almost drop-in replacement to panics in proc-macros"))
    (license license:expat)))

(define-public rust-proc-macro-error-attr_1_0_4
  (package
    (name "rust-proc-macro-error-attr")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error-attr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-version_check" ,rust-version_check_0_9_3))))
    (home-page "None")
    (synopsis "Attribute macro for proc-macro-error crate")
    (description
      (beautify-description "Attribute macro for proc-macro-error crate"))
    (license license:expat)))

(define-public rust-proc-macro-hack_0_5_19
  (package
    (name "rust-proc-macro-hack")
    (version "0.5.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-hack" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rg0kzsj7lj00qj602d3h77spwfz48vixn1wbjp7a4yrq65w9w6v"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Procedural functionlike!() macros using only Macros 1.1")
    (description
      (beautify-description "Procedural functionlike!() macros using only Macros 1.1"))
    (license license:expat)))

(define-public rust-proc-macro2_1_0_30
  (package
    (name "rust-proc-macro2")
    (version "1.0.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w7fc5mvk7jsfgn1pmiphkvjd0min12zj1y0l1zqpg37pj73bhzd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-xid" ,rust-unicode-xid_0_2_2))))
    (home-page "None")
    (synopsis "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple\ntoken-based libraries from the procedural macro use case.")
    (description
      (beautify-description "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple\ntoken-based libraries from the procedural macro use case."))
    (license license:expat)))

(define-public rust-pulldown-cmark_0_6_1
  (package
    (name "rust-pulldown-cmark")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17lx5cy8rdqdrw0fh3x8zxkpr0ag64q6fs2h5m75kwql4b45q80w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-getopts" ,rust-getopts_0_2_21)        
        ("rust-memchr" ,rust-memchr_2_4_1)        
        ("rust-unicase" ,rust-unicase_2_6_0))))
    (home-page "None")
    (synopsis "A pull parser for CommonMark")
    (description
      (beautify-description "A pull parser for CommonMark"))
    (license license:expat)))

(define-public rust-pyo3_0_13_2
  (package
    (name "rust-pyo3")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hq965lgi25dn578fpn9hjva6zjr1c8rl7lxywijq44aw7lbhds8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-ctor" ,rust-ctor_0_1_21)        
        ("rust-indoc" ,rust-indoc_0_3_6)        
        ("rust-inventory" ,rust-inventory_0_1_10)        
        ("rust-libc" ,rust-libc_0_2_104)        
        ("rust-parking_lot" ,rust-parking_lot_0_11_2)        
        ("rust-paste" ,rust-paste_0_1_18)        
        ("rust-pyo3-macros" ,rust-pyo3-macros_0_13_2)        
        ("rust-unindent" ,rust-unindent_0_1_7))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Bindings to Python interpreter")
    (description
      (beautify-description "Bindings to Python interpreter"))
    (license license:asl2.0)))

(define-public rust-pyo3-macros_0_13_2
  (package
    (name "rust-pyo3-macros")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fxi5lx5dl7xh469gr5xckyjy3r3c5dqypzxcj0fbhzf1hq2qzx4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pyo3-macros-backend" ,rust-pyo3-macros-backend_0_13_2)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Proc macros for PyO3 package")
    (description
      (beautify-description "Proc macros for PyO3 package"))
    (license license:asl2.0)))

(define-public rust-pyo3-macros-backend_0_13_2
  (package
    (name "rust-pyo3-macros-backend")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3-macros-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rjxayd78l10hnyphk03bcvhm0jpsvnzn07lczhy7jsgv3jrgc47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Code generation for PyO3 package")
    (description
      (beautify-description "Code generation for PyO3 package"))
    (license license:asl2.0)))

(define-public rust-quote_1_0_10
  (package
    (name "rust-quote")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01ff7a76f871ggnby57iagw6499vci4bihcr11g6bqzjlp38rg1q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_30))))
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
      (beautify-description "Quasi-quoting macro quote!(...)"))
    (license license:expat)))

(define-public rust-redox_syscall_0_2_10
  (package
    (name "rust-redox_syscall")
    (version "0.2.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zq36bhw4c6xig340ja1jmr36iy0d3djp8smsabxx71676bg70w3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2))))
    (home-page "None")
    (synopsis "A Rust library to access raw Redox system calls")
    (description
      (beautify-description "A Rust library to access raw Redox system calls"))
    (license license:expat)))

(define-public rust-regex_1_5_4
  (package
    (name "rust-regex")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qf479kjbmb582h4d1d6gfl75h0j8aq2nrdi5wg6zdcy6llqcynh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick_0_7_18)        
        ("rust-memchr" ,rust-memchr_2_4_1)        
        ("rust-regex-syntax" ,rust-regex-syntax_0_6_25))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license license:expat)))

(define-public rust-regex-syntax_0_6_25
  (package
    (name "rust-regex-syntax")
    (version "0.6.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16y87hz1bxmmz6kk360cxwfm3jnbsxb3x4zw9x1gzz7khic2i5zl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      (beautify-description "A regular expression parser."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-ron_0_5_1
  (package
    (name "rust-ron")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ron" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mb2bavvp8jg5wx0kx9n45anrsbjwhjzddim987bjaa11hg45kif"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_10_1)        
        ("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-serde" ,rust-serde_1_0_130))))
    (home-page "https://github.com/ron-rs/ron")
    (synopsis "Rusty Object Notation")
    (description
      (beautify-description "Rusty Object Notation"))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-ryu_1_0_5
  (package
    (name "rust-ryu")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vpqv1dj7fksa6hm3zpk5rbsjs0ifbfy7xwzsyyil0rx37a03lvi"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast floating point to string conversion")
    (description
      (beautify-description "Fast floating point to string conversion"))
    (license license:asl2.0)))

(define-public rust-scopeguard_1_1_0
  (package
    (name "rust-scopeguard")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kbqm85v43rq92vx7hfiay6pmcga03vrjbbfwqpyj3pwsg3b16nj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies.")
    (description
      (beautify-description "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-serde_1_0_130
  (package
    (name "rust-serde")
    (version "1.0.130")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04y9s1mxcxakg9bhfdiff9w4zzprk6m6dazcpmpi8nfg6zg0cbgi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde_derive" ,rust-serde_derive_1_0_130))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
      (beautify-description "A generic serialization/deserialization framework"))
    (license license:expat)))

(define-public rust-serde_derive_1_0_130
  (package
    (name "rust-serde_derive")
    (version "1.0.130")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12shxhi47db54i4j44ic2nl299x5p89ngna0w3m6854nn4d1mg6p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-syn" ,rust-syn_1_0_80))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license license:expat)))

(define-public rust-serde_json_1_0_68
  (package
    (name "rust-serde_json")
    (version "1.0.68")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n2jg9cf14lrxasj63rlrwxlw5v79m851gycw6zy20jnjx9hhs8g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-itoa" ,rust-itoa_0_4_8)        
        ("rust-ryu" ,rust-ryu_1_0_5)        
        ("rust-serde" ,rust-serde_1_0_130))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
      (beautify-description "A JSON serialization file format"))
    (license license:expat)))

(define-public rust-smallvec_1_7_0
  (package
    (name "rust-smallvec")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02gka690j8l12gl50ifg7axqnx1m6v6d1byaq0wl3fx66p3vdjhy"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack"))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-strsim_0_10_0
  (package
    (name "rust-strsim")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice.")
    (description
      (beautify-description "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice."))
    (license license:expat)))

(define-public rust-syn_1_0_80
  (package
    (name "rust-syn")
    (version "1.0.80")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "156i2pkz6rqic3zgfgq5wjhgy1gwmrm93aahsr8nv45x7xia246h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_30)        
        ("rust-quote" ,rust-quote_1_0_10)        
        ("rust-unicode-xid" ,rust-unicode-xid_0_2_2))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
      (beautify-description "Parser for Rust source code"))
    (license license:expat)))

(define-public rust-termcolor_1_1_2
  (package
    (name "rust-termcolor")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x65i1ny4m6z1by62ra6wdcrd557p2ysm866x0pg60zby2cxizid"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi-util" ,rust-winapi-util_0_1_5))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis "A simple cross platform library for writing colored text to a terminal.")
    (description
      (beautify-description "A simple cross platform library for writing colored text to a terminal."))
    (license license:expat)))

(define-public rust-textwrap_0_12_1
  (package
    (name "rust-textwrap")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12978qmkl5gcp94lxndpvp9qxq8mxp7hm9xbrw3422dgikchhc10"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-width" ,rust-unicode-width_0_1_9))))
    (home-page "None")
    (synopsis "Powerful library for word wrapping, indenting, and dedenting strings")
    (description
      (beautify-description "Powerful library for word wrapping, indenting, and dedenting strings"))
    (license license:expat)))

(define-public rust-unicase_2_6_0
  (package
    (name "rust-unicase")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicase" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xmlbink4ycgxrkjspp0mf7pghcx4m7vxq7fpfm04ikr2zk7pwsh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-version_check" ,rust-version_check_0_9_3))))
    (home-page "None")
    (synopsis "A case-insensitive wrapper around strings.")
    (description
      (beautify-description "A case-insensitive wrapper around strings."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-unicode-segmentation_1_8_0
  (package
    (name "rust-unicode-segmentation")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-segmentation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nrqfgxkh00wb5dhl0874z20789i2yjimp6ndgh4ay4yjjd895c8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis "This crate provides Grapheme Cluster, Word and Sentence boundaries\naccording to Unicode Standard Annex #29 rules.")
    (description
      (beautify-description "This crate provides Grapheme Cluster, Word and Sentence boundaries\naccording to Unicode Standard Annex #29 rules."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-unicode-width_0_1_9
  (package
    (name "rust-unicode-width")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-width" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wq9wl69wlp6zwlxp660g9p4hm5gk91chwk14dp1gl9bxba45mry"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width of `char` and `str` types\naccording to Unicode Standard Annex #11 rules.")
    (description
      (beautify-description "Determine displayed width of `char` and `str` types\naccording to Unicode Standard Annex #11 rules."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-unicode-xid_0_2_2
  (package
    (name "rust-unicode-xid")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wrkgcw557v311dkdb6n2hrix9dm2qdsb1zpw7pn79l03zb85jwc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31.")
    (description
      (beautify-description "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31."))
    (license license:expat)))

(define-public rust-unindent_0_1_7
  (package
    (name "rust-unindent")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unindent" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1is1gmx1l89z426rn3xsi0mii4vhy2imhqmhx8x2pd8mji6y0kpi"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Remove a column of leading whitespace from a string")
    (description
      (beautify-description "Remove a column of leading whitespace from a string"))
    (license license:expat)))

(define-public rust-vec_map_0_8_2
  (package
    (name "rust-vec_map")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vec_map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1481w9g1dw9rxp3l6snkdqihzyrd2f8vispzqmwjwsdyhw8xzggi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/contain-rs/vec-map")
    (synopsis "A simple map based on a vector for small integer keys")
    (description
      (beautify-description "A simple map based on a vector for small integer keys"))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-version_check_0_9_3
  (package
    (name "rust-version_check")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zmkcgj2m0pq0l4wnhrp1wl1lygf7x2h5p7pvjwc4719lnlxrv2z"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Tiny crate to check the version of the installed/running rustc.")
    (description
      (beautify-description "Tiny crate to check the version of the installed/running rustc."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-winapi_0_3_9
  (package
    (name "rust-winapi")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi-i686-pc-windows-gnu" ,rust-winapi-i686-pc-windows-gnu_0_4_0)        
        ("rust-winapi-x86_64-pc-windows-gnu" ,rust-winapi-x86_64-pc-windows-gnu_0_4_0))))
    (home-page "None")
    (synopsis "Raw FFI bindings for all of Windows API.")
    (description
      (beautify-description "Raw FFI bindings for all of Windows API."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-winapi-i686-pc-windows-gnu_0_4_0
  (package
    (name "rust-winapi-i686-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-i686-pc-windows-gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import libraries for the i686-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the i686-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead."))
    (license (list license:expat
               license:asl2.0))))

(define-public rust-winapi-util_0_1_5
  (package
    (name "rust-winapi-util")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y71bp7f6d536czj40dhqk0d55wfbbwqfp2ymqf1an5ibgl6rv3h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "A dumping ground for high level safe wrappers over winapi.")
    (description
      (beautify-description "A dumping ground for high level safe wrappers over winapi."))
    (license (list license:unlicense
               license:expat))))

(define-public rust-winapi-x86_64-pc-windows-gnu_0_4_0
  (package
    (name "rust-winapi-x86_64-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-x86_64-pc-windows-gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the x86_64-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead."))
    (license (list license:expat
               license:asl2.0))))


rust-prjoxide
