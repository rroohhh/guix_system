(define-module (vup matterhorn)
 #:use-module (guix build-system haskell)
 #:use-module (guix licenses)
 #:use-module (guix gexp)
 #:use-module (guix packages)
 #:use-module (guix download)
 #:use-module (gnu packages haskell-xyz)
 #:use-module (gnu packages haskell-web)
 #:use-module (gnu packages haskell-check)
 #:use-module ((guix licenses) #:prefix license:))

;; (define-public ghc-mattermost-api-qc
;;   (package
;;     (name "ghc-mattermost-api-qc")
;;     (version "50200.13.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "mattermost-api-qc" version))
;;               (sha256
;;                (base32
;;                 "0qy0jzv808hry99clfkc61sfmid8qwv3qcf2gsmana30bdxy8v57"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-mattermost-api ghc-quickcheck))
;;     (home-page "https://github.com/matterhorn-chat/mattermost-api-qc")
;;     (synopsis "QuickCheck instances for the Mattermost client API library")
;;     (description
;;      "This package provides a library providing QuickCheck for the mattermost-api
;; library to allow testing.  This is provided as a separate library to allow use
;; of the API library without testing dependencies.")
;;     (license license:isc)))
;; (define-public ghc-checkers
;;   (package
;;     (name "ghc-checkers")
;;     (version "0.6.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "checkers" version))
;;               (sha256
;;                (base32
;;                 "1r4rsa4k0fy8xig3m530ryflry9viv9v47g4gh7h0ld27rbd6z60"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-random ghc-quickcheck ghc-semigroupoids))
;;     (home-page "https://github.com/haskell-checkers/checkers")
;;     (synopsis "Check properties on standard classes and data structures.")
;;     (description
;;      "''Checkers'' wraps up the expected properties associated with various standard
;; type classes as QuickCheck properties.  Also some morphism properties.  It also
;; provides arbitrary instances and generator combinators for common data types. .
;; &#169; 2008-2013 by Conal Elliott; BSD3 license.")
;;     (license license:bsd-3)))
;; (define-public ghc-unique
;;   (package
;;     (name "ghc-unique")
;;     (version "0.4.7.9")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "Unique" version))
;;               (sha256
;;                (base32
;;                 "14f1qnmhdmbam8qis725dhwq1mk9h86fsnzhkwhsx73ny9z29s1l"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-extra ghc-hashable ghc-unordered-containers))
;;     (native-inputs (list ghc-hspec ghc-quickcheck))
;;     (home-page "http://hackage.haskell.org/package/Unique")
;;     (synopsis "It provides the functionality like unix \"uniq\" utility")
;;     (description
;;      "Library provides the functions to find unique and duplicate elements in the list")
;;     (license license:bsd-3)))
;; (define-public ghc-stm-delay
;;   (package
;;     (name "ghc-stm-delay")
;;     (version "0.1.1.1")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "stm-delay" version))
;;               (sha256
;;                (base32
;;                 "0cla21v89gcvmr1iwzibq13v1yq02xg4h6k9l6kcprj7mhd5hcmi"))))
;;     (build-system haskell-build-system)
;;     (home-page "https://github.com/joeyadams/haskell-stm-delay")
;;     (synopsis "Updatable one-shot timer polled with STM")
;;     (description
;;      "This library lets you create a one-shot timer, poll it using STM, and update it
;; to ring at a different time than initially specified. .  It uses GHC event
;; manager timeouts when available (GHC 7.2+, @-threaded@, non-Windows OS),
;; yielding performance similar to @threadDelay@ and @registerDelay@.  Otherwise,
;; it falls back to forked threads and @threadDelay@. . [0.1.1] Add tryWaitDelayIO,
;; improve performance for certain cases of @newDelay@ and @updateDelay@, and
;; improve example.")
;;     (license license:bsd-3)))
;; (define-public ghc-aspell-pipe
;;   (package
;;     (name "ghc-aspell-pipe")
;;     (version "0.6")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "aspell-pipe" version))
;;               (sha256
;;                (base32
;;                 "09dw4v4j5pmqi8pdh3p7kk7f8pph5w33s7vd21fgvhv3arnrj6p8"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-async))
;;     (home-page "http://hackage.haskell.org/package/aspell-pipe")
;;     (synopsis "Pipe-based interface to the Aspell program")
;;     (description
;;      "This package provides a pipe-based interface to the Aspell program (no dynamic
;; linking required).")
;;     (license license:bsd-3)))
;; (define-public ghc-hclip
;;   (package
;;     (name "ghc-hclip")
;;     (version "3.0.0.4")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "Hclip" version))
;;               (sha256
;;                (base32
;;                 "04ppwm7vfzndrys8x1n8vfb41vzwx59r9xp4dkbiqmrms390pj6q"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-strict))
;;     (home-page "https://github.com/jetho/Hclip")
;;     (synopsis
;;      "A small cross-platform library for reading and modifying the system clipboard.")
;;     (description
;;      "This package provides a small cross-platform library for reading and modifying
;; the system clipboard. .  Hclip works on Windows, Mac OS X and Linux (but see the
;; requirements below!). .  Requirements: . * Windows: No additional requirements.
;; . * Mac OS X: Requires the pbcopy and pbpaste commands, which ship with Mac OS
;; X. . * Linux: Requires xclip or xsel installed.")
;;     (license license:bsd-3)))
;; (define-public ghc-brick-skylighting
;;   (package
;;     (name "ghc-brick-skylighting")
;;     (version "0.3")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "brick-skylighting" version))
;;               (sha256
;;                (base32
;;                 "1wjl5ff9c7czg7azj2pi17b3kzbgb5rmwb4nkxdy86xn6d68adi1"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-brick ghc-vty ghc-skylighting-core))
;;     (home-page "https://github.com/jtdaugherty/brick-skylighting/")
;;     (synopsis "Show syntax-highlighted text in your Brick UI")
;;     (description
;;      "This package provides a module to use Skylighting to perform syntax highlighting
;; and display the results in Brick-based interfaces.")
;;     (license license:bsd-3)))
;; (define-public ghc-vty
;;   (package
;;     (name "ghc-vty")
;;     (version "5.35.1")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "vty" version))
;;               (sha256
;;                (base32
;;                 "062dpz8fxrnggzpl041zpbph0xj56jki98ajm2s78dldg5vy0c9k"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-blaze-builder
;;                   ghc-microlens
;;                   ghc-microlens-mtl
;;                   ghc-microlens-th
;;                   ghc-hashable
;;                   ghc-parallel
;;                   ghc-utf8-string
;;                   ghc-vector
;;                   ghc-ansi-terminal))
;;     (native-inputs (list ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-random
;;                          ghc-quickcheck
;;                          ghc-smallcheck
;;                          ghc-quickcheck-assertions
;;                          ghc-test-framework
;;                          ghc-test-framework-smallcheck
;;                          ghc-random
;;                          ghc-hunit
;;                          ghc-quickcheck
;;                          ghc-smallcheck
;;                          ghc-quickcheck-assertions
;;                          ghc-test-framework
;;                          ghc-test-framework-smallcheck
;;                          ghc-test-framework-hunit
;;                          ghc-random
;;                          ghc-string-qq))
;;     (home-page "https://github.com/jtdaugherty/vty")
;;     (synopsis "A simple terminal UI library")
;;     (description
;;      "vty is terminal GUI library in the niche of ncurses.  It is intended to be easy
;; to use, have no confusing corner cases, and good support for common terminal
;; types. .  See the @vty-examples@ package as well as the program
;; @test/interactive_terminal_test.hs@ included in the @vty@ package for examples
;; on how to use the library. .  Import the \"Graphics.Vty\" convenience module to
;; get access to the core parts of the library. . &#169; 2006-2007 Stefan O'Rear;
;; BSD3 license. . &#169; Corey O'Connor; BSD3 license. . &#169; Jonathan
;; Daugherty; BSD3 license.")
;;     (license license:bsd-3)))
;; (define-public ghc-brick
;;   (package
;;     (name "ghc-brick")
;;     (version "0.70.1")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "brick" version))
;;               (sha256
;;                (base32
;;                 "18i1i06ll6pklzaazcl2bzbi3w5zdn43l9wvkclhfcmddjy19lp4"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-vty
;;                   ghc-data-clist
;;                   ghc-dlist
;;                   ghc-microlens
;;                   ghc-microlens-th
;;                   ghc-microlens-mtl
;;                   ghc-config-ini
;;                   ghc-vector
;;                   ghc-contravariant
;;                   ghc-text-zipper
;;                   ghc-word-wrap
;;                   ghc-random))
;;     (native-inputs (list ghc-quickcheck))
;;     (home-page "https://github.com/jtdaugherty/brick/")
;;     (synopsis "A declarative terminal user interface library")
;;     (description
;;      "Write terminal user interfaces (TUIs) painlessly with 'brick'! You write an
;; event handler and a drawing function and the library does the rest. . . > module
;; Main where > > import Brick > > ui :: Widget () > ui = str \"Hello, world!\" > >
;; main :: IO () > main = simpleMain ui . .  To get started, see: . *
;; <https://github.com/jtdaugherty/brick/blob/master/README.md The README> . * The
;; <https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst Brick user
;; guide> . * The demonstration programs in the 'programs' directory . .  This
;; package deprecates <http://hackage.haskell.org/package/vty-ui vty-ui>.")
;;     (license license:bsd-3)))
;; (define-public ghc-modern-uri
;;   (package
;;     (name "ghc-modern-uri")
;;     (version "0.3.4.4")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "modern-uri" version))
;;               (sha256
;;                (base32
;;                 "19fffy7kb7ibajagdryjy872x56045zi6c1div8wvr8aisd55qsz"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-quickcheck
;;                   ghc-contravariant
;;                   ghc-megaparsec
;;                   ghc-profunctors
;;                   ghc-reflection
;;                   ghc-tagged))
;;     (native-inputs (list ghc-hspec hspec-discover ghc-hspec-megaparsec))
;;     (home-page "https://github.com/mrkkrp/modern-uri")
;;     (synopsis "Modern library for working with URIs")
;;     (description "Modern library for working with URIs.")
;;     (license license:bsd-3)))
;; (define-public ghc-random-1.1
;;   (package
;;     (name "ghc-random")
;;     (version "1.1")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "random" version))
;;               (sha256
;;                (base32
;;                 "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p"))))
;;     (build-system haskell-build-system)
;;     (arguments
;;      `(#:cabal-revision ("1"
;;                          "1pv5d7bm2rgap7llp5vjsplrg048gvf0226y0v19gpvdsx7n4rvv")))
;;     (home-page "http://hackage.haskell.org/package/random")
;;     (synopsis "random number library")
;;     (description
;;      "This package provides a basic random number generation library, including the
;; ability to split random number generators.")
;;     (license license:bsd-3)))
;; (define-public ghc-quickcheck-2.13.1
;;   (package
;;     (name "ghc-quickcheck")
;;     (version "2.13.1")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append
;;              "https://hackage.haskell.org/package/QuickCheck/QuickCheck-"
;;              version
;;              ".tar.gz"))
;;        (sha256
;;         (base32
;;          "1inri6n4rr7v7wrmajikcqmbjh77lvf9m4fw2ib6szdgwyb3lim6"))))
;;     (build-system haskell-build-system)
;;     (inputs
;;      (list ghc-random-1.1 ghc-splitmix-bootstrap))
;;     (home-page "https://github.com/nick8325/quickcheck")
;;     (synopsis "Automatic testing of Haskell programs")
;;     (description
;;      "QuickCheck is a library for random testing of program properties.  The
;; programmer provides a specification of the program, in the form of properties
;; which functions should satisfy, and QuickCheck then tests that the properties
;; hold in a large number of randomly generated cases.  Specifications are
;; expressed in Haskell, using combinators defined in the QuickCheck library.")
;;     (license license:bsd-3)))
;; (define-public ghc-http-media
;;   (package
;;     (name "ghc-http-media")
;;     (version "0.8.0.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "http-media" version))
;;               (sha256
;;                (base32
;;                 "0lww5cxrc9jlvzsysjv99lca33i4rb7cll66p3c0rdpmvz8pk0ir"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-case-insensitive ghc-utf8-string))
;;     (native-inputs (list ghc-quickcheck-2.13.1 ghc-test-framework
;;                          ghc-test-framework-quickcheck2))
;;     (arguments
;;      `(#:cabal-revision ("6"
;;                          "08r6a83awajyyv9wwd2i17vvksvg9b7d8kyblfi4j3ab1bz62grv")))
;;     (home-page "https://github.com/zmthy/http-media")
;;     (synopsis "Processing HTTP Content-Type and Accept headers")
;;     (description
;;      "This library is intended to be a comprehensive solution to parsing and selecting
;; quality-indexed values in HTTP headers.  It is capable of parsing both media
;; types and language parameters from the Accept and Content header families, and
;; can be extended to match against other accept headers as well.  Selecting the
;; appropriate header value is achieved by comparing a list of server options
;; against the quality-indexed values supplied by the client. .  In the following
;; example, the Accept header is parsed and then matched against a list of server
;; options to serve the appropriate media using 'mapAcceptMedia': . > getHeader >>=
;; maybe send406Error sendResourceWith .  mapAcceptMedia >     [ (\"text/html\",
;;   asHtml) >     , (\"application/json\", asJson) >     ] .  Similarly, the
;; Content-Type header can be used to produce a parser for request bodies based on
;; the given content type with 'mapContentMedia': . > getContentType >>= maybe
;; send415Error readRequestBodyWith .  mapContentMedia >     [ (\"application/json\",
;; parseJson) >     , (\"text/plain\",       parseText) >     ] .  The API is
;; agnostic to your choice of server.")
;;     (license license:expat)))
;; (define-public ghc-mattermost-api
;;   (package
;;     (name "ghc-mattermost-api")
;;     (version "50200.13.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "mattermost-api" version))
;;               (sha256
;;                (base32
;;                 "128x1ygfb8g8ig4wy3cgmjh3ijkrg7vg27ylxvb1rnn4hc4lfn7i"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-websockets
;;                   ghc-aeson
;;                   ghc-connection
;;                   ghc-memory
;;                   ghc-resource-pool
;;                   ghc-http
;;                   ghc-http-media
;;                   ghc-network-uri
;;                   ghc-modern-uri
;;                   ghc-unordered-containers
;;                   ghc-hashable
;;                   ghc-gitrev
;;                   ghc-microlens
;;                   ghc-microlens-th
;;                   ghc-pretty-show
;;                   ghc-split))
;;     (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-hunit))
;;     (home-page "http://hackage.haskell.org/package/mattermost-api")
;;     (synopsis "Client API for Mattermost chat system")
;;     (description
;;      "Client API for Mattermost chat system.  Mattermost is a flexible, open source
;; messaging platform that meets even the most demanding privacy and security
;; standards.  This library provides network API interaction with the Mattermost
;; server.")
;;     (license license:bsd-3)))
;; (define-public matterhorn
;;   (package
;;     (name "matterhorn")
;;     (version "50200.17.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (hackage-uri "matterhorn" version))
;;               (sha256
;;                (base32
;;                 "10n0vgphdkbdhyadiwn2a35zdazmxz2k0ibq7f1v9iyr8ylshz46"))))
;;     (build-system haskell-build-system)
;;     (inputs (list ghc-mattermost-api
;;                   ghc-base-compat
;;                   ghc-unordered-containers
;;                   ghc-split
;;                   ghc-data-clist
;;                   ghc-semigroups
;;                   ghc-connection
;;                   ghc-config-ini
;;                   ghc-microlens-platform
;;                   ghc-brick
;;                   ghc-brick-skylighting
;;                   ghc-vty
;;                   ghc-word-wrap
;;                   ghc-text-zipper
;;                   ghc-xdg-basedir
;;                   ghc-vector
;;                   ghc-strict
;;                   ghc-hashable
;;                   ghc-commonmark
;;                   ghc-commonmark-extensions
;;                   ghc-utf8-string
;;                   ghc-temporary
;;                   ghc-gitrev
;;                   ghc-hclip
;;                   ghc-aspell-pipe
;;                   ghc-stm-delay
;;                   ghc-skylighting-core
;;                   ghc-timezone-olson
;;                   ghc-timezone-series
;;                   ghc-aeson
;;                   ghc-async
;;                   ghc-uuid
;;                   ghc-random
;;                   ghc-network-uri))
;;     (native-inputs (list ghc-unique
;;                          ghc-checkers
;;                          ghc-tasty
;;                          ghc-tasty-hunit
;;                          ghc-tasty-quickcheck
;;                          ghc-mattermost-api-qc))
;;     (home-page "http://hackage.haskell.org/package/matterhorn")
;;     (synopsis "Terminal client for the Mattermost chat system")
;;     (description
;;      "This is a terminal client for the Mattermost chat system.  Please see the README
;; for a list of features and information on getting started.")
;;     (license license:bsd-3)))
