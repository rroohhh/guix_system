(define-module (vup scrabble)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module ((guix licenses) #:prefix license:))

(define-public scrabble-server
  (package
    (name "scrabble-server")
    (version "0.23-120")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://files.niemo.de/ScrabbleServer_V0.23_B120_D202010171549"))
              (sha256
               (base32
                "18zypmj4vbb330ry8rxh0g5yshl5z2jvjwgkkjkfkp0ld1w0fa5x"))))
    (build-system trivial-build-system)
    (native-inputs `(("libc" ,glibc)
                     ("patchelf" ,patchelf)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((bin (string-append %output "/bin"))
                          (ld-so (string-append (assoc-ref %build-inputs
                                                           "libc")
                                                ,(glibc-dynamic-linker)))
                          (patchelf (string-append (assoc-ref %build-inputs
                                                              "patchelf")
                                                   "/bin/patchelf")))
                     (copy-file (assoc-ref %build-inputs "source")
                                "ScrabbleServer")
                     (install-file "ScrabbleServer" bin)
                     (chmod (string-append bin "/ScrabbleServer") #o755)
                     (invoke patchelf "--set-interpreter" ld-so
                             (string-append bin "/ScrabbleServer"))) #t)))
    (home-page "https://git.froheiyd.de/dirk/Scrabble")
    (synopsis "Multiplayer cross platform scrabble server in go")
    (description "Multiplayer cross platform scrabble server in go")
    (license license:mpl2.0)))
