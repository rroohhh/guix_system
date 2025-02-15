(define-module (vup ispc)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses))


(define-public ispc
  (let*
      ((commit "0985ef0fb6204891c29131432877df8c5c707e01")
       (version (string-append "v1.16.1-g" (string-take commit 9))))
    (package
       (name "ispc")
       (version version)
       (source
        (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/ispc/ispc")
                (commit commit)))
         (file-name (git-file-name name version))
         (patches '("ispc.patch"))
         (sha256
          (base32 "1b9763729dqxc50prdhca520nn69dy4fnmy3ckdkmc9cgh516vv1"))))
       (build-system cmake-build-system)
       (native-inputs `(("git" ,git) ("m4" ,m4) ("bison" ,bison) ("flex" ,flex)
                        ("gcc" ,clang-toolchain-12)))
       (inputs `(("gcc" ,clang-toolchain-12) ("python" ,python)
                 ("ncurses" ,ncurses)))
       (arguments `(#:configure-flags (list
                                       "-DWASM_ENABLED=NO" ; needs emscripten
                                       "-DGENX_ENABLED=NO")
                    #:modules ((srfi srfi-1)
                               (ice-9 match)
                               ,@%cmake-build-system-modules)
                    #:phases
                    (modify-phases (@ (guix build cmake-build-system) %standard-phases)
                      (add-after 'set-paths 'remove-glibc
                        (lambda* (#:key inputs #:allow-other-keys)
                          (let* ((filters '("libc"))
                                 (input-directories
                                  (filter-map (lambda (input)
                                                (match input
                                                  ((name . dir)
                                                   (and (not (member name filters))
                                                        dir))))
                                              inputs)))
                            (set-path-environment-variable "CPLUS_INCLUDE_PATH"
                                                           '("include")
                                                           input-directories)
                            (set-path-environment-variable "C_INCLUDE_PATH"
                                                           '("include")
                                                           input-directories)
                            #t))))))


       (home-page "https://ispc.github.io/")
       (synopsis "Intel Implicit SPMD Program Compiler")
       (description "Intel Implicit SPMD Program Compiler")
       (license #f))))
