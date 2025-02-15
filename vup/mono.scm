(define-module (vup mono)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages)
  #:use-module (games packages mono)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))


(define-public mono
  (package
    (name "mono")
    (version "6.12.0.199")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.mono-project.com/sources/mono/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xcf8wrz0n5m0lwis5bz06h18shc94a0jpyl70ibm9jkada0v1f0"))))
              ;; (patches (search-patches "mono-pkgconfig-before-gac.patch"))))
                                       ;; TODO: Update this patch.
                                       ;; "mono-mdoc-timestamping.patch"

    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("python" ,python)
       ("cmake" ,cmake)
       ("which" ,which)
       ("libgdiplus" ,libgdiplus)
       ("libx11" ,libx11)
       ;; TODO: Test if these 2 are necessary.
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "mono/mini/Makefile.in"
               (("build_date = [^;]*;")
                "build_date = (void*) 0;"))
             #t))
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "mono/eglib/test/path.c"
               (("const gchar \\*newdir = \"/bin\";")
                (string-append "const gchar *newdir = \"/tmp\";")))
             #t))
         ;; TODO: Update Mono certs.  We need a certificate bundle, which nss-certs does not have.
         ;; (add-after 'install 'update-mono-key-store
         ;;   (lambda* (#:key outputs inputs #:allow-other-keys)
         ;;     (let* ((out (assoc-ref outputs "out"))
         ;;            (ca (assoc-ref inputs "nss-certs"))
         ;;            (cert-sync (string-append out "/bin/cert-sync"))))
         ;;     (invoke cert-sync (string-append ca "/etc/ssl/certs/ca-bundle.crt")
         (add-after 'install 'install-gmcs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (symlink (string-append out "/bin/mcs")
                        (string-append out "/bin/gmcs")))
             #t))
         (add-after 'unpack 'set-env
           (lambda _ ;;* (#:key inputs #:allow-other-keys)
             ;; all tests under mcs/class fail trying to access $HOME
             (setenv "HOME" "/tmp")
             ;; ZIP files have "DOS time" which starts in Jan 1980.
             (setenv "SOURCE_DATE_EPOCH" "315532800")
             #t))
         ;; TODO: This fix seems obsolete in Mono 6.
         ;; (add-after 'unpack 'fix-includes
         ;;   (lambda _
         ;;     ;; makedev is in <sys/sysmacros.h> now.  Include it.
         ;;     (substitute* "mono/io-layer/processes.c"
         ;;      (("#ifdef HAVE_SYS_MKDEV_H") "#if 1")
         ;;      (("sys/mkdev.h") "sys/sysmacros.h"))
         ;;     #t))
         ;; TODO: Those patches don't seem to be useful anymore.
         (add-after 'unpack 'patch-tests
           (lambda _  ;;* (#:key inputs #:allow-other-keys)
             (substitute* "mono/tests/Makefile.in"
               ;; does not build: no rule to make unhandled-exception-*
               (("@test-unhandled-exception-2:" all)
                (string-append all "#")))
             (substitute* "mcs/tools/mono-symbolicate/Makefile"
               ;; does not build: Source file `Test/StackTraceDumper.cs'
               ;; could not be found
               (("^check: test-local") "check:\ntest-local:")
               (("^test-local: all") "disabled-test-local:"))
             (substitute* "mono/unit-tests/Makefile.in"
               ;; test fails
               (("^test-sgen-qsort.log:")
                "disabled-test-sgen-qsort.log:\ntest-sgen-qsort.log:"))
             ;; tests fail, trying to access $HOME
             (substitute* "mcs/class/Makefile"
               (("^include ../build/rules.make" all)
                (string-append
                 all
                 "\nrun-test-recursive:\n\t@echo skipping tests\n")))
             ;; tests fail, trying to access $HOME
             (substitute* "mcs/class/Microsoft.Build.Tasks/Makefile"
               (("^include ../../build/rules.make" all)
                (string-append
                 all
                 "\nrun-test-recursive:\n\t@echo skipping tests\n")))
             (substitute* '("mcs/tools/mono-shlib-cop/Makefile"
                            "mcs/tools/mdoc/Makefile")
               (("^run-test-local:" all)
                (string-append "#" all)))
             (substitute* "mcs/tools/sqlmetal/Makefile"
               (("^include ../../build/rules.make" all)
                (string-append
                 "NO_TEST:=true\n"
                 all
                 "\nrun-test-lib:\n\t@echo skipping test\n"))))))
       ;; TODO: Do these 4 tests still fail?
       #:make-flags `(,(string-append "PLATFORM_DISABLED_TESTS="
                                      " appdomain-unload.exe"
                                      " delegate2.exe"
                                      " finally_guard.exe"
                                      " remoting4.exe"))
       #:configure-flags (list
                          (string-append "--x-includes="
                                         (assoc-ref %build-inputs "libx11")
                                         "/include")
                          (string-append "--x-libraries="
                                         (assoc-ref %build-inputs "libx11")
                                         "/lib")
                          (string-append "--with-libgdiplus="
                                         (assoc-ref %build-inputs "libgdiplus")
                                         "/lib/libgdiplus.so"))
       #:tests? #f))
    (synopsis "Compiler and libraries for the C# programming language")
    (description "Mono is a compiler, vm, debugger and set of libraries for
C#, a C-style programming language from Microsoft that is very similar to
Java.")
    (home-page "https://www.mono-project.com/")
    ;; TODO: Still x11?
    (license license:x11)))
