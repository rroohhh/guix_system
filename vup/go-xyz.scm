(define-module (vup go-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public vault
  (package
    (name "vault")
    (version "1.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.hashicorp.com/vault/" version "/vault_" version "_linux_amd64.zip"))
       (sha256
        (base32 "19lnwya2xq6qi2x3jf4yanq1znkpx9r6da6l418m21h8ng81a46g"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (invoke (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip") (assoc-ref %build-inputs "source"))
         (install-file "vault" (string-append %output "/bin"))
         #t)))
    (home-page
      "https://www.vaultproject.io")
    (synopsis "A tool for managing secrets")
    (description
      "A tool for managing secrets")
    (license license:mpl2.0)))

(define-public rtsp-simple-server
  (package
    (name "rtsp-simple-server")
    (version "0.21.6")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/aler9/rtsp-simple-server/releases/download/v" version "/rtsp-simple-server_v" version "_linux_amd64.tar.gz"""))
       (sha256
        (base32 "1w8hjndviylh9nm023dm003xrsac5pk6g1wgww0rnw9vczfv6bx0"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("rtsp-simple-server" "bin/rtsp-simple-server")
         ("rtsp-simple-server.yml" "etc/rtsp-simple-server.yml"))))
    (home-page
      "https://github.com/aler9/rtsp-simple-server")
    (synopsis "ready-to-use RTSP / RTMP / HLS server and proxy that allows to read, publish and proxy video and audio streams")
    (description
      "ready-to-use RTSP / RTMP / HLS server and proxy that allows to read, publish and proxy video and audio streams")
    (license #f))) ;; mit



(define-public go-1.21
 (package
  (inherit go-1.20)
  (name "go")
  (version "1.21.4")
  (source (origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/golang/go")
                 (commit (string-append "go" version))))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "0xp1mqjbbs53bjg00d4a37af5p1by28xnflj2xi5kchcpmlqn5nz"))))
  (arguments
   (substitute-keyword-arguments (package-arguments go-1.20)
 ;; Source patching phases are broken up into discrete steps to allow
 ;; future versions to discard individual phases without having to
 ;; discard all source patching.
    ((#:phases phases)
     #~(modify-phases #$phases
        (delete 'skip-TestGoPathShlibGccgo-tests)
        (delete 'patch-source)
        (add-after 'unpack 'patch-os-tests
         (lambda _
          (substitute* "src/os/os_test.go"
           (("/usr/bin") (getcwd))
           (("/bin/sh") (which "sh")))))

        (add-after 'unpack 'apply-patches
         (lambda* (#:key inputs #:allow-other-keys)
 ;; Having the patch in the 'patches' field of <origin> breaks
 ;; the 'TestServeContent' test due to the fact that
 ;; timestamps are reset. Thus, apply it from here.
          (invoke "patch" "-p1" "--force" "-i"
           (assoc-ref inputs "go-fix-script-tests.patch"))))

        (add-after 'unpack 'patch-src/net
         (lambda* (#:key inputs #:allow-other-keys)
          (let ((net-base (assoc-ref inputs "net-base")))
           (substitute* "src/net/lookup_unix.go"
            (("/etc/protocols")
             (string-append net-base "/etc/protocols")))
           (substitute* "src/net/port_unix.go"
            (("/etc/services")
             (string-append net-base "/etc/services"))))))

        (add-after 'unpack 'patch-zoneinfo
         (lambda* (#:key inputs #:allow-other-keys)
 ;; Add the path to this specific version of tzdata's zoneinfo
 ;; file to the top of the list to search. We don't want to
 ;; replace any sources because it will affect how binaries
 ;; compiled with this Go toolchain behave on non-guix
 ;; platforms.
          (substitute* "src/time/zoneinfo_unix.go"
           (("var platformZoneSources.+" all)
            (format #f "~a~%\"~a/share/zoneinfo\",~%"
             all
             (assoc-ref inputs "tzdata"))))))

        (add-after 'unpack 'patch-cmd/go/testdata/script
         (lambda _
          (substitute* "src/cmd/go/testdata/script/cgo_path_space.txt"
           (("/bin/sh") (which "sh")))))

        (add-after 'enable-external-linking 'enable-external-linking-1.21
         (lambda _
 ;; Invoke GCC to link any archives created with GCC (that is, any
 ;; packages built using 'cgo'), because Go doesn't know how to
 ;; handle the runpaths but GCC does. Use substitute* rather than
 ;; a patch since these files are liable to change often.
 ;;
 ;; XXX: Replace with GO_EXTLINK_ENABLED=1 or similar when
 ;; <https://github.com/golang/go/issues/31544> and/or
 ;; <https://github.com/golang/go/issues/43525> are resolved.
          (substitute* "src/cmd/link/internal/ld/config.go"
           (("\\(iscgo && \\(.+\\)") "iscgo"))
          (substitute* "src/internal/testenv/testenv.go"
           (("!CanInternalLink.+") "true {\n"))
          (substitute* "src/syscall/exec_linux_test.go"
           (("testenv.MustHaveExecPath\\(t, \"whoami\"\\)")
            "t.Skipf(\"no passwd file present\")"))))

        (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
 ;; Notably, we do not install archives (180M), which Go will
 ;; happily recompile quickly (and cache) if needed, almost
 ;; surely faster than they could be substituted.
 ;;
 ;; The main motivation for pre-compiled archives is to use
 ;; libc-linked `net' or `os' packages without a C compiler,
 ;; but on Guix a C compiler is necessary to properly link the
 ;; final binaries anyway. Many build flags also invalidate
 ;; these pre-compiled archives, so in practice Go often
 ;; recompiles them anyway.
 ;;
 ;; Upstream is also planning to no longer install these
 ;; archives: <https://github.com/golang/go/issues/47257>
 ;;
 ;; When necessary, a custom pre-compiled library package can
 ;; be created with `#:import-path "std"' and used with
 ;; `-pkgdir'.
 ;;
 ;; When moving files into place, any files that come from
 ;; GOROOT should remain in GOROOT to continue functioning. If
 ;; they need to be referenced from some other directory, they
 ;; need to be symlinked from GOROOT. For more information,
 ;; please see https://github.com/golang/go/issues/61921
          (let* ((out (assoc-ref outputs "out"))
                 (tests (assoc-ref outputs "tests")))
           (for-each
            (lambda (file)
             (copy-recursively file (string-append out "/lib/go/" file)))
            '("bin" "go.env" "lib" "VERSION" "pkg/include" "pkg/tool"))

           (symlink "lib/go/bin" (string-append out "/bin"))

           (for-each
            (match-lambda
             ((file dest output)
 ;; Copy to output/dest and symlink from output/lib/go/file.
              (let ((file* (string-append output "/lib/go/" file))
                    (dest* (string-append output "/" dest)))
               (copy-recursively file dest*)
               (mkdir-p (dirname file*))
               (symlink (string-append "../../" dest) file*))))
            `(("src" "share/go/src" ,out)
              ("misc" "share/go/misc" ,out)
              ("doc" "share/doc/go/doc" ,out)
              ("api" "share/go/api" ,tests)
              ("test" "share/go/test" ,tests))))))))))))
            ;; (delete 'patch-source)))))))
