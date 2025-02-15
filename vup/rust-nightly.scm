(define-module (vup rust-nightly))

(use-modules (gnu packages))
(use-modules (gnu packages rust))
(use-modules (gnu packages ninja))
(use-modules (gnu packages curl))
(use-modules (gnu packages libffi))
(use-modules (gnu packages web))
(use-modules (gnu packages compression))
(use-modules (gnu packages gdb))
(use-modules (gnu packages linux))
(use-modules (gnu packages cmake))
(use-modules (gnu packages jemalloc))
(use-modules (guix packages))
(use-modules (guix search-paths))
(use-modules (guix build utils))
(use-modules (guix utils))
(use-modules (ice-9 match))


(use-modules (gnu packages llvm))
(use-modules (guix download))
(use-modules (guix build-system cmake))
(use-modules ((guix licenses) #:prefix license:))

(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    (_                (nix-system->gnu-triplet system))))

(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define* (rust-uri version #:key date (dist "static"))
  (string-append "https://" dist ".rust-lang.org/dist/"
                 (if date (string-append date "/") "")
                 "rustc-" version "-src.tar.gz"))

(define* (rust-bootstrapped-package base-rust version checksum #:key date)
  "Bootstrap rust VERSION with source checksum CHECKSUM using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
      (origin
        (inherit (package-source base-rust))
        (uri (rust-uri version #:date date))
        (sha256 (base32 checksum))))
    (native-inputs
     (cons*
      `("clang-source" ,(package-source clang-runtime-18))
      `("gdb" ,gdb/pinned)
      `("procps" ,procps)
       (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust)))))))

(define-public rust-nightly
  (let ((base-rust
         (rust-bootstrapped-package rust-1.82 "1.83.0"
           "0vhwhk4cbyppnz0lcazfjyddyz811fgvadfxswldicpashxpfbbj")))
    (package
      (inherit base-rust)
      (name "rust-nightly")
      (outputs '("out" "doc" "cargo" "rust-src" "tools"))
      (source
        (origin
          (inherit (package-source base-rust))
          (snippet
           '(begin
;              (for-each delete-file-recursively
;                        '("src/llvm-project"
;                          "vendor/tikv-jemalloc-sys/jemalloc"))
              ;; Remove vendored dynamically linked libraries.
              ;; find . -not -type d -executable -exec file {} \+ | grep ELF
              ;; Also remove the bundled (mostly Windows) libraries.
              (for-each delete-file
                        (find-files "vendor" "\\.(a|dll|exe|lib)$"))))))
      (inputs (modify-inputs (package-inputs base-rust)
                             (append ninja)
                             (append cmake)
                             (append jemalloc)
                             (append curl)
                             (append libffi)
                             (append `(,nghttp2 "lib"))
                             (append zlib)
                             (replace "llvm" llvm-18)))

      (native-search-paths
       (cons
         ;; For HTTPS access, Cargo reads from a single-file certificate
         ;; specified with $CARGO_HTTP_CAINFO. See
         ;; https://doc.rust-lang.org/cargo/reference/environment-variables.html
         (search-path-specification
          (variable "CARGO_HTTP_CAINFO")
          (file-type 'regular)
          (separator #f)              ;single entry
          (files '("etc/ssl/certs/ca-certificates.crt")))
         ;; rustc invokes gcc, so we need to set its search paths accordingly.
         %gcc-search-paths))
      (arguments
       (substitute-keyword-arguments (package-arguments rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'check) ;; TODO(robin): remove again, just delete for testing
             (replace 'adjust-rpath-values
               ;; This adds %output:out to rpath, allowing us to install utilities in
               ;; different outputs while reusing the shared libraries.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (substitute* "src/bootstrap/src/core/builder.rs"
                     ((" = rpath.*" all)
                      (string-append all
                                     "                "
                                     "self.rustflags.arg(\"-Clink-args=-Wl,-rpath="
                                     out "/lib\");\n"))))))
             (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gcc (assoc-ref inputs  "gcc")))
                   ;; Hide GCC's C++ headers so that they do not interfere with
                   ;; the ones we are attempting to build.
                   (setenv "CPLUS_INCLUDE_PATH"
                           (string-join
                            (cons (string-append
                                   (assoc-ref inputs "gcc") "/include/c++/x86_64-unknown-linux-gnu")
                                  (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                #\:))
                            ":"))
                   (format #true
                           "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                           (getenv "CPLUS_INCLUDE_PATH")))))
             ;; (add-after 'unpack 'unpack-profiler-rt
             ;;   ;; Copy compiler-rt sources to where libprofiler_builtins looks
             ;;   ;; for its vendored copy.
             ;;   (lambda* (#:key inputs #:allow-other-keys)
             ;;     (mkdir-p "src/llvm-project/compiler-rt")
             ;;     (copy-recursively
             ;;       (string-append (assoc-ref inputs "clang-source")
             ;;                      "/compiler-rt")
             ;;       "src/llvm-project/compiler-rt")))
             ;; (add-after 'configure 'add-gdb-to-config
             ;;   (lambda* (#:key inputs #:allow-other-keys)
             ;;     (let ((gdb (assoc-ref inputs "gdb")))
             ;;       (substitute* "config.toml"
             ;;         (("^python =.*" all)
             ;;          (string-append all
             ;;                         "gdb = \"" gdb "/bin/gdb\"\n"))))))
             (add-after 'configure 'enable-extended
               (lambda* (#:key outputs #:allow-other-keys)
                 (substitute* "config.toml"
                   (("\\[llvm\\]")
                    "[llvm]
download-ci-llvm = false")
                   (("submodules = false")
                    "submodules = false
sanitizers = true
profiler = true
extended = true
# target = [\"x86_64-unknown-linux-gnu\",\"armv7-none-eabi\",\"armv7a-none-eabi\",\"thumbv7m-none-eabi\",\"thumbv6m-none-eabi\",\"armv7-none-eabihf\",\"armv7a-none-eabihf\",\"thumbv7em-none-eabi\",\"thumbv7em-none-eabihf\",\"wasm32-unknown-unknown\",\"armv7-unknown-linux-musleabihf\"]
tools = [\"cargo\",  \"rust-demangler\", \"clippy\", \"rustfmt\", \"rustdoc\", \"analysis\", \"src\", \"rust-analyzer\", \"rust-analyzer-proc-macro-srv\"]"))
                 (substitute* "config.toml"
;                   (("target\\.x86.*")
;                    "target]
;")
                   (("jemalloc=true")
                    "jemalloc=true
codegen-backends=[\"llvm\", \"cranelift\"]
lld=true
use-lld=true
llvm-tools=true
"))
                 #t))
             (add-after 'configure 'fix-lld-compilation
                (lambda* _
                  (invoke "cp" "-r" "src/llvm-project/libunwind/include/mach-o/" "src/llvm-project/lld/MachO/")
                  #t))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (sanitizers-dir (string-append (assoc-ref outputs "out") "/lib/rustlib/x86_64-unknown-linux-gnu/lib/"))
                        (from-dir "build/x86_64-unknown-linux-gnu/native/sanitizers/build/lib/linux/"))
                     (mkdir-p sanitizers-dir)
                     (copy-file (string-append from-dir "libclang_rt.asan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.asan.a"))
                     (copy-file (string-append from-dir "libclang_rt.tsan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.tsan.a"))
                     (copy-file (string-append from-dir "libclang_rt.msan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.msan.a"))
                     (copy-file (string-append from-dir "libclang_rt.lsan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.lsan.a")))
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'tools' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "tools"))))
                 (invoke "./x.py" "install" "clippy")
                 (invoke "./x.py" "install" "rust-analyzer")
                 (invoke "./x.py" "install" "rustfmt")
                 ;; (delete-file (string-append (assoc-ref outputs "out") "/lib/rustlib/src/rust/Cargo.lock"))
                 (for-each delete-file-recursively
                          '("src/llvm-project"))))
                            ;; "vendor/tikv-jemalloc-sys/jemalloc"))))
             (delete 'patch-cargo-checksums)
             (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
               ;; Generate checksums after patching generated files (in
               ;; particular, vendor/jemalloc/rep/Makefile).
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* (cons* "Cargo.lock"
                                     "src/bootstrap/Cargo.lock"
                                     "library/Cargo.lock"
                                     "compiler/rustc_codegen_gcc/Cargo.lock"
                                     "compiler/rustc_codegen_cranelift/Cargo.lock"
                                     (find-files "src/tools" "Cargo.lock"))
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor")
                 (generate-all-checksums "compiler")
                 (generate-all-checksums "src/tools")
                 #t))
            (delete 'delete-install-logs)
            (add-after 'configure 'switch-to-nightly
              (lambda _
                (substitute* "config.toml"
                             (("channel = \"stable\"") "channel = \"nightly\"")))))))))))

rust-nightly
