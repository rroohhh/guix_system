(define-module (vup vkdt)
  #:use-module (vup rust-import-lock)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages image)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages video)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix base32)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public vkdt
  (package
    (name "vkdt")
    (version "0.0-1.5b509af")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hanatos/obs-flat-vkdt")
             (commit "5b509af804e611e156f3e0fe49b51475b4c0aad0")
             ;; rawspeed and (custom) imgui bundled
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19bzd7zv1i58j38l7gqxqj0hxgaikx94j6a4pkln6g9vhv45x7pi"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:imported-modules `((guix build json)
                           (guix build cargo-build-system)
                           ,@%default-gnu-imported-modules)
      #:modules `((guix build json)
                  (guix build utils)
                  (guix build gnu-build-system)
                  ((guix build cargo-build-system)
                   #:prefix cargo:))
      #:make-flags
      #~(list (string-append "DESTDIR="
                             #$output)
              ;; The Makefile uses a lowercase "prefix"
              (string-append "prefix="))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-rawloader
            (lambda* (#:key inputs system target #:allow-other-keys)
              (chdir "src/pipe/modules/i-raw/rawloader-c")
              (substitute* "Cargo.toml"
                (("rawler = .*")
                 "rawler = { version = \"0.6.3\" }\n"))
              ((assoc-ref cargo:%standard-phases 'configure) #:inputs inputs #:system system #:target target)
              (rename-file "guix-vendor/rawler-0.6.3.tar.gz" "guix-vendor/rawler-0.6.3.tar.gz2")
              (rename-file "guix-vendor/rawler-0.6.3.tar.gz2/rawler" "guix-vendor/rawler-0.6.3.tar.gz")
              (delete-file-recursively "guix-vendor/rawler-0.6.3.tar.gz2")
              ((assoc-ref cargo:%standard-phases 'patch-cargo-checksums) #:inputs inputs #:system system #:target target)
              (chdir "../../../../../")))
          ;; Guix's vulkan doesn't have pkg-config files.
          (add-after 'unpack 'lvulkan
            (lambda _
              (substitute* "src/qvk/flat.mk"
                (("\\$\\(shell pkg-config --libs vulkan\\)")
                 "-lvulkan"))))
          ;; no configure
          (delete 'configure)
          ;; The Makefile for building is in a different directory.
          (add-before 'build 'build-chdir
            (lambda* _
              (chdir "bin")))
          (add-before 'install 'install-chdir
            (lambda* _
              (chdir "..")))
          (add-before 'install 'mkdir-share
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append (assoc-ref outputs "out") "/share/icons"))
              (mkdir-p (string-append (assoc-ref outputs "out") "/share/applications"))
              )))))
    (inputs
     (append
      (import-rust-lock
       (file-append source "/src/pipe/modules/i-raw/rawloader-c")
       `(("rawler" unquote
          (nix-base32-string->bytevector
           "1526p8mjpl0n0xbl2g5dvs2s3nfnj0jnmfzrv797iv0g96gkz235"))))
      (list glfw
                  glslang
                  libjpeg-turbo
                  libomp
                  libvorbis
                  libxml2
                  alsa-lib
                  ffmpeg
                  rust
                  (list rust "cargo")
                  pugixml
                  rsync ;for install
                  vulkan-headers
                  vulkan-loader
                  zlib
                  xxd)))
    (native-inputs (list clang cmake pkg-config))
    (synopsis "Darktable which sucks less")
    (description
     "this is an experimental complete rewrite of darktable, at this point with a reduced
feature set. vkdt is designed with high performance in mind. there are already some new
features, too: support for animations, raw video, and heavy lifting algorithms like image
alignment and better highlight inpainting. this is made possible by faster processing,
allowing more complex operations.

the processing pipeline has been rewritten as a generic node graph (DAG) which supports
multiple inputs and multiple outputs. all processing is done in glsl shaders/vulkan. this
facilitates potentially heavy duty computational photography tasks, for instance aligning
multiple raw files and merging them before further processing, as well as outputting
intermediate results for debugging. the gui profits from this scheme as well and can
display textures while they are still on GPU and output the data to multiple targets, such
as the main view and histograms.")
    (home-page "https://jo.dreggn.org/vkdt/")
    (license license:bsd-2)))

vkdt
