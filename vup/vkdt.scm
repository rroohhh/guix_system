(define-module (vup vkdt)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages image)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public vkdt
  (let ((commit "c777e1c30f4236eaacf467dfd45633dd7034192a"))
    (package
      (name "vkdt")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hanatos/obs-flat-vkdt")
               (commit commit)
               ;; rawspeed and (custom) imgui bundled
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0iwy770akrqs4rn5ds3d4jw2s6q00sfd67vvcny71rylhgdng5xs"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; no tests
        #:make-flags
        #~(list
           (string-append "DESTDIR=" #$output)
           ;; The Makefile uses a lowercase "prefix"
           (string-append "prefix="))
        #:phases
        #~(modify-phases %standard-phases
            ;; Guix's vulkan doesn't have pkg-config files.
            (add-after 'unpack 'lvulkan
              (lambda _
                (substitute* "src/qvk/flat.mk"
                  (("\\$\\(shell pkg-config --libs vulkan\\)") "-lvulkan"))))
            ;; no configure
            (delete 'configure)
            ;; The Makefile for building is in a different directory.
            (add-before 'build 'build-chdir
              (lambda* _
                (chdir "bin")))
            (add-before 'install 'install-chdir
              (lambda* _
                (chdir ".."))))))
      (inputs (list glfw
                    glslang
                    libjpeg-turbo
                    libomp
                    libvorbis
                    libxml2
                    pugixml
                    rsync ; for install
                    vulkan-headers
                    vulkan-loader
                    zlib))
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
      (license license:bsd-2))))

vkdt
