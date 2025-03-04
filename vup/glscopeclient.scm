(define-module (vup glscopeclient)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (vup misc)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix licenses:))

(define-public ffts
  (package
    (name "ffts")
    (version "0.0-1.b22d839")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anthonix/ffts")
             (commit "b22d839b61c5bd8a993e24ae3e01f39d9b5084fb")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14y83n86xgzgg2g02sif3lph0n3lss74cf5prwl1rwxys2wcw2h0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_SHARED=ON")
       #:tests? #f))
    (home-page "https://anthonix.com/ffts")
    (synopsis "The Fastest Fourier Transform in the South")
    (description "The Fastest Fourier Transform in the South")
    (license #f)))
; whatever it is

(define-public ngscopeclient
  (package
    (name "ngscopeclient")
    (version "0.0-1.cd27439")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/ngscopeclient/scopehal-apps")
             (commit "cd2743948b9be146e53d170c906d20908e19fc5b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hcpa0x8kaqw35x8y5x20dvq1y6jzb0qmfgk4ly8jkk1wvinzw6a"))))
    (build-system cmake-build-system)
    (inputs (list ffts
                  yaml-cpp
                  glew
                  gtkmm-3
                  catch2
                  vulkan-headers-upstream
                  vulkan-loader
                  shaderc-upstream
                  vulkan-hpp
                  glslang-upstream
                  glfw
                  spirv-tools-upstream
                  hidapi))
    (native-inputs (list pkg-config git))
    (arguments
     `(#:tests? #f ;needs working vulkan device
       #:configure-flags '("-DCMAKE_BUILD_TYPE=RELEASE")
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
       #:modules ((guix build utils)
                  (guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system)
                   #:prefix glib-or-gtk:))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-glslang-include-share-folder
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "lib/scopehal/scopehal.cpp"
                        (("\"/share/")
                         "\"/../share/"))
                      (substitute* "lib/scopehal/CMakeLists.txt"
                        (("/usr/include/glslang/Include/")
                         (string-append (assoc-ref inputs "glslang")
                                        "/include/glslang/Include/"))) #t))
                  (add-after 'install 'glib-or-gtk-wrap
                    (assoc-ref glib-or-gtk:%standard-phases
                               'glib-or-gtk-wrap))
                  (add-after 'glib-or-gtk-wrap 'glib-or-gtk-compile-schemas
                    (assoc-ref glib-or-gtk:%standard-phases
                               'glib-or-gtk-compile-schemas)))))
    (home-page "https://github.com/ngscopeclient/scopehal-apps")
    (synopsis "ngscopeclient and other client applications for libscopehal")
    (description "ngscopeclient and other client applications for libscopehal")
    (license licenses:bsd-3)))
; whatever it is
