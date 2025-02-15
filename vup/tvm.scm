(define-module (vup tvm)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (vup misc)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tvm
  (let* ((version "0.10.0"))
    (package
      (name "tvm")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/apache/tvm/releases/download/v"
                      version "/apache-tvm-src-v" version ".rc0.tar.gz"))
                (sha256
                 (base32
                  "1r7wn2sjxk7vzdyg63zgm9lnr901px58ckx3qwpv6dknhjzh381d"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DUSE_VULKAN=ON" "-DUSE_KHRONOS_SPIRV=ON"
                             "-DUSE_SPIRV_KHR_INTEGER_DOT_PRODUCT=ON"
                             "-DUSE_LLVM=ON" "-DUSE_LIBBACKTRACE=OFF")
         #:phases (modify-phases %standard-phases
                    (delete 'check))))
      (inputs (list vulkan-loader spirv-tools-upstream llvm-15 libffi))
      (native-inputs (list vulkan-headers spirv-headers-upstream))
      (home-page "https://tvm.apache.org/")
      (synopsis
       "An End to End Machine Learning Compiler Framework for CPUs, GPUs and accelerators")
      (description
       "An End to End Machine Learning Compiler Framework for CPUs, GPUs and accelerators")
      (license license:asl2.0))))

(define-public python-synr
  (package
    (name "python-synr")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "synr" version))
              (sha256
               (base32
                "0khyb7zbgry4x1asmsjjczc78vwm64x1awik3scf321r1jqickhb"))))
    (build-system python-build-system)
    (propagated-inputs (list python-attrs))
    (home-page "")
    (synopsis "A consistent AST for Python")
    (description "This package provides a consistent AST for Python")
    (license #f)))


(define-public python-tvm
  (package
    (inherit tvm)
    (name "python-tvm")
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'check)
                  (delete 'sanity-check) ;breaks because of homeless shelter
                  (add-after 'install 'install-configs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin/tvmc"))
                             (etc-configs (string-append out "/etc/configs")))
                        (mkdir-p etc-configs)
                        (copy-recursively "../configs" etc-configs)
                        (wrap-program bin
                                      `("TVM_CONFIGS_JSON_DIR" =
                                        (,etc-configs))))))
                  (add-after 'unpack 'chdir-and-set-env
                    (lambda* (#:key inputs #:allow-other-keys)
                      (chdir "python")
                      (setenv "TVM_LIBRARY_PATH"
                              (string-append (assoc-ref inputs "tvm") "/lib")))))))
    (inputs (list tvm git))
    (propagated-inputs (list python-numpy
                             python-tornado
                             python-decorator
                             python-attrs
                             python-wheel
                             python-synr
                             python-psutil
                             python-cloudpickle
                             python-scipy))
    (home-page "https://tvm.apache.org/")
    (synopsis
     "An End to End Machine Learning Compiler Framework for CPUs, GPUs and accelerators")
    (description
     "An End to End Machine Learning Compiler Framework for CPUs, GPUs and accelerators")
    (license license:asl2.0)))
