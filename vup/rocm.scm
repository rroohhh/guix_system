(define-module (vup rocm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages rocm)
  #:use-module ((guix licenses) #:prefix license:))

(define %rocm-version (package-version rocr-runtime))

(define-public rocm-smi
  (package
    (name "rocm-smi")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/rocm_smi_lib")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19gbskjawf9rr594bbkzm262a1w73azxlfpn8jzpahwy9k2khnnp"))))
    (build-system cmake-build-system)
    (arguments
      `(#:phases
        (modify-phases %standard-phases
          (delete 'check))))
    (home-page "https://github.com/RadeonOpenCompute/rocm_smi_lib")
    (synopsis "ROCm System Management Interface (ROCm SMI) Library")
    (description "C library for Linux that provides a user space interface for applications to monitor and control GPU applications.")
    (license license:ncsa)))

(define-public hipify
  (package
    (name "hipify")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm-Developer-Tools/HIPIFY")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11hhv3vb9zh21cb0ds6rpgp4yq7cs8nj15b4y0wwjagwzcp5kh41"))))
    (build-system cmake-build-system)
    (inputs (list clang-toolchain))
    (arguments
      `(#:phases (modify-phases %standard-phases
         (delete 'check))))
    (home-page "https://github.com/ROCm-Developer-Tools/HIPIFY")
    (synopsis "Tools to translate CUDA source code into portable HIP C++ automatically")
    (description "hipify-clang is a clang-based tool for translating CUDA sources into HIP sources. It translates CUDA source into an abstract syntax tree, which is traversed by transformation matchers. After applying all the matchers, the output HIP source is produced.")
    (license license:expat)))

(define hip-src
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ROCm-Developer-Tools/HIP")
          (commit (string-append "rocm-" %rocm-version))))
    (file-name (git-file-name "hip-src" %rocm-version))
    (sha256
     (base32
      "0c2ic1371fnc9yfp717xhm2ib4qs40nmxyq2zmk3ddc68b9w78m4"))))

(define rocclr-src
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ROCm-Developer-Tools/ROCclr")
          (commit (string-append "rocm-" %rocm-version))))
    (file-name (git-file-name "rocclr-src" %rocm-version))
    (sha256
     (base32
      "0x1frzpz9j1s516vscbdm9g5cqirvv5w7wmq2kyljcygnci7yqar"))))

(define-public hip-amd
  (package
    (name "hip-amd")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCm-Developer-Tools/hipamd")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d6xwzsx2vjapqh8qkf6raiz16zz58r5s5niq9sl9crhmjndkr0s"))))
    (build-system cmake-build-system)
    (native-inputs (list perl))
    (propagated-inputs (list rocm-comgr rocr-runtime llvm-for-rocm numactl rocminfo mesa))
    (arguments
      `(#:configure-flags
        ,#~(list
            (string-append "-DHIP_COMMON_DIR=" #$hip-src)
            (string-append "-DAMD_OPENCL_PATH=" #$(package-source rocm-opencl-runtime))
            (string-append "-DROCM_PATH=" #$output)
            (string-append "-DHIP_PLATFORM=amd")
            (string-append "-DROCCLR_PATH=" #$rocclr-src)
            (string-append "-D__HIP_ENABLE_PCH=NO")
            (string-append "-DHIP_VERSION_BUILD_ID=0")
            )
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (runtime (assoc-ref inputs "rocr-runtime"))
                     (clang (assoc-ref inputs "llvm-for-rocm"))
                     (env-vars (list
                                 `("HIP_PATH" = (,out))
                                 `("HSA_PATH" = (,runtime))
                                 `("HCC_AMDGPU_TARGET" = ("gfx902,gfx90c"))
                                 `("HIP_CLANG_PATH" = (,(string-append clang "/bin"))))))
                (apply wrap-program `(,(string-append out "/bin/hipcc") ,@env-vars))
                (apply wrap-program `(,(string-append out "/bin/hipconfig") ,@env-vars))))))))
    (home-page "https://github.com/ROCm-Developer-Tools/HIP")
    (synopsis "HIP: C++ Heterogeneous-Compute Interface for Portability")
    (description "HIP is a C++ Runtime API and Kernel Language that allows developers to create portable applications for AMD and NVIDIA GPUs from single source code.")
    (license license:expat)))

(define-public rccl
  (package
    (name "rccl")
    (version %rocm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ROCmSoftwarePlatform/rccl")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "084lxwp8h4y7rynp0cvj8g7k08lpimwlpasa83jf57cvbilrnnss"))))
    (build-system cmake-build-system)
    (native-inputs (list rocm-cmake))
    (inputs (list googletest hip-amd rocr-runtime rocm-device-libs rocm-comgr rocm-smi))
    (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-before 'configure 'setup
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CXX" "hipcc"))))))
    (home-page "https://github.com/ROCmSoftwarePlatform/rccl")
    (synopsis "ROCm Communication Collectives Library")
    (description "RCCL (pronounced \"Rickle\") is a stand-alone library of standard collective communication routines for GPUs, implementing all-reduce, all-gather, reduce, broadcast, reduce-scatter, gather, scatter, and all-to-all.")
    (license #f))) ;;; whatever that license is
