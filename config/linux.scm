(define-module (config linux)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module ((gnu packages linux) #:prefix guix:)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages llvm))

(define linux-common
  '("CONFIG_UPROBE_EVENTS=y"
    "CONFIG_KPROBE_EVENTS=y"
    ;; kheaders module also helpful for tracing
    "CONFIG_IKHEADERS=y"
    "CONFIG_BPF=y"
    "CONFIG_BPF_SYSCALL=y"
    "CONFIG_BPF_JIT_ALWAYS_ON=y"
    ;; optional, for tc filters
    "CONFIG_NET_CLS_BPF=m"
    ;; optional, for tc actions
    "CONFIG_NET_ACT_BPF=m"
    "CONFIG_BPF_JIT=y"
    ;; for Linux kernel versions 4.1 through 4.6
    ;; ("CONFIG_HAVE_BPF_JIT" . y)
    ;; for Linux kernel versions 4.7 and later
    "CONFIG_HAVE_EBPF_JIT=y"
    ;; optional, for kprobes
    "CONFIG_BPF_EVENTS=y"
    ;; configs module
    "CONFIG_IKCONFIG=y"
    "CONFIG_IKCONFIG_PROC=y"
    ;; MGLRU
    "CONFIG_LRU_GEN_ENABLED=y"
    ;; more interactive scheduling
    ;; "CONFIG_SCHED_BORE=y"
    ;; lto
    ;; "CONFIG_LTO_NONE=y"
    ))


(define linux-x86
  `(,@linux-common
    ;; I915 low level tracing
    "CONFIG_DRM_I915_LOW_LEVEL_TRACEPOINTS=y"
    "CONFIG_X86_CPU_RESCTRL=y"
    "CONFIG_PERF_EVENTS_AMD_UNCORE=y"

    ;; "CONFIG_X86_AMD_PSTATE=y"
    "CONFIG_DEVICE_PRIVATE=y"
    "CONFIG_HSA_AMD=y"
    "CONFIG_HSA_AMD_SVM=y"))

(define linux-arm
  linux-common)


(define* (make-vup-linux config)
 (let ((base
        (guix:customize-linux
         #:linux linux-nonfree-stable
         #:extra-version "vup"
         #:configs config)))
     (package
       (inherit base)
       (inputs `(("cpio" ,cpio) ,@(package-inputs base)))
       (native-inputs (modify-inputs (package-native-inputs base)
                        ;; (prepend clang-17)
                        ;; (prepend lld-17)
                        (prepend python)))
       ;; (arguments
       ;;  (substitute-keyword-arguments
       ;;    (package-arguments base)
       ;;    ((#:phases phases)
       ;;     #~(modify-phases #$phases
       ;;         (add-after 'unpack 'configure-llvm
       ;;           (lambda* _
       ;;             (setenv "LLVM" "1")
       ;;             (setenv "CC" "clang")))))))
       (source
        (origin
          (inherit (package-source base))
          (patches
           (append
            (origin-patches (package-source base))
            (list
             (origin
              (method url-fetch)
              (uri "https://raw.githubusercontent.com/bkerler/mtkclient/30b53348cc4ae73b88904bc96c33edff1f05e722/mtkclient/Setup/Linux/kernelpatches/disable-usb-checks-5.10.patch")
              (sha256 "12xsfnckpqzgcnxmd1xgpiibgxasl6k7ina0ckj3cn1dgy4g2lss"))
             ;; "0001-cachyos-base-all.patch"
             ;; (origin
             ;;  (method url-fetch)
             ;;  (uri "https://raw.githubusercontent.com/CachyOS/kernel-patches/7ae55e8c51f2e84c6c9429ed547f43af97f34754/6.10/all/0001-cachyos-base-all.patch")
             ;;  (sha256 "05kvwy06r992x6jjp54ir5y7gfx31pbrjdrmqpxfgfq88jy81qlg"))
             ;; (origin
             ;;  (method url-fetch)
             ;;  (uri "https://raw.githubusercontent.com/CachyOS/kernel-patches/7ae55e8c51f2e84c6c9429ed547f43af97f34754/6.10/sched/0001-sched-ext.patch")
             ;;  (sha256 "04wzj6v0lfh41jk10mjrmjmf39pnhjflnr1abks9x7g0vrglry5m"))
             ;; (origin
             ;;  (method url-fetch)
             ;;  (uri "https://raw.githubusercontent.com/CachyOS/kernel-patches/7ae55e8c51f2e84c6c9429ed547f43af97f34754/6.10/sched/0001-bore-cachy-ext.patch")
             ;;  (sha256 "1ag84h7b5m685jjl4n21h61wy8x1vqhfxivis882cfgq256gd8w2"))
             ;; "thinlto-cachedir.patch"
             ))))))))

(define (linux-nonfree-urls version)
  "Return a list of URLs for Linux-Nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/"
         "v" (version-prefix version 1) ".x/"
         "linux-" version ".tar.xz")))

(define linux-nonfree-stable
  (let* ((version "6.12.16"))
    (package
      (inherit guix:linux-libre)
      (name "linux-nonfree-stable")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (linux-nonfree-urls version))
                (sha256
                 (base32
                  "1i3xkprqsd3yqbai1pbgrszcg6ycy5rwpblzzw5m4lagd4m3d0az"))))
      (synopsis "Mainline Linux kernel, nonfree binary blobs included.")
      (description "Linux is a kernel.")
      (license license:gpl2)
      (home-page "https://kernel.org/"))))

(define linux-firmware-version "20250211")
(define (linux-firmware-source version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                              "/git/firmware/linux-firmware.git"))
          (commit version)))
    (file-name (git-file-name "linux-firmware" (string-take version 8)))
    (sha256
     (base32
      "090wx40ybgl1cv6mamsmnr2i8a17skwf1pqq5i4wpx7w7qrw4ib4"))))

(define-public linux-firmware-nonfree
  (package
    (name "linux-firmware-nonfree")
    (version linux-firmware-version)
    (source (linux-firmware-source version))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware/")))
                     (mkdir-p fw-dir)
                     (copy-recursively source fw-dir)
                     #t))))
    (home-page "")
    (synopsis "Non-free firmware for Linux")
    (description "Non-free firmware for Linux")
    ;; FIXME: What license?
    (license (license:non-copyleft "https://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.radeon_firmware;hb=HEAD"))))

(define-public perf-nonfree
  (package
    (inherit guix:perf)
    (name "perf-nonfree")
    (version (package-version linux-nonfree-stable))
    (source (package-source linux-nonfree-stable))
    (license (package-license linux-nonfree-stable))))

(define-public cpupower-nonfree
  (package
    (inherit guix:cpupower)
    (name "cpupower-nonfree")
    (version (package-version linux-nonfree-stable))
    (source (package-source linux-nonfree-stable))
    (license (package-license linux-nonfree-stable))
    (arguments (substitute-keyword-arguments
     (package-arguments guix:cpupower)
     ((#:make-flags flags)
      #~(append #$flags (list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))))))))

(define-public linux-nonfree/extra_config-x86
  (make-vup-linux linux-x86))

;; (define-public linux-nonfree/extra_config-arm
;;   (make-vup-linux linux-arm))
