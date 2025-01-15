(define-module (config linux)
  #:use-module (vup linux)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
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

(define-public linux-nonfree/extra_config-x86
  (make-vup-linux linux-x86))

(define-public linux-nonfree/extra_config-arm
  (make-vup-linux linux-arm))
