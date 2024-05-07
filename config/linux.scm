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
    "CONFIG_SCHED_BORE=y"
    ;; lto
    "CONFIG_LTO_CLANG_THIN=y"
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
         #:linux linux-nonfree
         #:extra-version "vup"
         #:configs config)))
     (package
       (inherit base)
       (inputs `(("cpio" ,cpio) ,@(package-inputs base)))
       (native-inputs (modify-inputs (package-native-inputs base)
                        (prepend clang-17)
                        (prepend lld-17)
                        (prepend python)))
       (arguments
        (substitute-keyword-arguments
          (package-arguments base)
          ((#:phases phases)
           #~(modify-phases #$phases
               (add-after 'unpack 'configure-llvm
                 (lambda* _
                   (setenv "LLVM" "1")
                   (setenv "CC" "clang")))))))
       (source
        (origin
          (inherit (package-source base))
          (patches
           (append
            (origin-patches (package-source base))
            (list
             "thinlto-cachedir.patch"
             (origin
              (method url-fetch)
              (uri "https://raw.githubusercontent.com/bkerler/mtkclient/main/Setup/Linux/kernelpatches/disable-usb-checks-5.10.patch")
              (sha256 "12xsfnckpqzgcnxmd1xgpiibgxasl6k7ina0ckj3cn1dgy4g2lss"))
             (origin
              (method url-fetch)
              (uri "https://raw.githubusercontent.com/cachyos/kernel-patches/fefa20cb087b5047c37c69e40eb4829c9aa91bb3/6.8/all/0001-cachyos-base-all.patch")
              (sha256 "07i2c6mar86xfm946c6l61d7drf36bdvsc55fxn3zdpnn5map0ny"))
             (origin
              (method url-fetch)
              (uri "https://raw.githubusercontent.com/cachyos/kernel-patches/fefa20cb087b5047c37c69e40eb4829c9aa91bb3/6.8/sched/0001-sched-ext.patch")
              (sha256 "16rbk11s85rvnyasn1q51665ak2wj3v3hihmird50yds5d524xw2"))
             (origin
              (method url-fetch)
              (uri "https://raw.githubusercontent.com/cachyos/kernel-patches/fefa20cb087b5047c37c69e40eb4829c9aa91bb3/6.8/sched/0001-bore-cachy-ext.patch")
              (sha256 "157vrxsv90a8d7yrf1jjcd89i49h5lb5n34kmxlvdqs8kq4zmk6r"))
             ))))))))

(define-public linux-nonfree/extra_config-x86
  (make-vup-linux linux-x86))

(define-public linux-nonfree/extra_config-arm
  (make-vup-linux linux-arm))
