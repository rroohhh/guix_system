(define-module (config linux)
  #:use-module (vup linux)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages cpio))

(define (config->string options)
  (string-join
   (map
    (match-lambda
      ((option . 'm)
       (string-append option "=m"))
      ((option . #t)
       (string-append option "=y"))
      ((option . #f)
       (string-append option "=n")))
    options)
   "\n"))

(define* (make-linux* base
                      #:key
                      (extra-version #f)
                      (extra-options %default-extra-linux-options))
  (package
    (inherit base)
    (name (if extra-version
              (string-append (package-name base) "-" extra-version)
              (package-name base)))
    (arguments
     (substitute-keyword-arguments (package-arguments base)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'configure 'add-extra-options
              (lambda* (#:key inputs native-inputs target #:allow-other-keys)
                (setenv "EXTRA_VERSION" #$extra-version)

                (let ((port (open-file ".config" "a"))
                      (extra-configuration #$(config->string extra-options)))
                  (display extra-configuration port)
                  (close-port port))

                (invoke "make" "oldconfig")))))))))



(define-public linux-nonfree/extra_config
  (let ((base
         (make-linux* linux-nonfree
               #:extra-version "vup"
               #:extra-options `(;; Needed for probes
                                 ("CONFIG_UPROBE_EVENTS" . #t)
                                 ("CONFIG_KPROBE_EVENTS" . #t)
                                 ;; kheaders module also helpful for tracing
                                 ("CONFIG_IKHEADERS" . #t)
                                 ("CONFIG_BPF" . #t)
                                 ("CONFIG_BPF_SYSCALL" . #t)
                                 ("CONFIG_BPF_JIT_ALWAYS_ON" . #t)
                                 ;; optional, for tc filters
                                 ("CONFIG_NET_CLS_BPF" . m)
                                 ;; optional, for tc actions
                                 ("CONFIG_NET_ACT_BPF" . m)
                                 ("CONFIG_BPF_JIT" . #t)
                                 ;; for Linux kernel versions 4.1 through 4.6
                                 ;; ("CONFIG_HAVE_BPF_JIT" . y)
                                 ;; for Linux kernel versions 4.7 and later
                                 ("CONFIG_HAVE_EBPF_JIT" . #t)
                                 ;; optional, for kprobes
                                 ("CONFIG_BPF_EVENTS" . #t)
                                 ;; kheaders module
                                 ("CONFIG_IKHEADERS" . #t)
                                 ;; configs module
                                 ("CONFIG_IKCONFIG" . #t)
                                 ("CONFIG_IKCONFIG_PROC" . #t)
                                 ;; I915 low level tracing
                                 ("CONFIG_DRM_I915_LOW_LEVEL_TRACEPOINTS" . #t)))))
    (package
      (inherit base)
      (inputs `(("cpio" ,cpio) ,@(package-inputs base)))
      (source
       (origin
         (inherit (package-source base))
         (patches
          (append
              (origin-patches (package-source base))
              (list (origin
                     (method url-fetch)
                     (uri "https://raw.githubusercontent.com/bkerler/mtkclient/main/Setup/Linux/kernelpatches/disable-usb-checks-5.10.patch")
                     (sha256 "12xsfnckpqzgcnxmd1xgpiibgxasl6k7ina0ckj3cn1dgy4g2lss"))))))))))
