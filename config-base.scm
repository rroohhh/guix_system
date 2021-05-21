(define-module (config-base))

(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm

(use-modules (vup patches)) ; really bad hacks, but who cares

(use-modules (gnu) (gnu system nss))

(use-modules (guix packages))
(use-package-modules ssh vim certs cpio)
(use-modules (guix utils))
(use-modules (guix records))

(use-modules (gnu services))
(use-service-modules dbus desktop xorg avahi networking ssh sysctl)

(use-modules (vup linux))
(use-modules (vup hwinfo))

(use-modules (srfi srfi-1))
(use-modules (ice-9 match))

(define (config->string options)
  (string-join (map (match-lambda
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
        `(modify-phases ,phases
           (add-after 'configure 'add-extra-options
             (lambda* (#:key inputs native-inputs target #:allow-other-keys)
               (setenv "EXTRA_VERSION" ,extra-version)

               (let ((port (open-file ".config" "a"))
                     (extra-configuration ,(config->string extra-options)))
                 (display extra-configuration port)
                 (close-port port))

               (invoke "make" "oldconfig")))))))))

(define-public linux-nonfree/extra_config
  (let ((base
         (make-linux* linux-nonfree
               #:extra-version "vup"
               #:extra-options `( ;; Needed for probes
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
      (inputs `(("cpio" ,cpio) ,@(package-inputs base))))))


(define-public base-system-config
  (operating-system
    (host-name "placeholder")
    (timezone "Europe/Berlin")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "de" "vup"))
    (initrd-modules (append '("virtio_crypto" "virtio-gpu" "virtio_pmem" "nd_virtio"
                              "virtio_scsi" "virtio_rpmsg_bus" "virtio_mem"
                              "virtio_dma_buf" "virtio_mmio" "virtio"
                              "virtio_ring" "virtio_input" "virtio_vdpa"
                              "caif_virtio" "vmw_vsock_virtio_transport"
                              "vmw_vsock_virtio_transport_common" "9pnet_virtio") %base-initrd-modules))


    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")
                 (keyboard-layout keyboard-layout)))

    (setuid-programs (append (list
                              (file-append hwinfo "/bin/hwinfo"))
                             %setuid-programs))

    (kernel linux-nonfree/extra_config)
    (firmware (append (list linux-firmware-nonfree) %base-firmware))
    (file-systems '())

    (packages
     (append
      (list
       openssh vim
       nss-certs) ;; for HTTPS access
      %base-packages))

    (name-service-switch %mdns-host-lookup-nss)))

(define-public ssh-default-authorized-keys `(("robin" ,(local-file "robin.pub"))
					     ("root"  ,(local-file "robin.pub"))))

(define-public base-services
  `(,(service polkit-service-type)
    ,(elogind-service)
    ,(dbus-service)
    ,(accountsservice-service)
    ,(service localed-service-type)

    ,(service avahi-service-type)
    ,(service ntp-service-type)
    ,(service openssh-service-type
              (openssh-configuration
               (x11-forwarding? #t)
               (authorized-keys ssh-default-authorized-keys)
	           (extra-content "PermitUserEnvironment yes")))
    ,(service guix-publish-service-type
              (guix-publish-configuration
               (host "0.0.0.0")
               (advertise? #t)))
    ,@(modify-services %base-services
        (guix-service-type config =>
			    (guix-configuration
			      (inherit config)
			      (discover? #t)))
        (sysctl-service-type config =>
                       (sysctl-configuration
                         (settings (append '(("kernel.dmesg_restrict" . "0"))
                                           %default-sysctl-settings)))))))
