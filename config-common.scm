(define-module (config-common))

(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm
(use-modules (pulseaudio))

(use-modules (vup patches)) ; really bad hacks, but who cares

(use-modules (gnu) (gnu system nss))

(use-modules (guix packages))
(use-modules (guix utils))
(use-modules (guix records))
(use-package-modules bootloaders certs wm suckless xorg linux ssh emacs vim version-control mail connman polkit vpn samba admin glib autotools readline documentation pkg-config python tls android rust-apps cpio)

(use-modules (gnu services))
(use-service-modules desktop avahi dbus xorg shepherd mcron docker networking ssh linux nix cups sysctl)

(use-modules (vup linux))
(use-modules (vup python-xyz))
(use-modules (vup caps2esc))
(use-modules (vup hwinfo))

(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (guix download))
(use-modules (guix git-download))
(use-modules (guix build-system gnu))
(use-modules (guix build-system trivial))
(use-modules ((guix licenses) #:prefix license:))

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
      (inputs `(("cpio" ,cpio) ,@(package-inputs base))))))

(define (root-remount-shepherd-service _)
  (list (shepherd-service
         (documentation "remount root to apply /etc/fstab settings")
         (provision '(root-remount))
         (one-shot? #t)
         (start #~(make-forkexec-constructor
                   (list (string-append #$util-linux "/bin/mount") "-o" "remount" "/")))
         (stop #~(make-kill-destructor)))))

(define-public root-remount-service-type
  (service-type (name 'root-remount)
                (description "remount root to apply /etc/fstab settings")
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     root-remount-shepherd-service)))
                (default-value '())))

(define-public trackpoint-udev-config
  (package
    (name "trackpoint-udev-config")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/lib/udev/rules.d/"))
         (with-output-to-file (string-append %output "/lib/udev/rules.d/10-trackpoint.rules")
           (lambda _
             (display "KERNEL==\"serio2\", SUBSYSTEM==\"serio\", DRIVERS==\"psmouse\", ATTR{sensitivity}:=\"255\", ATTR{speed}:=\"255\", ATTR{drift_time}:=\"0\""))))))
    (synopsis "my trackpoint configuration")
    (description "my trackpoint configuration")
    (license license:gpl3)
    (home-page #f)))

(define-public common-system-config
  (operating-system
    (host-name "placeholder")
    (timezone "Europe/Berlin")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "de" "vup"))

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")
                 (keyboard-layout keyboard-layout)))

    (setuid-programs (append (list
                              (file-append hwinfo "/bin/hwinfo"))
                             %setuid-programs))

    (kernel linux-nonfree/extra_config)
    (firmware (append (list linux-firmware-nonfree) %base-firmware))
    (groups (append %base-groups
                    (list
                     (user-group (system? #t) (name "libvirt"))
                     (user-group (system? #t) (name "adbusers"))
                     (user-group (system? #t) (name "pulse"))
                     (user-group (system? #t) (name "pulse-access")))))

    (users (append (list
                    (user-account
                     (name "robin")
                     (comment "owner")
                     (group "users")
                     (supplementary-groups '("lp" "wheel" "netdev"
                                             "audio" "video" "docker" "adbusers"
                                             "kvm" "pulse-access" "libvirt")))
                    (user-account
                     (name "pulse")
                     (group "pulse")
                     (home-directory "/var/run/pulse")
                     (create-home-directory? #f)
                     (system? #t)
                     (supplementary-groups '("audio" "lp")))) ; lp for bluetooth access
                   %base-user-accounts))

    (file-systems '())

    (packages
     (append
      (list
       openssh vim
       nss-certs) ;; for HTTPS access
      %base-packages))

    ;; Allow resolution of '.local' host names with mDNS.
    ;; no idea what this does
    (name-service-switch %mdns-host-lookup-nss)))

(define-public common-services 
  `(,(service caps2esc-service-type)
    ,(service root-remount-service-type)
    ,(service docker-service-type)
    ,(service cups-pk-helper-service-type)

    ,(service nix-service-type
              (nix-configuration
               (build-sandbox-items '("/bin/sh"))))

    ,(service cups-service-type
              (cups-configuration
               (web-interface? #t)))

    ,(service polkit-service-type)
    ,(elogind-service)
    ,(dbus-service)
    ,(accountsservice-service)
    ,(service localed-service-type)

    ,(pam-limits-service
      (list
       (pam-limits-entry "robin" 'both 'nofile 100000)
       (pam-limits-entry "@audio" 'both 'rtprio 99)
       (pam-limits-entry "@audio" 'both 'memlock 'unlimited)))
    ,(service avahi-service-type)
    ,(service modem-manager-service-type)
    ,(service ntp-service-type)
    ,(service openssh-service-type
              (openssh-configuration
               (x11-forwarding? #t)
               (permit-root-login 'without-password)
               (authorized-keys
                `(("root" ,(local-file "robin.pub"))
		  ("robin" ,(local-file "robin.pub"))))
	           (extra-content "PermitUserEnvironment yes")))
    ,@(modify-services %base-services
        (guix-service-type config =>
			    (guix-configuration
			      (inherit config)
			      (discover? #t)))
        (sysctl-service-type config =>
                       (sysctl-configuration
                         (settings (append '(("kernel.dmesg_restrict" . "0"))
                                           %default-sysctl-settings))))
        (udev-service-type config =>
                           (udev-configuration
                            (inherit config)
                            (rules
                             (append
                              (udev-configuration-rules config)
                              (list android-udev-rules python-openant/udev trackpoint-udev-config))))))))
