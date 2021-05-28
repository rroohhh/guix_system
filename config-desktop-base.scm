(define-module (config-desktop-base))

(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm
(use-modules (config-base))
(use-modules (pulseaudio))
(use-modules (cgroupv2))

(use-modules (vup patches)) ; really bad hacks, but who cares

(use-modules (gnu) (gnu system nss))

(use-modules (guix packages))
(use-modules (guix utils))
(use-modules (guix records))
(use-package-modules bootloaders certs wm suckless xorg linux ssh emacs vim version-control mail connman polkit vpn samba admin glib autotools readline documentation pkg-config python tls android rust-apps cpio)

(use-modules (gnu services))
(use-service-modules desktop avahi dbus xorg shepherd mcron networking ssh linux nix cups sysctl)

(use-modules (vup linux))
(use-modules (vup python-xyz))
(use-modules (vup caps2esc))
(use-modules (vup hwinfo))
(use-modules (vup docker))

(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (guix download))
(use-modules (guix git-download))
(use-modules (guix build-system gnu))
(use-modules (guix build-system trivial))
(use-modules ((guix licenses) #:prefix license:))

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

(define-public base-desktop-system-config
  (operating-system
    (inherit base-system-config)
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

    (file-systems '())))

(define-public base-desktop-services
  `(,(service caps2esc-service-type)
    ,(service root-remount-service-type)
    ,(service docker-service-type
              (docker-configuration
               (docker docker)))
    ,(service cups-pk-helper-service-type)

    ,(service nix-service-type
              (nix-configuration
               (build-sandbox-items '("/bin/sh"))))

    ,(service cups-service-type
              (cups-configuration
               (web-interface? #t)))

    ,(pam-limits-service
      (list
       (pam-limits-entry "robin" 'both 'nofile 100000)
       (pam-limits-entry "@audio" 'both 'rtprio 99)
       (pam-limits-entry "@audio" 'both 'memlock 'unlimited)))
    ,(service modem-manager-service-type)
    ,@(modify-services base-services
        (udev-service-type config =>
                           (udev-configuration
                            (inherit config)
                            (rules
                             (append
                              (udev-configuration-rules config)
                              (list android-udev-rules python-openant/udev trackpoint-udev-config))))))))
