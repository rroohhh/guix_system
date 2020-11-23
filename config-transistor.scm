(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm

(use-modules (my-tlp))
(use-modules (config-common))

(use-modules (guix gexp))
(use-modules (guix packages))
(use-modules (guix git-download))
(use-modules (guix build-system gnu))
(use-modules ((guix licenses) #:prefix license:))

(use-modules (gnu packages mail))
(use-modules (gnu packages glib))
(use-modules (gnu packages autotools))
(use-modules (gnu packages gnome))
(use-modules (gnu packages linux))
(use-modules (gnu packages readline))
(use-modules (gnu packages documentation))
(use-modules (gnu packages pulseaudio))
(use-modules (gnu packages pkg-config))
(use-modules (gnu packages python))
(use-modules (gnu packages tls))

(use-modules (gnu services shepherd))
(use-modules (gnu services linux))
(use-modules (gnu services networking))
(use-modules (gnu services dbus))
(use-modules (gnu services desktop))
(use-modules (gnu services mcron))

(use-modules (gnu system mapped-devices))
(use-modules (gnu system uuid))
(use-modules (gnu system file-systems))

(use-modules (vup misc))

(define-public iwd
  (package
    (name "iwd")
    (version "0.21")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.kernel.org/pub/scm/network/wireless/iwd.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "001dikinsa6kshwscjbvwipavzwpqnpvx9fpshcn63gbvbhyd393"))))
    (build-system gnu-build-system)
    (inputs
     `(("dbus" ,dbus)
       ("libtool" ,libtool)
       ("ell" ,ell)
       ("readline" ,readline)))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkgconfig" ,pkg-config)
       ("python" ,python)
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
       (let ((dbus (assoc-ref %outputs "out")))
         (list "--disable-systemd-service"
               "--enable-external-ell"
               "--enable-hwsim"
               "--enable-tools"
               "--enable-wired"
               "--enable-docs"
               "--localstatedir=/var"
               (string-append "--with-dbus-datadir=" dbus "/etc/")
               (string-append "--with-dbus-busdir="
                              dbus "/share/dbus-1/system-services")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'pre-bootstrap
           (lambda _
             (substitute* "Makefile.am"
               ;; Test disabled because it needs the kernel module
               ;; 'pkcs8_key_parser' loaded.
               (("unit\\/test-eapol.*? ") "")
               ;; Don't try to 'mkdir /var'.
               (("\\$\\(MKDIR_P\\) -m 700") "true"))
             #t)))))
    (home-page "https://git.kernel.org/pub/scm/network/wireless/iwd.git/")
    (synopsis "Internet Wireless Daemon")
    (description "iwd is a wireless daemon for Linux that aims to replace WPA
Supplicant.  It optimizes resource utilization by not depending on any external
libraries and instead utilizing features provided by the Linux kernel to the
maximum extent possible.")
    (license license:lgpl2.1+)))

(define-public (iwd-shepherd-service _)
  "Return a shepherd service for iwd"
  (list (shepherd-service
         (documentation "Run iwd")
         (provision '(iwd networking))
         (requirement
          `(user-processes dbus-system loopback))
         (start #~(make-forkexec-constructor
                   (list (string-append #$iwd
                                        "/libexec/iwd"))))
         (stop #~(make-kill-destructor)))))


(define-public iwd-service-type
  (let ((iwd-package (lambda (_) (list iwd))))
    (service-type (name 'iwd)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            iwd-shepherd-service)
                                        ;                         (service-extension polkit-service-type
                                        ;                                            connman-package)
                         (service-extension dbus-root-service-type
                                            iwd-package)
                         ;; Add connman to the system profile.
                         (service-extension profile-service-type
                                            iwd-package)))
                  (default-value '())
                  (description
                   "Run @url{https://01.org/iwd,iwd}, a wpa-supplicant replacemennt."))))


(define-public (ofono-shepherd-service _)
  "Return a shepherd service for ofono"
  (list (shepherd-service
         (documentation "Run ofono")
         (provision '(ofono))
         (requirement
          `(user-processes dbus-system loopback))
         (start #~(make-forkexec-constructor
                   (list (string-append #$ofono
                                        "/sbin/ofonod") "-n")))
         (stop #~(make-kill-destructor)))))


(define-public ofono-service-type
  (let ((ofono-package (lambda (_) (list ofono))))
    (service-type (name 'ofono)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            ofono-shepherd-service)
                         (service-extension dbus-root-service-type
                                            ofono-package)))
                  (default-value '())
                  (description
                   "Run @url{https://01.org/ofono,ofono}"))))

(operating-system
  (inherit common-system-config)
  (host-name "transistor")

  ;; almost always swap, zram is nice
  (kernel-arguments '("--rootflags=compress=zstd,discard,subvolid=54188,subvol=@guix_root,acl" "mitigations=off" "vm.swappiness=200"))

  (mapped-devices
   (list (mapped-device
          (source (uuid "2888c8e3-3a67-46a9-bc83-022ed5e66c02"))
          (target "main_ssd")
          (type luks-device-mapping))))

  (file-systems
   (append
    (list
     (file-system
       (device (file-system-label "main_ssd"))
       (mount-point "/")
       (type "btrfs")
       (options "compress=zstd,discard,subvolid=54188,subvol=@guix_root,acl")
       (dependencies mapped-devices))
                                        ;             (needed-for-boot? #t))
     (file-system
       (device (file-system-label "main_ssd"))
       (mount-point "/data")
       (type "btrfs")
       (options "compress=zstd,discard,subvolid=259,subvol=@robin,acl")
       (dependencies mapped-devices))
     (file-system
       (device (uuid "41AD-B3AB" 'fat))
       (mount-point "/boot/efi")
       (type "vfat")))
    %base-file-systems))

  (services
   (append
    common-services
    (list
     (bluetooth-service)

     (service tlp-service-type
              (tlp-configuration
               (start-charge-thresh-bat0 75)
               (start-charge-thresh-bat1 75)
               (stop-charge-thresh-bat0 80)
               (stop-charge-thresh-bat1 80)))

     (service mcron-service-type
              (mcron-configuration
               (jobs (list #~(job '(next-minute (range 0 60 5))
                                  (string-append #$isync "/bin/mbsync -a; " #$notmuch "/bin/notmuch new")
                                  #:user "robin")))))

     (service zram-device-service-type
              (zram-device-configuration
               (size "16G")
               (compression-algorithm 'lz4)))

     (service network-manager-service-type)
     (service wpa-supplicant-service-type)
     (service ofono-service-type)))))
