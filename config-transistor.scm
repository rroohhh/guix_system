(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm

(use-modules (my-tlp))
(use-modules (util))
(use-modules (config-common))

(use-modules (guix gexp))
(use-modules (guix packages))
(use-modules (guix records))
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
(use-modules (gnu services base))
(use-modules (gnu services linux))
(use-modules (gnu services networking))
(use-modules (gnu services dbus))
(use-modules ((gnu services desktop) #:prefix guix:))
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



(define (bool value)
  (if value "true\n" "false\n"))

(define-record-type* <bluetooth-configuration>
  bluetooth-configuration make-bluetooth-configuration
  bluetooth-configuration?
  (bluez bluetooth-configuration-bluez (default bluez))
  (auto-enable? bluetooth-configuration-auto-enable? (default #f)))

(define (bluetooth-configuration-file config)
  "Return a configuration file for the systemd bluetooth service, as a string."
  (string-append
   "[General]\n"
   "Enable=Source\n"
   "\n"
   "[Policy]\n"
   "AutoEnable=" (bool (bluetooth-configuration-auto-enable?
                        config))))

(define (bluetooth-directory config)
  (computed-file "etc-bluetooth"
                 #~(begin
                     (mkdir #$output)
                     (chdir #$output)
                     (call-with-output-file "main.conf"
                       (lambda (port)
                         (display #$(bluetooth-configuration-file config)
                                  port))))))

(define (bluetooth-shepherd-service config)
  "Return a shepherd service for @command{bluetoothd}."
  (shepherd-service
   (provision '(bluetooth))
   (requirement '(dbus-system udev))
   (documentation "Run the bluetoothd daemon.")
   (start #~(make-forkexec-constructor
             (list #$(file-append (bluetooth-configuration-bluez config)
                                  "/libexec/bluetooth/bluetoothd"))))
   (stop #~(make-kill-destructor))))


(define %hrdj-device-config
  `("modprobe.d/hrdj.conf"
    ,(plain-file "hrdj.conf"
                 "blacklist snd-usb-audio")))

;; (define (zram-device-udev-rule config)
;;   (file->udev-rule "99-zram.rules"
;;                    (plain-file "99-zram.rules"
;;                                (zram-device-configuration->udev-string config))))

(define hrdj-device-service-type
  (service-type
    (name 'hrdj)
    (default-value '())
    (extensions
      (list (service-extension etc-service-type
                               (const (list %hrdj-device-config)))))))

(define bluetooth-service-type
  (service-type
   (name 'bluetooth)
   (extensions
    (list (service-extension dbus-root-service-type
                             (compose list bluetooth-configuration-bluez))
          (service-extension udev-service-type
                             (compose list bluetooth-configuration-bluez))
          (service-extension etc-service-type
                             (lambda (config)
                               `(("bluetooth"
                                  ,(bluetooth-directory config)))))
          (service-extension shepherd-root-service-type
                             (compose list bluetooth-shepherd-service))))
   (default-value (bluetooth-configuration))
   (description "Run the @command{bluetoothd} daemon, which manages all the
Bluetooth devices and provides a number of D-Bus interfaces.")))

(define* (bluetooth-service #:key (bluez bluez) (auto-enable? #f))
  "Return a service that runs the @command{bluetoothd} daemon, which manages
all the Bluetooth devices and provides a number of D-Bus interfaces.  When
AUTO-ENABLE? is true, the bluetooth controller is powered automatically at
boot.

Users need to be in the @code{lp} group to access the D-Bus service.
"
  (service bluetooth-service-type
           (bluetooth-configuration
            (bluez bluez)
            (auto-enable? auto-enable?))))


(define %hosts-file
  (plain-file "hosts"
              (read-file-as-string "etc-hosts")))

(operating-system
  (inherit common-system-config)
  (host-name "transistor")
  (hosts-file %hosts-file)

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
       (mount-point "/tmp")
       (device "none")
       (title 'device)
       (type "tmpfs"))
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

     (service hrdj-device-service-type)

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
               (size "32G")
               (compression-algorithm 'lz4)))

     (service network-manager-service-type
              (network-manager-configuration
               (dns "dnsmasq")
               (vpn-plugins (list network-manager-openvpn network-manager-openconnect network-manager-vpnc))))
     (service wpa-supplicant-service-type)
    ; (service ofono-service-type)
     ))))
