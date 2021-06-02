(define-module (services iwd)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (connman-service-type
			connman-configuration))

(define-public connman-with-iwd
  (package
    (name "connman")
    (version "1.37")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kernel.org/linux/network/connman/"
                            "connman-" version ".tar.xz"))
    (sha256
     (base32 "05kfjiqhqfmbbwc4snnyvi5hc4zxanac62f6gcwaf5mvn0z9pqkc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-nmcompat"
             "--enable-polkit"
             "--enable-openconnect"
             "--enable-openvpn"
             "--enable-vpnc"
             "--enable-pptp"
             "--enable-l2tp"
             "--enable-iwd"
             "--localstatedir=/var"
             (string-append
              "--with-dbusconfdir=" (assoc-ref %outputs "out") "/etc")
             (string-append
              "--with-dbusdatadir=" (assoc-ref %outputs "out") "/share"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (inputs
     `(("dbus" ,dbus)
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("iptables" ,iptables)
       ("polkit" ,polkit)        ;so connman can be used by unprivileged users
       ("readline" ,readline)
       ;; These inputs are needed for connman to include the interface to
       ;; these technologies so IF they are installed they can be used.
       ;; TODO: add neard, ofono
       ("openconnect" ,openconnect)
       ("openvpn" ,openvpn)
       ("ppp" ,ppp)
       ("vpnc" ,vpnc)
       ("wpa-supplicant" ,wpa-supplicant)))
    (home-page "https://01.org/connman")
    (synopsis "Connection management daemon")
    (description "Connman provides a daemon for managing Internet connections.
The Connection Manager is designed to be slim and to use as few resources as
possible. It is fully modular system that can be extended through plug-ins.
The plug-in approach allows for easy adaption and modification for various use
cases.  Connman implements DNS resolving and caching, DHCP clients for both
IPv4 and IPv6, link-local IPv4 address handling and tethering (IP connection
sharing) to clients via USB, ethernet, WiFi, cellular and Bluetooth.")
    (license license:gpl2)))

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
		 (provision '(iwd))
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
                   "Run @url{https://01.org/iwd,iwd},
a wpa-supplicant replacemennt."))))


(define-record-type* <connman-configuration>
  connman-configuration make-connman-configuration
  connman-configuration?
  (connman      connman-configuration-connman
                (default connman))
  (disable-vpn? connman-configuration-disable-vpn?
                (default #f))
  (use-iwd? connman-configuration-use-iwd?
                (default #f)))

(define-public (connman-activation config)
  (let ((disable-vpn? (connman-configuration-disable-vpn? config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p "/var/lib/connman/")
          (unless #$disable-vpn?
            (mkdir-p "/var/lib/connman-vpn/"))))))



(define-public (connman-shepherd-service config)
  "Return a shepherd service for Connman"
  (and
   (connman-configuration? config)
   (let* ((connman      (connman-configuration-connman config))
         (disable-vpn? (connman-configuration-disable-vpn? config))
         (use-iwd? (connman-configuration-use-iwd? config))
		 (wifi-agent (if use-iwd? 'iwd 'wpa-supplicant)))
     (list (shepherd-service
            (documentation "Run Connman")
            (provision '(networking))
            (requirement
             `(user-processes dbus-system loopback ,wifi-agent)) ; maybe add wpa-supplicant again
            (start #~(make-forkexec-constructor
                      (list (string-append #$connman
                                           "/sbin/connmand")
                            "-n" "-r"
                            #$@(if disable-vpn? '("--noplugin=vpn") '())
                            #$@(if use-iwd? '("--wifi=iwd_agent") '()))

                      ;; As connman(8) notes, when passing '-n', connman
                      ;; "directs log output to the controlling terminal in
                      ;; addition to syslog."  Redirect stdout and stderr
                      ;; to avoid spamming the console (XXX: for some reason
                      ;; redirecting to /dev/null doesn't work.)
                      #:log-file "/var/log/connman.log"))
            (stop #~(make-kill-destructor)))))))


(define connman-service-type
  (let ((connman-package (compose list connman-configuration-connman)))
    (service-type (name 'connman)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            connman-shepherd-service)
                         (service-extension polkit-service-type
                                            connman-package)
                         (service-extension dbus-root-service-type
                                            connman-package)
                         (service-extension activation-service-type
                                            connman-activation)
                         ;; Add connman to the system profile.
                         (service-extension profile-service-type
                                            connman-package)))
                  (default-value (connman-configuration))
                  (description
                   "Run @url{https://01.org/connman,Connman},
a network connection manager."))))
