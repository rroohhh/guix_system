(add-to-load-path (dirname (current-filename)))

(use-modules (my-tlp))
(use-modules (util))
(use-modules (config-common))
(use-modules (bluetooth))
(use-modules (zfs))

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
(use-modules (gnu packages file-systems))
(use-modules (gnu packages pkg-config))
(use-modules (gnu packages python))
(use-modules (gnu packages tls))

(use-modules (gnu services))
(use-modules (gnu services shepherd))
(use-modules (gnu services base))
(use-modules (gnu services admin))
(use-modules (gnu services linux))
(use-modules (gnu services networking))
(use-modules (gnu services dbus))
(use-modules ((gnu services desktop) #:prefix guix:))
(use-modules (gnu services mcron))

(use-modules (gnu system))
(use-modules (gnu system mapped-devices))
(use-modules (gnu system uuid))
(use-modules (gnu system file-systems))

(use-modules (vup misc))
(use-modules (vup hwinfo))

(define %hrdj-device-config
  `("modprobe.d/hrdj.conf"
    ,(plain-file "hrdj.conf"
                 "blacklist snd-usb-audio")))

(define hrdj-device-service-type
  (service-type
    (name 'hrdj)
    (default-value '())
    (extensions
      (list (service-extension etc-service-type
                               (const (list %hrdj-device-config)))))))

(define %hosts-file
  (plain-file "hosts"
              (read-file-as-string "etc-hosts")))


(define zfs-with-my-kernel
  (package
    (inherit zfs)
    (arguments (cons* #:linux linux-nonfree/extra_config
                      (package-arguments zfs)))))

(operating-system
  (inherit common-system-config)
  (host-name "ada")
  (hosts-file %hosts-file)

  ;; almost always swap, zram is nice
  (kernel-arguments '("modprobe.blacklist=pcspkr" "mitigations=off" "vm.swappiness=200"))
  (kernel-loadable-modules (append (list v4l2loopback-linux-module (list zfs-with-my-kernel "module")) (operating-system-kernel-loadable-modules common-system-config)))

  (setuid-programs (append (list
	(file-append hwinfo/amd "/bin/hwinfo")) %setuid-programs))

  (file-systems
   (append
    (list
     (file-system
       (mount-point "/tmp")
       (device "none")
       (title 'device)
       (type "tmpfs"))
     (file-system
       (device (uuid "67792584-3957-400f-a732-71afcdb23eb1"))
       (mount-point "/")
       (type "ext4")
       (options "acl"))
     (file-system
       (device (uuid "0773-D529" 'fat))
       (mount-point "/boot/efi")
       (type "vfat")))
    %base-file-systems))

  (services
   (append
    common-services
    (list
     (service zfs-service-type
      (zfs-configuration
       (kernel linux-nonfree/extra_config)
       (auto-scrub #f)
       (auto-snapshot? #f)))

     (bluetooth-service)

     (service hrdj-device-service-type)

     (service tlp-service-type
              (tlp-configuration
               (start-charge-thresh-bat0 75)
               (start-charge-thresh-bat1 75)
               (stop-charge-thresh-bat0 80)
               (stop-charge-thresh-bat1 80)))

     (simple-service 'rotate-mcron-log
                     rottlog-service-type
                     (list (log-rotation (files '("/var/log/mcron.log")))))

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
     (service wpa-supplicant-service-type)))))
     
