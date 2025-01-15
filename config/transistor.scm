(define-module (config transistor)
  #:use-module (config desktop-base)
  #:use-module (misc trackpoint-config)
  #:use-module (services hrdj)
  #:use-module (services my-tlp)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services mcron)
  #:use-module (gnu services mail)
  #:use-module (gnu services desktop)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mail))

(define-public transistor-system-config
  (operating-system
   (inherit base-desktop-system-config)
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
       (mount-point "/tmp")
       (device "none")
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
     (modify-services base-desktop-services
                      (udev-service-type config =>
                                         (udev-configuration
                                          (inherit config)
                                          (rules
                                           (append
                                            (udev-configuration-rules config)
                                            (list trackpoint-udev-config))))))
     (list
      (bluetooth-service)

      (service hrdj-device-service-type)
      (service trackpoint-quirk-service-type)

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
      (service wpa-supplicant-service-type))))))
