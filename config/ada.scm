(define-module (config ada)
  #:use-module (config linux)
  #:use-module (config desktop-base)
  #:use-module (config network)
  #:use-module (config network-utils)
  #:use-module (services bluetooth)
  #:use-module (services influxdb)
  #:use-module (services my-tlp)
  #:use-module (services zfs)
  #:use-module (services hrdj)
  #:use-module (misc util)
  #:use-module (misc trackpoint-config)
  #:use-module (misc titan-key)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services mcron)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages wm)
  #:use-module (vup hwinfo)
  #:use-module (vup linux))

(define extra-telegraf-config
  #~(string-append
     "[[inputs.execd]]
  command = [\"" #$(file-append zfs "/libexec/zfs/zpool_influxdb") "\", \"-e\"]
  signal = \"STDIN\"

"))


(define-public ada-system-config
  (operating-system
     (inherit base-desktop-system-config)
     (host-name "ada")

     ;; fuck mitigations, who cares
     (kernel-arguments '("modprobe.blacklist=pcspkr" "tsc=reliable" "mitigations=off" "noibrs" "noibpb" "nopti" "nospectre_v2" "nospectre_v1" "l1tf=off" "nospec_store_bypass_disable" "no_stf_barrier" "mds=off" "tsx=on" "tsx_async_abort=off"))
     (kernel-loadable-modules (append
                                (list
                                 v4l2loopback-linux-module
                                 (list zfs-with-vup-kernel "module"))
                                (operating-system-kernel-loadable-modules base-desktop-system-config)))

     (setuid-programs (append (list
                               (file-like->setuid-program (file-append hwinfo/amd "/bin/hwinfo"))
                               (file-like->setuid-program (file-append swaylock "/bin/swaylock")))
                              %setuid-programs))

     (file-systems
      (append
       (list
        (file-system
          (mount-point "/tmp")
          (device "none")
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
       (modify-services base-desktop-services
                        (udev-service-type config =>
                                           (udev-configuration
                                            (inherit config)
                                            (rules
                                              (append
                                               (udev-configuration-rules config)
                                               (list trackpoint-udev-config titan-key-udev-config))))))

       (networking-for host-name)

       (list
        (service zfs-service-type
                  (zfs-configuration
                   (kernel linux-nonfree/extra_config)))

        (bluetooth-service)

        ;; (service telegraf-service-type
        ;;          (telegraf-configuration
        ;;           (influxdb-address (string-append "http://" (address-of "mel" host-name) ":8086"))
        ;;           (influxdb-token-file "/data/projects/guix_system/data/secrets/ada_telegraf_token") ; TODO(robin): use vault??
        ;;           (influxdb-bucket "monitoring")
        ;;           (influxdb-orga "infra")
        ;;           (config (list
        ;;                    %telegraf-default-config
        ;;                    extra-telegraf-config))))

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
        (service wpa-supplicant-service-type))))))
        
     
