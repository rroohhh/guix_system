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
  #:use-module (misc ftdi)
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
  #:use-module (guix transformations)
  #:use-module (home hy3)
  #:use-module (vup hwinfo)
  #:use-module (vup linux))

(define extra-telegraf-config
  #~(string-append
     "[[inputs.execd]]
  command = [\"" #$(file-append zfs-with-vup-kernel "/libexec/zfs/zpool_influxdb") "\", \"-e\"]
  signal = \"STDIN\"

"))

(define ada-transform
  ;; The package transformation procedure.
  (options->transformation
   '((tune . "zenv2"))))

(define-public ada-system-config
  ;; (ada-transform
   (operating-system
      (inherit base-desktop-system-config)
      (host-name "ada")

                    ;; fuck mitigations, who cares
      (kernel-arguments '("modprobe.blacklist=pcspkr" "tsc=reliable" "mitigations=off" "noibrs" "noibpb" "nopti" "nospectre_v2" "nospectre_v1" "l1tf=off" "nospec_store_bypass_disable" "no_stf_barrier" "mds=off" "tsx=on" "tsx_async_abort=off"))

      (setuid-programs (append (list
                                (file-like->setuid-program (file-append hwinfo/amd "/bin/hwinfo"))
                                (file-like->setuid-program (file-append swaylock "/bin/swaylock"))
                                (file-like->setuid-program (file-append fuse "/bin/fusermount3")))
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
                                (list trackpoint-udev-config titan-key-udev-config ftdi-udev-config))))))

        (networking-for host-name)
        (list
         (service zfs-service-type
             (zfs-configuration
                  (kernel linux-nonfree/extra_config-x86)))

         (simple-service 'installing-module linux-loadable-module-service-type
            (append
             (list ; v4l2loopback-linux-module TODO: fix this with LLVM=1
              (list zfs-with-vup-kernel "module"))
             (operating-system-kernel-loadable-modules base-desktop-system-config)))

         (service bluetooth-service-type)

         (service telegraf-service-type
                  (telegraf-configuration
                   (influxdb-address (string-append "http://" (address-of "mel" host-name) ":8086"))
                   (influxdb-token-file "/data/projects/guix_system/data/secrets/ada_telegraf_token") ; TODO(robin): use vault??
                   (influxdb-bucket "monitoring")
                   (influxdb-orga "infra")
                   (config (list
                            %telegraf-default-config
                            extra-telegraf-config))))

         (service hrdj-device-service-type)

         (service tlp-service-type
                  (tlp-configuration
                   (start-charge-thresh-bat0 75)
                   (start-charge-thresh-bat1 75)
                   (stop-charge-thresh-bat0 80)
                   (stop-charge-thresh-bat1 80)))

         (service greetd-service-type
           (greetd-configuration
             (greeter-supplementary-groups '("input" "video"))
             (terminals
              (list
               (greetd-terminal-configuration (terminal-vt "2") (terminal-switch #t))
               (greetd-terminal-configuration
                (terminal-vt "1")
                (terminal-switch #t)
                (default-session-command
                  (greetd-agreety-session
                   (command (file-append sway "/bin/sway"))
                   (command-args '()))))
               (greetd-terminal-configuration
                (terminal-vt "3")
                (terminal-switch #t)
                (default-session-command
                  (greetd-agreety-session
                   (command (file-append hyprland-0.46 "/bin/Hyprland"))
                   (command-args '()))))))))
;;                (greetd-terminal-configuration
;;                  (terminal-vt "1")
;;                  (terminal-switch #t)
;;                  (default-session-command
;;                   (greetd-wlgreet-sway-session
;;                    (sway-configuration
;;                     (plain-file "sway-kb-layout-conf" "input * {
;;   natural_scroll enabled
;;   xkb_layout de
;;   xkb_variant vup
;; }
;; ")))))
;;               ))))

         (service mcron-service-type
                  (mcron-configuration
                   (jobs
                    (list
                      '(job
                        '(next-minute (range 0 60 5))
                        "for id in $(pgrep -w telegram); do echo 15000000 > /proc/$id/timerslack_ns; done"
                        #:user "root")
                     #~(job
                        '(next-minute (range 0 60 5))
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
