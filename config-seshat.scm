(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm

(use-modules (config-base))

(use-modules (gnu))
(use-modules (gnu system file-systems))
(use-service-modules networking ssh)

;; initial setup:
;; boot into rescue mode (debian 10)
;; guix system  image  -t efi-raw config-seshat.scm
;; copy over the image
;; mkdir /chroot
;; mount /dev/sda2 /chroot
;; mount /dev/sda1 /chroot/boot
;; rm -r /chroot
;; partx -a image
;; mount /dev/loop1p2 /mnt
;; rsync -av /mnt/ /chroot/

(define os
  (operating-system
    (inherit base-system-config)

    (host-name "seshat")

    (kernel-arguments '("mitigations=off"))

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")
                 (keyboard-layout (operating-system-keyboard-layout base-system-config))))

    (file-systems
     (append
      (list
       (file-system
         (device (uuid "ffa1c42b-668c-41fb-b2e8-98a784e42b0f"))
         (mount-point "/")
         (type "ext4"))
       (file-system
         (device (uuid "5c83a2d6-7862-48d5-adb3-356f86553e05"))
         (mount-point "/boot")
         (type "ext4")))
      %base-file-systems))

    (services `(,(static-networking-service "eth0" "167.86.67.237"
                                            #:gateway "167.86.67.1"
                                            #:name-servers '("213.136.95.10" "213.136.95.11" "1.1.1.1" "1.0.0.1"))
                ,@(modify-services base-services
                        (openssh-service-type config =>
                          (openssh-configuration
                            (inherit config)
                            (authorized-keys (map
                              (lambda (key-config)
                                (if (string=? (car key-config) "root")
                                    (append key-config `(,(local-file "mel-robin.pub")))
                                    key-config))
                             ssh-default-authorized-keys))))
                        (guix-service-type config =>
                                           (guix-configuration
                                             (inherit config)
                                             (authorized-keys
                                              (append (list (local-file "ada-signing-key.pub"))
                                                      %default-authorized-guix-keys)))))))))
(list (machine
       (operating-system os)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "coroot.de")
                       (system "x86_64-linux")
                       (identity "/home/robin/.ssh/id_ed25519")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHDeKweSXGxPW8MQvynT2tN19M5ttMDPiGeGGg4Cbic2")))))
