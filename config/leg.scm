; (define-module (config leg)
;   #:use-module (config base)
;   #:use-module (gnu)
;   #:use-module (gnu system file-systems)
;   #:use-module (gnu services networking)
;   #:use-module (gnu services ssh))
;
; ;; initial setup: ?????
;
; (define-public leg-system-config
;   (operating-system
;     (inherit aarch64-base-system-config)
;
;     (host-name "leg")
;
;     (kernel-arguments '("mitigations=off" "console=ttyS0" "console=tty1"))
;
;     (bootloader (bootloader-configuration
;                  (bootloader grub-bootloader)
;                  (target "/dev/sda")
;                  (keyboard-layout (operating-system-keyboard-layout base-system-config))))
;
;     (file-systems
;      (append
;       (list
;        (file-system
;           (device "/dev/sda")
;           (mount-point "/")
;           (type "ext4")))
;       %base-file-systems))
;
;     (services `(,(service dhcp-client-service-type)
;                 ,@(modify-services
;                    base-services
;                    (openssh-service-type
;                     config => (openssh-configuration
;                                (inherit config)
;                                (authorized-keys
;                                 (map
;                                  (lambda (key-config)
;                                    (if (string=? (car key-config) "root")
;                                        (append key-config `(,(local-file "../data/mel-robin.pub")))
;                                        key-config))
;                                  ssh-default-authorized-keys)))))))))
;
; ;; (list (machine
; ;;        (operating-system seshat-system-config)
; ;;        (environment managed-host-environment-type)
; ;;        (configuration
; ;;         (machine-ssh-configuration
; ;;          (host-name "coroot.de")
; ;;          (system "x86_64-linux")
; ;;          (identity "/home/robin/.ssh/id_ed25519")
; ;;          (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHDeKweSXGxPW8MQvynT2tN19M5ttMDPiGeGGg4Cbic2")))))
;
; leg-system-config
