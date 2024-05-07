(define-module (config welp)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services networking)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (config base)
  #:use-module (config linux)
  #:use-module (config network-utils))


(define u-boot-cubieboard2
  (make-u-boot-package "Cubieboard2" "arm-linux-gnueabihf"))

(define install-allwinner-u-boot
  #~(lambda (bootloader root-index image)
      (let ((u-boot (string-append bootloader
                                   "/libexec/u-boot-sunxi-with-spl.bin")))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 8 1024)))))

(define u-boot-allwinner-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (disk-image-installer install-allwinner-u-boot)))

(define u-boot-cubieboard2-bootloader
  (bootloader
    (inherit u-boot-allwinner-bootloader)
    (package u-boot-cubieboard2)))

(define welp-transform-no-tests
  ;; The package transformation procedure.
  (options->transformation
   '((without-tests . "guix"))))

(define welp-transform
  ;; The package transformation procedure.
  (package-input-rewriting/spec `(("guix" . ,(welp-transform-no-tests guix)))))

(define-public welp-system-config
  (welp-transform
   (operating-system
     (inherit base-system-config)
     (host-name "welp")
     (kernel linux-nonfree/extra_config-arm)
     (bootloader
      (bootloader-configuration
       (targets (list "/dev/mmcblk0"))
       (bootloader u-boot-cubietruck-bootloader)))
     (file-systems
      (cons* (file-system
               (mount-point "/")
               (device "/dev/mmcblk0p1")
               (type "ext4"))
             %base-file-systems))
     (services (append
                base-services
                (networking-for host-name)
                (list
                 (service wpa-supplicant-service-type)
                 (service network-manager-service-type)))))))

(list
      (machine
       (operating-system welp-system-config)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (host-name (operating-system-host-name welp-system-config))
         (system "armhf-linux")
         (identity "/home/robin/.ssh/id_ed25519")
         (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII5NlwiXunTqXKa72M3Sa4wy0yDwdG7+lA9eJBBTTDlN")))))


welp-system-config
