(define-module (config base)
  #:use-module (vup patches) ; really bad hacks, but who cares
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu)
  ;; #:use-module ((gnu system nss) #:select (%mdns-host-lookup-nss))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages tmux)
  #:use-module (gnu services admin)
  #:use-module (gnu services dbus)
  #:use-module (gnu services xorg)
  #:use-module (gnu services avahi)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services desktop)
  #:use-module (gnu system setuid)
  #:use-module (vup linux)
  #:use-module (vup hwinfo)
  #:use-module (config linux)
  ;; #:use-module (services cgroupv2)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public docker-fixed
  (package
    (inherit docker)
    (name "docker-fixed")
    (arguments
     (append `(#:tests? #f ,@(package-arguments docker))))))


(define-public base-system-config
  (operating-system
    (host-name "placeholder")
    (timezone "Europe/Berlin")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "de" "vup"))
    (initrd-modules (append '("virtio_crypto" "virtio-gpu" "virtio_pmem" "nd_virtio"
                              "virtio_scsi" "virtio_rpmsg_bus" "virtio_mem"
                              "virtio_dma_buf" "virtio_mmio" "virtio"
                              "virtio_ring" "virtio_input" "virtio_vdpa"
                              "caif_virtio" "vmw_vsock_virtio_transport"
                              "vmw_vsock_virtio_transport_common" "9pnet_virtio") %base-initrd-modules))


    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))
                 (keyboard-layout keyboard-layout)))

    (setuid-programs (append (list
                              (file-like->setuid-program (file-append hwinfo "/bin/hwinfo")))
                             %setuid-programs))

    (kernel linux-nonfree/extra_config-x86) ;; TODO(robin): switch this automtically depending on arch
    (firmware (append (list linux-firmware-nonfree) %base-firmware))
    (file-systems '())

    (packages
     (append
      (list
       tmux
       rsync
       xfsprogs
       openssh vim
       ;; nss-certs ;; for HTTPS access
       turbostat) ;; sysprof
      %base-packages))

    (name-service-switch %mdns-host-lookup-nss)))

(define-public aarch64-base-system-config
  (operating-system
    (inherit base-system-config)
    (initrd-modules (remove
                     (cut member <> '("virtio_mem" "virtio_rpmsg_bus" "caif_virtio"))
                     (operating-system-initrd-modules base-system-config)))))

(define-public ssh-default-authorized-keys `(("robin" ,(local-file "../data/robin.pub"))
                                             ("root"  ,(local-file "../data/robin.pub"))))

(define-public base-services
  `(,(service polkit-service-type)
    ,(service elogind-service-type
              (elogind-configuration
               (handle-lid-switch 'suspend)))
    ,(service dbus-root-service-type)
    ,(service accountsservice-service-type)
    ,(service localed-service-type)
    ,(service avahi-service-type)
    ,(service ntp-service-type)
    ,(service openssh-service-type
              (openssh-configuration
               (permit-root-login 'prohibit-password)
               (x11-forwarding? #t)
               (authorized-keys ssh-default-authorized-keys)
               (extra-content "PermitUserEnvironment yes")))
    ,(service guix-publish-service-type
              (guix-publish-configuration
               (host "0.0.0.0")
               (port 12734)
               (advertise? #t)))
    ;; ,(service log-rotation-service-type)
    ,@(modify-services %base-services
        (guix-service-type config =>
                (guix-configuration
                  (inherit config)
                  (substitute-urls
                    (append
                     (list
                      "https://guix.bordeaux.inria.fr"
                      "https://substitutes.nonguix.org")
                            %default-substitute-urls))
                  (discover? #t)
                  (authorized-keys
                   (append (list (local-file "../data/ada-signing-key.pub")
                                 (local-file "../data/mel-signing-key.pub")
                                 (local-file "../data/nonguix-signing-key.pub")
                                 (local-file "../data/guix-hpc-signing-key.pub"))
                           %default-authorized-guix-keys))))
        (sysctl-service-type config =>
                       (sysctl-configuration
                         (settings (append '(("kernel.dmesg_restrict" . "0")
                                             ("net.ipv4.ip_forward" . "1"))
                                           %default-sysctl-settings)))))))
