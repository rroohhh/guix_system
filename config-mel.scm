(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm

(use-modules (gnu services networking))
(use-modules (gnu system file-systems))

(use-modules (config-common))

(operating-system
  (inherit common-system-config)

  (host-name "mel")

  (kernel-arguments '("--rootflags=compress=zstd,discard,subvolid=5439,subvol=@guix_root,acl" "mitigations=off"))

  (file-systems
   (append
    (list
     (file-system
       (device (uuid "cd09cac9-d42e-4f12-9f7a-b93121b3b5fb"))
       (mount-point "/")
       (type "btrfs")
       (options "compress=zstd,discard,subvolid=5439,subvol=@guix_root,acl"))
                                        ;             (needed-for-boot? #t))
     (file-system
       (device (uuid "cd09cac9-d42e-4f12-9f7a-b93121b3b5fb"))
       (mount-point "/data")
       (type "btrfs")
       (options "compress=zstd,discard,subvolid=259,subvol=@robin,acl"))
     (file-system
       (device (uuid "1BD1-2470" 'fat))
       (mount-point "/boot/efi")
       (type "vfat"))
     (file-system
       (device "none")
       (mount-point "/tmp")
       (type "tmpfs")
       (check? #f)))
    %base-file-systems))

  (services (append common-services
                    (list
                      (service wpa-supplicant-service-type)
                      (service connman-service-type
                            (connman-configuration))))))
