(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm

(use-modules (gnu services networking))
(use-modules (gnu services virtualization))
(use-modules (gnu services ssh))
(use-modules (gnu system file-systems))
(use-modules (gnu packages gnome))

(use-modules (guix gexp))
(use-modules (guix records))

(use-modules (config-base))
(use-modules (config-desktop-base))
(use-modules (util))
(use-modules (wireguard))

(operating-system
  (inherit base-desktop-system-config)

  (host-name "mel")

  (kernel-arguments '("--rootflags=compress=zstd,discard,subvolid=5439,subvol=@guix_root,acl" "mitigations=off"))

  (users (append (list
                  (user-account
                   (name "anuejn")
                   (group "users")
                   (supplementary-groups '("wheel" "netdev" "audio" "video" "docker" "kvm" "libvirt")))) 
                 (operating-system-users base-desktop-system-config)))

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

  (services (append 
    (modify-services base-desktop-services
      (openssh-service-type config =>
              (openssh-configuration
		(inherit config)
                (authorized-keys (append 
          			 `(("anuejn" ,(local-file "anuejn.pub")))
				 ssh-default-authorized-keys)))))
      (list
	;; (service wireguard-service-type
	;;          (wireguard-configuration
	;;           (address "192.168.3.4/24")
	;;           (private-key (read-file-as-string "wg_mel.key"))
	;;           (peers (list
	;;                   '("yZIp9A+SZIhKmP+leydI3kqSYI4P9eKpI/JJxvzAIDQ=" "192.168.3.0/24" "46.101.193.235:443")))))
	(service libvirt-service-type
	  (libvirt-configuration
   	    (unix-sock-group "libvirt")))
	(service virtlog-service-type
	  (virtlog-configuration
	    (max-clients 1000)))
	(service wpa-supplicant-service-type)
	(service network-manager-service-type
	  (network-manager-configuration
	    (dns "dnsmasq")
  	    (vpn-plugins (list network-manager-openvpn network-manager-openconnect network-manager-vpnc))))))))
