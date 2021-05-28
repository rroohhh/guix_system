(add-to-load-path (dirname (current-filename))) ; for my-tlp.scm

(use-modules (gnu services networking))
(use-modules (gnu services virtualization))
(use-modules (gnu services ssh))
(use-modules (gnu services databases))
(use-modules (gnu system file-systems))
(use-modules (gnu packages gnome))
(use-modules (gnu packages databases))
(use-modules (gnu packages rsync))
(use-modules (gnu packages freedesktop))

(use-modules (guix gexp))
(use-modules (guix records))
(use-modules (guix packages))
(use-modules (guix utils))

(use-modules (config-base))
(use-modules (config-desktop-base))
(use-modules (util))
(use-modules (wireguard))
(use-modules (btrbk))
(use-modules (concourse))
(use-modules (cgroupv2))
(use-modules (vup misc))

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
       (device (uuid "5195ef5f-6cac-4ac1-964d-5a50d7cf45ea"))
       (mount-point "/backup")
       (type "btrfs")
       (options "compress=zstd:9,acl"))
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
               ;; (elogind-service-type config =>
               ;;   (elogind-configuration
               ;;    (elogind patched-elogind)))
               (openssh-service-type config =>
                 (openssh-configuration
                   (inherit config)
                   (authorized-keys (append
                                     `(("anuejn" ,(local-file "anuejn.pub")))
                                     ssh-default-authorized-keys)))))
             (list
              (service concourse-web-service-type
                       (concourse-web-configuration
                        (local-users
                         '(("admin" "$2b$12$KnZ1OzFYJFQ.GGdZXFCMr.BXr9TRuvsz7z7lEJg88FKIm/QoeOSEm")))
                        (main-team-local-user "admin")
                        (tsa-host-key "/data/projects/guix_system/tsa_host_key")
                        (authorized-worker-keys '("ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCqBfLSGvGdYsmDhE0bqSN38oAbcndV8euE4qiqsKa0RUTg+gIhfzG/HYyUlWFG3eQNpE+v58N8XDD3NQtzqhY5m3ClscVtNpPqYDScoT8+QJDZrJs7yHmOsUP0nm+QBsIJB7YTZhrzcFI5sa2IGghG930Fy+AdtSejwpE5lx1jiaIlFHOaq4FQkbHHhtewWYDqBf/K7boui+/ew+HyyIktiApwuRXyNs2azCC9H2ohjyM12ur+X7Is3bc6awAlLQmjs944sxZC2uHqXF3CYuc5G//zsZPbT94vcssp5OPyQbjBYLlo5/7R7F0GIhDApCEv8OqmO4SCmwj+w3Jqk4abRS6+H+270xnEYE3Rlwi6J2dlkl+r8ON7zQaDl/mc61cyeYh4a66YWq+gW56jdbNNAdC3PDQqaosNbQlWNWhesTlqLV1c8S4S+2YZRrAU+tAIyPi7W5d3VY8jkF3FFqpfs8TI6S4ArrjXAXKZAWOgsOESDxFG/jvGpQQLXx+NNglgo6x9I93FNPFYyr8NYu24be/E9edPkMEZeL4kAgHNrxcEXo7535BOJNfOSkERulHv20Z+uCeBYJEuUs4I889MwmV0btmAltEslITCkJTOWV3Y+uCYJjLNWtUQsmGdGG8lj1qNK4rlLUk+a+qFaiaHinsEGVXaLta7sNckYot1Sw=="))
                        (external-url "https://ci.vup.niemo.de")
                        (session-singing-key "/data/projects/guix_system/session_signing_key")))
              (service concourse-worker-service-type
                       (concourse-worker-configuration
                        (private-key "/data/projects/guix_system/worker_key")
                        ("/var/lib/concourse-worker/work" work-dir)
                        (runtime 'containerd)
                        (mtu 1500)
                        (tsa-public-key "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3QQFsAn7xdBdR7uDlNid0lOSeGED+/N4yKHPOVc6texlznHnGDdOGvp0QwhMsYMs4RGLrYdYs3E/FtaJY1ze6peqj6VYrtuFtbWnWLjtQwkpBcI+tIOK1jnx4LW/kBDrpMzdK1tOApzUfC3JODFpgUjlBDm2A+npT3nceYuw5pjcVPpszauyXm3vBvvtdiKIIw+F4o8xA/AHTbjIupgucuPwku3fl1+6FbXz7zLOziZfrj/y09TmvaEz5OxnS70B852r/aZ4PgTJ36f21RZW6SXGvCbi2jy+vnBMOPvw56Hza4SXFGSIDFBx6nfWIxU+r8x15fi9SvN6eHQSpETWl6LaSjXhJs9hZKlTrTejEWleTsVsZ5/VhekoOm7V0IwVo3K1535Q4TIM3njA1TgLPEfr7Mq+bccKaD5mLCuoinb10PhWILFLCHHP2lCLRHz/w9QVPguVNpViMC3ywDcqff0EPCrjXYv1ZBx+5RvAGQDCqZ/RHgv7Rt4qXSRezczBI4f45OBkoR/sWhxtNgm2IYBZSe4cecAvmIIvVrTh6/sGcMJaEH9gnEX4JtTTHR45U3nbf2riduOBd9owzVjXW9Yj66IwDFg68rT9eMsIQWKFJeW0/+SvYo17ytpxdNKhZARMQR78Uze4FQEoIUl/Cp1rGOHhQ05unYmOC9opP3w==")))
              (service btrbk-service-type
                       (btrbk-configuration
                        (schedule "04 * * * *")
                        (snapshot-preserve-min "1d")
                        (snapshot-preserve "7d 4w *m")
                        (volume "/backup")
                        (subvolumes '("seshat-backup"))
                        (pre-run-hook #~(string-append #$rsync "/bin/rsync" " -azz --exclude=/proc --exclude=/dev --exclude=/sys --exclude=/run -e 'ssh -i /home/robin/.ssh/id_ed25519' --delete --inplace --numeric-ids --acls --xattrs coroot.de:/ /backup/seshat-backup/"))))
              (service libvirt-service-type
               (libvirt-configuration
                 (unix-sock-group "libvirt")))
              (service virtlog-service-type
                (virtlog-configuration
                  (max-clients 1000)))
              (service postgresql-service-type
                       (postgresql-configuration
                        (postgresql postgresql-13)))
              (service wpa-supplicant-service-type)
              (service network-manager-service-type
                (network-manager-configuration
                  (dns "dnsmasq")
                  (vpn-plugins (list network-manager-openvpn network-manager-openconnect network-manager-vpnc))))))))
