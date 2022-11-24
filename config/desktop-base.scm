(define-module (config desktop-base)
  #:use-module (vup patches)
  #:use-module (gnu)
  #:use-module (gnu system nss)
  #:use-module (config base)
  #:use-module (services pulseaudio)
  #:use-module (services cgroupv2)
  #:use-module (services misc)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module ((gnu services desktop) #:select (cups-pk-helper-service-type))
  #:use-module (gnu services nix)
  #:use-module (gnu services cups)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages android)
  #:use-module (vup caps2esc)
  ;; #:use-module (vup docker)
  #:use-module (vup python-xyz))

(define-public base-desktop-system-config
  (operating-system
    (inherit base-system-config)
    (groups (append %base-groups
                    (list
                     (user-group (system? #t) (name "libvirt"))
                     (user-group (system? #t) (name "adbusers"))
                     (user-group (system? #t) (name "pulse"))
                     (user-group (system? #t) (name "pulse-access")))))

    (users (append (list
                    (user-account
                     (name "robin")
                     (comment "owner")
                     (group "users")
                     (supplementary-groups '("lp" "wheel" "netdev"
                                             "audio" "video" "docker"
                                             "adbusers"
                                             "kvm" "pulse-access" "libvirt")))
                    (user-account
                     (name "pulse")
                     (group "pulse")
                     (home-directory "/var/run/pulse")
                     (create-home-directory? #f)
                     (system? #t)
                     (supplementary-groups '("audio" "lp")))) ; lp for bluetooth access
                   %base-user-accounts))

    (file-systems '())))

(define-public base-desktop-services
  `(
    ,(service caps2esc-service-type)
    ,(service root-remount-service-type)
    ,(service docker-service-type
              (docker-configuration
               (config (plain-file "daemon.json"
                                          "{
  \"ipv6\": false,
  \"bip\": \"172.39.1.5/24\",
  \"fixed-cidr\": \"172.39.1.0/25\"
}
"))))
    ,(service cups-pk-helper-service-type)

    ,(service qemu-binfmt-service-type
        (qemu-binfmt-configuration
          (platforms (lookup-qemu-platforms "arm" "aarch64"))))

    ,(service nix-service-type
              (nix-configuration
               (build-sandbox-items '("/bin/sh"))))

    ,(service cups-service-type
              (cups-configuration
               (web-interface? #t)))

    ,(udisks-service)

    ,(service earlyoom-service-type)

    ,(pam-limits-service
      (list
       (pam-limits-entry "robin" 'both 'nofile 1000000)
       (pam-limits-entry "@audio" 'both 'rtprio 99)
       (pam-limits-entry "@audio" 'both 'memlock 'unlimited)))
    ,(service modem-manager-service-type)
    ,@(modify-services base-services
        (udev-service-type config =>
                           (udev-configuration
                            (inherit config)
                            (rules
                             (append
                              (udev-configuration-rules config)
                              (list android-udev-rules python-openant/udev))))))))
    
