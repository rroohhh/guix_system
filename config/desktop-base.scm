(define-module (config desktop-base)
  #:use-module (vup patches)
  #:use-module (gnu)
  #:use-module (gnu system nss)
  #:use-module (config base)
  #:use-module (config sysprof)
  #:use-module (services pulseaudio)
  ;; #:use-module (services cgroupv2)
  #:use-module (services misc)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module ((gnu services desktop) #:select (cups-pk-helper-service-type))
  #:use-module (gnu services nix)
  #:use-module (gnu services dbus)
  #:use-module (gnu services cups)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages android)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
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
                                             "kvm" "pulse-access" "libvirt"
                                             "dialout")))
                    (user-account
                     (name "pulse")
                     (group "pulse")
                     (home-directory "/var/run/pulse")
                     (create-home-directory? #f)
                     (system? #t)
                     (supplementary-groups '("audio" "lp")))) ; lp for bluetooth access
                   %base-user-accounts))

    (packages
     (append
      (list
       power-profiles-daemon
       bluez)
      (operating-system-packages base-system-config)))

    (file-systems '())))



(define power-profiles-daemon-shepherd
  (list (shepherd-service
         (documentation "power-profiles-daemon")
         (provision '(power-profiles-daemon))
         (requirement '(dbus-system))
         (start #~(make-forkexec-constructor
                   (list #$(file-append power-profiles-daemon "/libexec/power-profiles-daemon"))
                   #:log-file "/var/log/power-profiles-daemon.log"))
         (stop #~(make-kill-destructor)))))



(define-public base-desktop-services
  `(
    ,(service caps2esc-service-type)
    ,(service root-remount-service-type)
    ,(extra-special-file
                     "/etc/docker/daemon.json" (plain-file "daemon.json"
                                                           "{
  \"ipv6\": false,
  \"bip\": \"172.39.1.5/24\",
  \"fixed-cidr\": \"172.39.1.0/25\"
}
"))
    ,(service docker-service-type)
    ,(service cups-pk-helper-service-type)

    ,(service upower-service-type)
    ,(service colord-service-type)
    ,(service geoclue-service-type)

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

    ,(simple-service 'polkit-sysprof polkit-service-type (list sysprof-new))
    ,(simple-service 'dbus-sysprof dbus-root-service-type (list sysprof-new))
    ,(simple-service 'turbostat-kernel-module kernel-module-loader-service-type
                           '("msr"))

    ,(simple-service 'polkit-power-profiles polkit-service-type (list power-profiles-daemon))
    ,(simple-service 'dbus-power-profiles dbus-root-service-type (list power-profiles-daemon))
    ,(simple-service 'shepherd-power-profiles shepherd-root-service-type power-profiles-daemon-shepherd)

    ,(simple-service 'mtp udev-service-type (list libmtp))
    ,(simple-service 'brightnessctl-udev udev-service-type (list brightnessctl))
    ,(service sane-service-type)

    ,(pam-limits-service
      (list
       (pam-limits-entry "robin" 'both 'nofile 1000000)
       (pam-limits-entry "@audio" 'both 'rtprio 99)
       (pam-limits-entry "@audio" 'both 'memlock 'unlimited)))
    ,(service modem-manager-service-type)
    ;; ,(service seatd-service-type)
    ,@(modify-services base-services
        (udev-service-type config =>
                           (udev-configuration
                            (inherit config)
                            (rules
                             (append
                              (udev-configuration-rules config)
                              (list android-udev-rules python-openant/udev)))))
        (delete console-font-service-type)
        ;; (delete elogind-service-type)
        (delete login-service-type)
        (delete agetty-service-type)
        (delete mingetty-service-type))))
