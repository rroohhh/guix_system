(define-module (config seshat)
  #:use-module (config base)
  #:use-module (config network)
  #:use-module (config network-utils)
  #:use-module (services influxdb)
  #:use-module (services scrabble)
  #:use-module (services caddy)
  #:use-module (services quassel)
  #:use-module (services boulderhaus)
  #:use-module (gnu)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services networking)
  #:use-module (gnu services databases)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages databases)
  #:use-module (guix utils)
  #:use-module (guix modules)
  #:use-module (srfi srfi-26))

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

(use-modules (ice-9 match))

(define (source-module-closure* modules)
  (source-module-closure
   modules
   #:select?
   (lambda* (name)
     (if (guix-module-name? name)
         #t
         (match name
           (('gnutls) #t)
           (('json _) #t)
           (('misc _) #t)
           (('config _) #t)
           (('services _) #t)
           (_ #f))))))

(define (caddy-configs host-name)
  (let* ((host "coroot.de")
         (simple-configs '(("vault.<host>" "mel" "8200")
                           ("influx.<host>" "mel" "8086")
                           ("ci.<host>" "mel" "8080")
                           ("git.froheiyd.de" "rofydi" "3000"))))
    (with-imported-modules (source-module-closure* '((config network))) ; TODO(robin): write a better version of source-module-closure to use here
        #~(begin
            (use-modules (ice-9 string-fun))
            (use-modules (config network))
            (string-replace-substring
             (string-join
              `("metrics.<host> { metrics }"
                ,@(map
                   (lambda (config)
                     (format #f "~a { reverse_proxy * ~a:~a }" (car config) (address-of (cadr config) #$host-name) (caddr config)))
                   '#$simple-configs)
                "<host> { reverse_proxy /scrabble localhost:48443 }" ;; scrabble
                ,(string-append "bh.<host> {
root * " #$boulderhaus-booking-webui "
reverse_proxy /api/* localhost:5558
file_server

log {
  output stdout
  level debug
}

} "))

              ;; scrabble
              "\n")
             "<host>"
             #$host)))))

(define extra-telegraf-config
  "[[inputs.postgresql]]
  ## specify address via a url matching:
  ##   postgres://[pqgotest[:password]]@localhost[/dbname]\
  ##       ?sslmode=[disable|verify-ca|verify-full]
  ## or a simple string:
  ##   host=localhost user=pqgotest password=... sslmode=... dbname=app_production
  ##
  ## All connection parameters are optional.
  ##
  ## Without the dbname parameter, the driver will default to a database
  ## with the same name as the user. This dbname is just for instantiating a
  ## connection with the server and doesn't restrict the databases we are trying
  ## to grab metrics for.
  ##
  address = \"host=/tmp user=telegraf sslmode=disable\"
  max_lifetime = \"0s\"
")


(define-public seshat-system-config
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
               ,(service bhbooking-service-type)
               ,(service postgresql-service-type
                         (postgresql-configuration
                          (postgresql postgresql-13)))
               ,(simple-service 'telegraf-postgresql-role
                                postgresql-role-service-type
                                (list
                                 (postgresql-role
                                  (name "telegraf")
                                  (create-database? #t))))
               ,(simple-service 'telegraf-postgresql-role
                                postgresql-role-service-type
                                (list
                                 (postgresql-role
                                  (name "quasselcore")
                                  (create-database? #t))))
               ,(service quassel-service-type)
               ,(service caddy-service-type
                         (caddy-configuration
                          (config-blocks (list (caddy-configs host-name)))))
               ,(service scrabble-service-type)
               ,(service telegraf-service-type
                         (telegraf-configuration
                          (influxdb-address (string-append "http://" (address-of "mel" host-name) ":8086"))
                          (influxdb-token-file "/secrets/seshat_telegraf_token") ; TODO(robin): use vault??
                          (influxdb-bucket "monitoring")
                          (influxdb-orga "infra")
                          (config (list
                                   %telegraf-default-config
                                   extra-telegraf-config))))
               ,@(networking-for host-name)
               ,@(modify-services
                  base-services
                  (openssh-service-type
                   config => (openssh-configuration
                              (inherit config)
                              (authorized-keys
                               (map
                                (lambda (key-config)
                                  (if (string=? (car key-config) "root")
                                      (append key-config `(,(local-file "../data/mel-robin.pub")))
                                      key-config))
                                ssh-default-authorized-keys)))))))))
                                 
(list (machine
       (operating-system seshat-system-config)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (host-name "coroot.de")
         (system "x86_64-linux")
         (identity "/home/robin/.ssh/id_ed25519")
         (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHDeKweSXGxPW8MQvynT2tN19M5ttMDPiGeGGg4Cbic2")))))
