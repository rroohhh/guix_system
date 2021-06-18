(define-module (config mel)
  #:use-module (config desktop-base)
  #:use-module (config network)
  #:use-module (config network-utils)
  #:use-module ((config base) #:select (ssh-default-authorized-keys))
  #:use-module (services concourse)
  #:use-module (services btrbk)
  #:use-module (services secrets)
  #:use-module (services influxdb)
  #:use-module (services vault)
  #:use-module (ice-9 textual-ports)
  #:use-module (gnu)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services networking)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages databases))

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

[[inputs.statsd]]
  parse_data_dog_tags = true
  datadog_extensions = true
  datadog_distributions = true
")


(define root-vault-connection
  #~(vault-connection-configuration
     (cert-dir #$(vault-cert-dir "/data/projects/guix_system/data/vault.server.cert.pem"))
     (auth-token-file "/data/projects/guix_system/data/secrets/vault_root_token")))

(define extra-users
  `(("anuejn" "users" ("wheel" "netdev" "audio" "video" "docker" "kvm" "libvirt") "data/anuejn.pub")))

(define-secret concourse-web-vault-role-id "concourse")
(define-secret concourse-web-vault-secret-id "concourse")

(define-public mel-system-config
  (operating-system
   (inherit base-desktop-system-config)

   (host-name "mel")

   (kernel-arguments '("--rootflags=compress=zstd,discard,subvolid=5439,subvol=@guix_root,acl" "mitigations=off"))

   (users (append (map (lambda (user)
                         (user-account
                          (name (car user))
                          (group (cadr user))
                          (supplementary-groups (caddr user))))
                       extra-users)
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
                               (openssh-service-type
                                config =>
                                (openssh-configuration
                                 (inherit config)
                                 (authorized-keys
                                  (append
                                   (map (lambda (user)
                                          `(,(car user) ,(local-file (cadddr user)))) extra-users)
                                   ssh-default-authorized-keys)))))
              (networking-for host-name)
              (list
               (service influxdb-service-type
                        (influxdb-configuration
                         (host "0.0.0.0"))) ; TODO(robin): any way to restrict this more? this would need a way to specify multiple addresses (localhost + 192.168.3.4) or just move everything over to use 192.168.3.4...
               (simple-service 'telegraf-postgresql-role
                               postgresql-role-service-type
                               (list
                                (postgresql-role
                                 (name "telegraf")
                                 (create-database? #t))))
               (service telegraf-service-type
                        (telegraf-configuration
                         (influxdb-token-file "/data/projects/guix_system/data/secrets/mel_telegraf_token") ; TODO(robin): use vault??
                         (influxdb-bucket "monitoring")
                         (influxdb-orga "infra")
                         (config (list
                                  %telegraf-default-config
                                  extra-telegraf-config))))
               (service vault-service-type
                        (vault-configuration
                         (address "0.0.0.0:8200")
                         (ui? #t)
                         (tls-key-file "/data/projects/guix_system/data/secrets/vault.server.key.pem")
                         (tls-cert-file "/data/projects/guix_system/data/vault.server.cert.pem")
                         (unauthenticated-metrics-access? #t)))
               (service vault-unseal-service-type
                        (vault-unseal-configuration
                         (vault-connection root-vault-connection)
                         (unsealing-keys-file "/data/projects/guix_system/data/secrets/vault_unseal_keys")))
               (service generated-secrets-root-service-type)
               (vault-generated-secret
                concourse-web-vault-role-id
                #~(lambda ()
                    (let* ((conn #$root-vault-connection)
                           (mount (assoc-ref (vault-list-mounts conn) "concourse/")))
                      (begin
                        (unless mount
                          (vault-enable-mount conn "concourse" '(("type" . "kv") ("options" . (("version" . "2"))))))
                        (vault-set-policy conn "concourse" "path \"concourse/*\" {
    policy = \"read\"
}")
                        (unless (assoc-ref (vault-list-auth conn) "approle/")
                          (vault-enable-auth conn "approle"))
                        (vault-set-approle conn "concourse" #("concourse"))
                        (vault-get-approle-role-id conn "concourse"))))
                '(vault-unseal)) ;; TODO(robin): figure out how to add this automatically on localhost
               (with-generated-secrets
                (list concourse-web-vault-role-id)
                (lambda _
                  (vault-generated-secret
                   concourse-web-vault-secret-id
                   #~(lambda ()
                       (assoc-ref (vault-generate-secret-id #$root-vault-connection "concourse") "secret_id")))))
               (with-generated-secrets
                (list concourse-web-vault-role-id concourse-web-vault-secret-id)
                (lambda (secrets)
                  (let* ((vault-role-id (assoc-ref secrets 'concourse-web-vault-role-id))
                         (vault-secret-id (assoc-ref secrets 'concourse-web-vault-secret-id)))
                    (service concourse-web-service-type
                             (concourse-web-configuration
                              (local-users
                               '(("admin" "$2b$12$KnZ1OzFYJFQ.GGdZXFCMr.BXr9TRuvsz7z7lEJg88FKIm/QoeOSEm")))
                              (main-team-local-user "admin")
                              (tsa-host-key "/data/projects/guix_system/data/secrets/tsa_host_key")
                              (authorized-worker-keys '("ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCqBfLSGvGdYsmDhE0bqSN38oAbcndV8euE4qiqsKa0RUTg+gIhfzG/HYyUlWFG3eQNpE+v58N8XDD3NQtzqhY5m3ClscVtNpPqYDScoT8+QJDZrJs7yHmOsUP0nm+QBsIJB7YTZhrzcFI5sa2IGghG930Fy+AdtSejwpE5lx1jiaIlFHOaq4FQkbHHhtewWYDqBf/K7boui+/ew+HyyIktiApwuRXyNs2azCC9H2ohjyM12ur+X7Is3bc6awAlLQmjs944sxZC2uHqXF3CYuc5G//zsZPbT94vcssp5OPyQbjBYLlo5/7R7F0GIhDApCEv8OqmO4SCmwj+w3Jqk4abRS6+H+270xnEYE3Rlwi6J2dlkl+r8ON7zQaDl/mc61cyeYh4a66YWq+gW56jdbNNAdC3PDQqaosNbQlWNWhesTlqLV1c8S4S+2YZRrAU+tAIyPi7W5d3VY8jkF3FFqpfs8TI6S4ArrjXAXKZAWOgsOESDxFG/jvGpQQLXx+NNglgo6x9I93FNPFYyr8NYu24be/E9edPkMEZeL4kAgHNrxcEXo7535BOJNfOSkERulHv20Z+uCeBYJEuUs4I889MwmV0btmAltEslITCkJTOWV3Y+uCYJjLNWtUQsmGdGG8lj1qNK4rlLUk+a+qFaiaHinsEGVXaLta7sNckYot1Sw=="))
                              (external-url "https://ci.coroot.de")
                              (session-singing-key "/data/projects/guix_system/data/secrets/session_signing_key")
                              (vault-url "https://127.0.0.1:8200")
                              (vault-ca-dir (vault-cert-dir "/data/projects/guix_system/data/vault.server.cert.pem"))
                              (vault-role-id-file vault-role-id)
                              (vault-secret-id-file vault-secret-id)
                              (prometheus-bind-ip "127.0.0.1")
                              (prometheus-bind-port "8123"))))))
               (service concourse-worker-service-type
                        (concourse-worker-configuration
                         (private-key "/data/projects/guix_system/data/secrets/worker_key")
                         (work-dir "/var/lib/concourse-worker/work")
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
                         (vpn-plugins (list network-manager-openvpn network-manager-openconnect network-manager-vpnc)))))))))
