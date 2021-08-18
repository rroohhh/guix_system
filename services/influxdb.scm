(define-module (services influxdb)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (vup influxdb)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (influxdb-configuration
            influxdb-service-type
            telegraf-configuration
            %telegraf-default-config
            telegraf-service-type))

(define-record-type* <influxdb-configuration>
  influxdb-configuration make-influxdb-configuration
  influxdb-configuration?
  ;; <package>
  (influxd                influxdb-configuration-influxd
                          (default influxd))

  ;; user to run it under
  (user                   influxdb-configuration-user
                          (default "influxd"))

  ;; port to run under
  (port                   influxdb-configuration-port
                          (default 8086))

  ;; host to run under
  (host                   influxdb-configuration-host
                          (default "localhost")))

(define (influxdb-shepherd-service config)
  "Return a <shepherd-service> for influxdb with CONFIG."

  (define args
    `(,(format #f "--http-bind-address=~a:~a" (influxdb-configuration-host config) (influxdb-configuration-port config))))

  (list (shepherd-service
         (documentation "influxd")
         (requirement '(networking))
         (provision '(influxdb))
         (start #~(make-forkexec-constructor
                                  (append (list #$(file-append (influxdb-configuration-influxd config)
                                                               "/bin/influxd") "run") (list #$@args))
                                  #:user #$(influxdb-configuration-user config)
                                  #:log-file "/var/log/influxdb.log"))
         (stop #~(make-kill-destructor))
         (auto-start? #t)
         (modules `((ice-9 textual-ports))))))

(define* (influxdb-accounts config)
  (let* ((name (influxdb-configuration-user config)))
   (list
     (user-group (name name) (system? #t))
     (user-account
      (name name)
      (group name)
      (system? #t)
      (comment "influxdb user")
      (home-directory (string-append "/var/influxdb/" name))
      (shell (file-append shadow "/sbin/nologin"))))))

(define influxdb-service-type
  (service-type (name 'influxdb)
                (description
                 "run influxdb")
                (default-value (influxdb-configuration))
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          influxdb-shepherd-service)
                       (service-extension account-service-type
                                          influxdb-accounts)))))

(define %telegraf-default-config
  #~(string-append "# Read metrics about cpu usage
[[inputs.cpu]]
  percpu = true
  totalcpu = true
  collect_cpu_time = true
  report_active = true


# Read metrics about disk usage by mount point
[[inputs.disk]]
  # mount_points = [\"/\"]

  ## Ignore mount points by filesystem type.
  ignore_fs = [\"tmpfs\", \"devtmpfs\", \"devfs\", \"overlay\", \"aufs\", \"squashfs\"]


# Read metrics about disk IO by device
[[inputs.diskio]]
  ## By default, telegraf will gather stats for all devices including
  ## disk partitions.
  ## Setting devices will restrict the stats to the specified devices.


# Get kernel statistics from /proc/stat
[[inputs.kernel]]
  # no configuration


# Read metrics about memory usage
[[inputs.mem]]
  # no configuration

# Read metrics about temperature
[[inputs.temp]]
  # no configuration


# Get the number of processes and group them by status
[[inputs.processes]]
  # no configuration


# Read metrics about swap memory usage
[[inputs.swap]]
  # no configuration


# Read metrics about system load & uptime
[[inputs.system]]
  # no configuration


# Collect statistics about itself
[[inputs.internal]]
  ## If true, collect telegraf memory stats.
  collect_memstats = true


[[inputs.net]]
  interval = \"1s\"


# Read TCP metrics such as established, time wait and sockets counts.
[[inputs.netstat]]
  # no configuration


# Collect kernel snmp counters and network interface statistics
[[inputs.nstat]]
  ## file paths for proc files. If empty default paths will be used:
  ##    /proc/net/netstat, /proc/net/snmp, /proc/net/snmp6
  ## These can also be overridden with env variables, see README.
  ## dump metrics with 0 values too
  dump_zeros       = true


# Read specific statistics per cgroup
[[inputs.cgroup]]
  # no configuration


[[inputs.sensors]]
  # no configuration

[[inputs.wireguard]]
  # no configuration

# Read metrics from storage devices supporting S.M.A.R.T.
[[inputs.smart]]
  ## On most platforms smartctl requires root access.
  ## Setting 'use_sudo' to true will make use of sudo to run smartctl.
  ## Sudo must be configured to to allow the telegraf user to run smartctl
  ## with out password.
  path_smartctl = \"/run/setuid-programs/smartctl\"
  use_sudo = false
  attributes = true
"))

(define-record-type* <telegraf-configuration>
  telegraf-configuration make-telegraf-configuration
  telegraf-configuration?
  ;; <package>
  (telegraf               telegraf-configuration-telegraf
                          (default telegraf))

  (user                   telegraf-configuration-user
                          (default "telegraf"))
  ;; influxdb address
  (influxdb-address       telegraf-configuration-influxdb-address
                          (default "http://localhost:8086"))

  ;; influxdb auth token
  (influxdb-token-file    telegraf-configuration-influxdb-token-file)

  (influxdb-bucket        telegraf-configuration-influxdb-bucket)

  (influxdb-orga          telegraf-configuration-influxdb-orga)

  ;; list of config snippets
  (config                 telegraf-configuration-config
                          (default (list %telegraf-default-config))))

(define (telegraf-shepherd-service config)
  "Return a <shepherd-service> for telegraf with CONFIG."


  (define telegraf-output-config
    (string-append  "[[outputs.influxdb_v2]]
  urls = [\"" (telegraf-configuration-influxdb-address config) "\"]
  token = \"$INFLUXDB_TOKEN\"
  organization = \"" (telegraf-configuration-influxdb-orga config) "\"
  bucket = \"" (telegraf-configuration-influxdb-bucket config) "\"

"))

  (define telegraf-config-file
    (computed-file "telegraf.config"
     #~(begin
         (use-modules (ice-9 match))
         (use-modules (guix build utils))
         (call-with-output-file #$output
            (lambda (port)
              (format port "~a" (string-join (append (list #$telegraf-output-config) (list #$@(telegraf-configuration-config config))) "\n"))
              #t)))
     #:options '(#:local-build? #t
                 #:modules ((guix build utils)))))

  (list (shepherd-service
         (documentation "telegraf")
         (requirement '(networking))
         (provision '(telegraf))
         (start #~ (lambda _
                     (let* ((env-vars (list
                                       (string-append
                                        "INFLUXDB_TOKEN="
                                        (call-with-input-file #$(telegraf-configuration-influxdb-token-file config) (lambda (port) (get-string-all port))))
                                       (string-append "PATH=" #$lm-sensors "/bin")))
                            (ctor (make-forkexec-constructor
                                   (list
                                    #$(file-append (telegraf-configuration-telegraf config) "/bin/telegraf") "--config" #$telegraf-config-file)
                                   #:directory "/var/telegraf"
                                   #:user #$(telegraf-configuration-user config)
                                   #:environment-variables env-vars
                                   #:log-file "/var/log/telegraf.log")))
                       (ctor))))
         (stop #~(make-kill-destructor))
         (auto-start? #t)
         (modules `((ice-9 textual-ports))))))

(define* (telegraf-accounts config)
  (let* ((name (telegraf-configuration-user config)))
   (list
     (user-group (name name) (system? #t))
     (user-account
      (name name)
      (group name)
      (system? #t)
      (comment "telegraf user")
      (home-directory "/var/telegraf")
      (shell (file-append shadow "/sbin/nologin"))))))

(define (telegraf-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((username #$(telegraf-configuration-user config))
             (user (getpwnam username)))
        (mkdir-p "/var/telegraf/.cache/snowflake/")
        (chown "/var/telegraf/.cache" (passwd:uid user) (passwd:gid user))
        (chown "/var/telegraf/.cache/snowflake/" (passwd:uid user) (passwd:gid user)))))

(define telegraf-service-type
  (service-type (name 'telegraf)
                (description
                 "runs telegraf")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          telegraf-shepherd-service)
                       (service-extension account-service-type
                                          telegraf-accounts)
                       (service-extension activation-service-type
                                          telegraf-activation)
                       (service-extension setuid-program-service-type
                                          (lambda _
                                            (list (file-like->setuid-program (file-append smartmontools "/sbin/smartctl")))))))))
