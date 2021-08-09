(define-module (config seshat)
  #:use-module (config base)
  #:use-module (config network)
  #:use-module (config network-utils)
  #:use-module (services influxdb)
  #:use-module (services scrabble)
  #:use-module (services caddy)
  #:use-module (services quassel)
  #:use-module (services boulderhaus)
  #:use-module (services rtsp-simple-server)
  #:use-module (gnu)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services networking)
  #:use-module (gnu services databases)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (guix utils)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
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
         (simple-configs '(("influx.<host>" "mel" "8086")
                           ("ci.<host>" "mel" "8080")
                           ("stream.<host>" "seshat" "8888")
                           ("files.<host>" "mel" "8000")
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
                ,(string-append "vault.<host> {
reverse_proxy * https://" (address-of "mel" #$host-name) ":8200 {
  transport http {
    tls_insecure_skip_verify
  }
}
}") ;; scrabble
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

(define extra-users
  `(("anuejn" "users" ("wheel" "netdev" "audio" "video" "kvm")"data/anuejn.pub")
    ("tpw_rules" "users" () "data/tpw_rules.pub" ,(file-append shadow "/sbin/nologin"))))

(define rtsp-simple-server-config
  "###############################################
# General parameters

# sets the verbosity of the program; available values are 'warn', 'info', 'debug'.
logLevel: debug
# destinations of log messages; available values are 'stdout', 'file' and 'syslog'.
logDestinations: [stdout]
# if 'file' is in logDestinations, this is the file which will receive the logs.
logFile: rtsp-simple-server.log

# timeout of read operations.
readTimeout: 10s
# timeout of write operations.
writeTimeout: 10s
# number of read buffers.
# a higher number allows a higher throughput,
# a lower number allows to save RAM.
readBufferCount: 512

# enable Prometheus-compatible metrics.
metrics: yes
# address of the metrics listener.
metricsAddress: :9998

# enable pprof-compatible endpoint to monitor performances.
pprof: no
# address of the pprof listener.
pprofAddress: :9999

# command to run when a client connects to the server.
# this is terminated with SIGINT when a client disconnects from the server.
# the server port is available in the RTSP_PORT variable.
runOnConnect:
# the restart parameter allows to restart the command if it exits suddenly.
runOnConnectRestart: no

###############################################
# RTSP parameters

# disable support for the RTSP protocol.
rtspDisable: no
# supported RTSP stream protocols.
# UDP is the most performant, but can cause problems if there's a NAT between
# server and clients, and doesn't support encryption.
# UDP-multicast allows to save bandwidth when clients are all in the same LAN.
# TCP is the most versatile, and does support encryption.
# The handshake is always performed with TCP.
protocols: [udp, multicast, tcp]
# encrypt handshake and TCP streams with TLS (RTSPS).
# available values are 'no', 'strict', 'optional'.
encryption: no
# address of the TCP/RTSP listener. This is needed only when encryption is 'no' or 'optional'.
rtspAddress: :8554
# address of the TCP/TLS/RTSPS listener. This is needed only when encryption is 'strict' or 'optional'.
rtspsAddress: :8555
# address of the UDP/RTP listener. This is needed only when 'udp' is in protocols.
rtpAddress: :8000
# address of the UDP/RTCP listener. This is needed only when 'udp' is in protocols.
rtcpAddress: :8001
# IP range of all UDP-multicast listeners. This is needed only when 'multicast' is in protocols.
multicastIPRange: 224.1.0.0/16
# port of all UDP-multicast/RTP listeners. This is needed only when 'multicast' is in protocols.
multicastRTPPort: 8002
# port of all UDP-multicast/RTCP listeners. This is needed only when 'multicast' is in protocols.
multicastRTCPPort: 8003
# path to the server key. This is needed only when encryption is 'strict' or 'optional'.
# this can be generated with:
# openssl genrsa -out server.key 2048
# openssl req -new -x509 -sha256 -key server.key -out server.crt -days 3650
serverKey: server.key
# path to the server certificate. This is needed only when encryption is 'strict' or 'optional'.
serverCert: server.crt
# authentication methods.
authMethods: [basic, digest]
# read buffer size.
# this doesn't influence throughput and shouldn't be touched unless the server
# reports errors about the buffer size.
readBufferSize: 2048

###############################################
# RTMP parameters

# disable support for the RTMP protocol.
rtmpDisable: no
# address of the RTMP listener.
rtmpAddress: :1935

###############################################
# HLS parameters

# disable support for the HLS protocol.
hlsDisable: no
# address of the HLS listener.
hlsAddress: :8888
# number of HLS segments to generate.
# increasing segments allows more buffering,
# decreasing segments decrease latency.
hlsSegmentCount: 1
# minimum duration of each segment.
# the real segment duration is also influenced by the interval between IDR frames,
# since the server changes the segment duration to include at least one IDR frame in each.
hlsSegmentDuration: 0.3s
# value of the Access-Control-Allow-Origin header provided in every HTTP response.
# This allows to play the HLS stream from an external website.
hlsAllowOrigin: '*'

###############################################
# Path parameters

# these settings are path-dependent.
# it's possible to use regular expressions by using a tilde as prefix.
# for example, '~^(test1|test2)$' will match both 'test1' and 'test2'.)
# for example, '~^prefix' will match all paths that start with 'prefix'.
# the settings under the path 'all' are applied to all paths that do not match
# another entry.
paths:
  all:
    # source of the stream - this can be:
    # * record -> the stream is published by a RTSP or RTMP client
    # * rtsp://existing-url -> the stream is pulled from another RTSP server
    # * rtsps://existing-url -> the stream is pulled from another RTSP server, with RTSPS
    # * rtmp://existing-url -> the stream is pulled from a RTMP server
    # * redirect -> the stream is provided by another path or server
    source: record

    # if the source is an RTSP or RTSPS URL, this is the protocol that will be used to
    # pull the stream. available values are 'automatic', 'udp', 'multicast', 'tcp'.
    # the TCP protocol can help to overcome the error 'no UDP packets received recently'.
    sourceProtocol: automatic

    # if the source is an RTSP or RTSPS URL, this allows to support cameras that
    # don't provide server ports. This is a security issue and must be enabled only
    # when interacting with old cameras that require it.
    sourceAnyPortEnable: no

    # if the source is an RTSPS URL, the fingerprint of the certificate of the source
    # must be provided in order to prevent man-in-the-middle attacks.
    # it can be obtained from the source by running:
    # openssl s_client -connect source_ip:source_port </dev/null 2>/dev/null | sed -n '/BEGIN/,/END/p' > server.crt
    # openssl x509 -in server.crt -noout -fingerprint -sha256 | cut -d '=' -f2 | tr -d ':'
    sourceFingerprint:

    # if the source is an RTSP or RTMP URL, it will be pulled only when at least
    # one reader is connected, saving bandwidth.
    sourceOnDemand: no
    # if sourceOnDemand is 'yes', readers will be put on hold until the source is
    # ready or until this amount of time has passed.
    sourceOnDemandStartTimeout: 10s
    # if sourceOnDemand is 'yes', the source will be closed when there are no
    # readers connected and this amount of time has passed.
    sourceOnDemandCloseAfter: 10s

    # if the source is 'redirect', this is the RTSP URL which clients will be
    # redirected to.
    sourceRedirect:

    # if the source is 'record' and a client is publishing, do not allow another
    # client to disconnect the former and publish in its place.
    disablePublisherOverride: no

    # if the source is 'record' and no one is publishing, redirect readers to this
    # path. It can be can be a relative path  (i.e. /otherstream) or an absolute RTSP URL.
    fallback:

    # username required to publish.
    # sha256-hashed values can be inserted with the 'sha256:' prefix.
    publishUser:
    # password required to publish.
    # sha256-hashed values can be inserted with the 'sha256:' prefix.
    publishPass:
    # ips or networks (x.x.x.x/24) allowed to publish.
    publishIps: []

    # username required to read.
    # sha256-hashed values can be inserted with the 'sha256:' prefix.
    readUser:
    # password required to read.
    # sha256-hashed values can be inserted with the 'sha256:' prefix.
    readPass:
    # ips or networks (x.x.x.x/24) allowed to read.
    readIps: []

    # command to run when this path is initialized.
    # this can be used to publish a stream and keep it always opened.
    # this is terminated with SIGINT when the program closes.
    # the path name is available in the RTSP_PATH variable.
    # the server port is available in the RTSP_PORT variable.
    runOnInit:
    # the restart parameter allows to restart the command if it exits suddenly.
    runOnInitRestart: no

    # command to run when this path is requested.
    # this can be used to publish a stream on demand.
    # this is terminated with SIGINT when the path is not requested anymore.
    # the path name is available in the RTSP_PATH variable.
    # the server port is available in the RTSP_PORT variable.
    runOnDemand:
    # the restart parameter allows to restart the command if it exits suddenly.
    runOnDemandRestart: no
    # readers will be put on hold until the runOnDemand command starts publishing
    # or until this amount of time has passed.
    runOnDemandStartTimeout: 10s
    # the runOnDemand command will be closed when there are no
    # readers connected and this amount of time has passed.
    runOnDemandCloseAfter: 10s

    # command to run when a client starts publishing.
    # this is terminated with SIGINT when a client stops publishing.
    # the path name is available in the RTSP_PATH variable.
    # the server port is available in the RTSP_PORT variable.
    runOnPublish:
    # the restart parameter allows to restart the command if it exits suddenly.
    runOnPublishRestart: no

    # command to run when a clients starts reading.
    # this is terminated with SIGINT when a client stops reading.
    # the path name is available in the RTSP_PATH variable.
    # the server port is available in the RTSP_PORT variable.
    runOnRead:
    # the restart parameter allows to restart the command if it exits suddenly.
    runOnReadRestart: no
")

(define-public seshat-system-config
  (operating-system
   (inherit base-system-config)

   (hosts-file etc-hosts-file)

   (users (append (map (lambda (user)
                         (user-account
                          (name (car user))
                          (group (cadr user))
                          (supplementary-groups (caddr user))
                          (shell (if (eq? (length user) 5)
                                     (last user)
                                     (file-append bash "/bin/bash")))))
                       extra-users)
                  (operating-system-users base-system-config)))

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
               ,(service rtsp-simple-server-service-type
                         (rtsp-simple-server-configuration
                          (config rtsp-simple-server-config)))
               ,(service bhbooking-service-type)
               ,(service bhscraping-service-type
                         (bhscraping-configuration
                          (influxdb-host (string-append "http://" (address-of "mel" host-name) ":8086"))
                          (influxdb-token-file "/secrets/bhscraping_influxdb_token")))
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
                               (append
                                (map (lambda (user)
                                       `(,(car user) ,(local-file (cadddr user)))) extra-users)
                                (map
                                 (lambda (key-config)
                                   (if (string=? (car key-config) "root")
                                       (append key-config `(,(local-file "../data/mel-robin.pub")))
                                       key-config))
                                 ssh-default-authorized-keys))))))))))
                                 
(list (machine
       (operating-system seshat-system-config)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (host-name "coroot.de")
         (system "x86_64-linux")
         (identity "/home/robin/.ssh/id_ed25519")
         (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHDeKweSXGxPW8MQvynT2tN19M5ttMDPiGeGGg4Cbic2")))))
