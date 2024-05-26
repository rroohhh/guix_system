(define-module (services zigbee2mqtt)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (vup zigbee2mqtt)
  #:use-module (srfi srfi-1)
  #:export (zigbee2mqtt-configuration
            zigbee2mqtt-service-type))

;; This sucks a little, because we cannot add hardcoded config, as zigbee2mqtt needs it writable
;; We could copy some default thing in the activation, but lets just keep this for now and make `/var/lib' part of the backup strategy
;; as it is already for various other stuff
(define-record-type* <zigbee2mqtt-configuration>
  zigbee2mqtt-configuration make-zigbee2mqtt-configuration
  zigbee2mqtt-configuration?
  ;; <package>
  (zigbee2mqtt                  zigbee2mqtt-configuration-zigbee2mqtt
                              (default zigbee2mqtt))
  ;; string
  (user                   zigbee2mqtt-configuration-user
                          (default "zigbee2mqtt")))

(define zigbee2mqtt-log-file "/var/log/zigbee2mqtt.log")
(define zigbee2mqtt-data-dir "/var/lib/zigbee2mqtt")

(define (zigbee2mqtt-shepherd-service config)
  "Return a <shepherd-service> for zigbee2mqtt with CONFIG."
  (list (shepherd-service
         (documentation "zigbee2mqtt server")
         (requirement '(mqtt))
         (provision '(zigbee2mqtt))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (zigbee2mqtt-configuration-zigbee2mqtt config)
                                        "/bin/zigbee2mqtt"))
                   #:user #$(zigbee2mqtt-configuration-user config)
                   #:group "dialout"
                   #:environment-variables (list (string-append "ZIGBEE2MQTT_DATA=" #$zigbee2mqtt-data-dir))
                   #:log-file #$zigbee2mqtt-log-file))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))


(define* (zigbee2mqtt-accounts config)
  (let* ((name (zigbee2mqtt-configuration-user config)))
    (list
     (user-group (name name) (system? #t))
     (user-account
      (name name)
      (group name)
      (supplementary-groups '("dialout"))
      (system? #t)
      (comment "zigbee2mqtt server user")
      (home-directory "/var/empty")
      (shell (file-append shadow "/sbin/nologin"))))))

(define (zigbee2mqtt-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let ((state-dir #$zigbee2mqtt-data-dir)
            (user (getpwnam #$(zigbee2mqtt-configuration-user config))))
        (mkdir-p state-dir)
        (chown state-dir (passwd:uid user) (passwd:gid user)))))

(define* (zigbee2mqtt-log-rotate _)
  (list
    (log-rotation
     (files (list zigbee2mqtt-log-file)))))

(define zigbee2mqtt-service-type
  (service-type
   (name 'zigbee2mqtt)
   (description
    "run zigbee2mqtt server")
   (default-value (zigbee2mqtt-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             zigbee2mqtt-shepherd-service)
          (service-extension account-service-type
                             zigbee2mqtt-accounts)
          (service-extension activation-service-type
                             zigbee2mqtt-activation)
          (service-extension rottlog-service-type
                             zigbee2mqtt-log-rotate)))))
