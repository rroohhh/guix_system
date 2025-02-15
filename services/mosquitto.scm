(define-module (services mosquitto)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages messaging)
  #:use-module (srfi srfi-1)
  #:export (mosquitto-configuration
            mosquitto-service-type))

(define-record-type* <mosquitto-configuration>
  mosquitto-configuration make-mosquitto-configuration
  mosquitto-configuration?
  ;; <package>
  (mosquitto                  mosquitto-configuration-mosquitto
                              (default mosquitto))
  ;; string
  (user                   mosquitto-configuration-user
                          (default "mosquitto"))
  ;; config
  (config          mosquitto-configuration-config))

(define (mosquitto-shepherd-service config)
  "Return a <shepherd-service> for mosquitto with CONFIG."

  (list (shepherd-service
         (documentation "mosquitto mqtt server")
         (requirement '(networking))
         (provision '(mosquitto mqtt))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (mosquitto-configuration-mosquitto config)
                                        "/sbin/mosquitto") "-c" #$(mosquitto-configuration-config config))
                   #:user #$(mosquitto-configuration-user config)
                   #:log-file "/var/log/mosquitto.log"))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))


(define* (mosquitto-accounts config)
  (let* ((name (mosquitto-configuration-user config)))
    (list
     (user-group (name name) (system? #t))
     (user-account
      (name name)
      (group name)
      (system? #t)
      (comment "mosquitto server user")
      (home-directory "/var/empty")
      (shell (file-append shadow "/sbin/nologin"))))))

(define mosquitto-service-type
  (service-type
   (name 'mosquitto)
   (description
    "run mosquitto server")
   (extensions
    (list (service-extension shepherd-root-service-type
                             mosquitto-shepherd-service)
          (service-extension account-service-type
                             mosquitto-accounts)))))
