(define-module (services rtsp-simple-server)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (vup go-xyz)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (rtsp-simple-server-configuration
            rtsp-simple-server-service-type))

(define-record-type* <rtsp-simple-server-configuration>
  rtsp-simple-server-configuration make-rtsp-simple-server-configuration
  rtsp-simple-server-configuration?
  ;; <package>
  (rtsp-simple-server     rtsp-simple-server-configuration-rtsp-simple-server
                          (default rtsp-simple-server))

  ;; string
  (data-dir               rtsp-simple-server-configuration-data-dir
                          (default "/var/lib/rtsp-simple-server"))

  ;; string
  (user                   rtsp-simple-server-configuration-user
                          (default "rtsp-simple-server"))

  ;; string
  (config                 rtsp-simple-server-configuration-config))

(define (rtsp-simple-server-shepherd-service config)
  "Return a <shepherd-service> for rtsp-simple-server with CONFIG."

  (list (shepherd-service
         (documentation "rtsp-simple-server server")
         (requirement '(networking))
         (provision '(rtsp-simple-server))
         (start #~(lambda args
                    (let* ((data-dir #$(rtsp-simple-server-configuration-data-dir config))
                           (config-file (string-append data-dir "/config.yml"))
                           (ctor (make-forkexec-constructor
                                  (list #$(file-append (rtsp-simple-server-configuration-rtsp-simple-server config)
                                                       "/bin/rtsp-simple-server") config-file)
                                  #:log-file "/var/log/rtsp-simple-server.log"
                                  #:user #$(rtsp-simple-server-configuration-user config))))
                      (begin
                        (with-output-to-file (string-append config-file)
                          (lambda _
                            (display #$(rtsp-simple-server-configuration-config config))))
                        (ctor)))))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))


(define (rtsp-simple-server-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((data-dir #$(rtsp-simple-server-configuration-data-dir config))
             (username #$(rtsp-simple-server-configuration-user config))
             (user (getpwnam username)))
        (mkdir-p data-dir)
        (chown data-dir (passwd:uid user) (passwd:gid user)))))

(define* (rtsp-simple-server-accounts config)
  (list
    (user-group (name (rtsp-simple-server-configuration-user config)) (system? #t))
    (user-account
     (name (rtsp-simple-server-configuration-user config))
     (group (rtsp-simple-server-configuration-user config))
     (system? #t)
     (comment "rtsp-simple-server node user")
     (home-directory "/var/empty")
     (shell (file-append shadow "/sbin/nologin")))))

(define rtsp-simple-server-service-type
  (service-type (name 'rtsp-simple-server)
                (description
                 "run rtsp-simple-server server")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          rtsp-simple-server-shepherd-service)
                       (service-extension account-service-type
                                          rtsp-simple-server-accounts)
                       (service-extension activation-service-type
                                          rtsp-simple-server-activation)))))
