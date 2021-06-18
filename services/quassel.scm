(define-module (services quassel)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages tls)
  #:use-module (gnu system shadow)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (ice-9 match)
  #:export (quassel-configuration
            quassel-service-type))

(define-record-type* <quassel-configuration>
  quassel-configuration make-quassel-configuration
  quassel-configuration?
  (quassel quassel-configuration-quassel
           (default quassel))
  (interface quassel-configuration-interface
             (default "::,0.0.0.0"))
  (port quassel-configuration-port
        (default 4242))
  (user quassel-configuration-user
        (default "quasselcore"))
  (loglevel quassel-configuration-loglevel
            (default "Info")))

(define quassel-shepherd-service
  (match-lambda
    (($ <quassel-configuration> quassel interface port user loglevel)
     (with-imported-modules (source-module-closure
                              '((gnu build shepherd)
                                (gnu system file-systems)))
       (list (shepherd-service
               (provision '(quassel))
               (requirement '(user-processes networking))
               (modules '((gnu build shepherd)
                          (gnu system file-systems)))
               (start #~(make-forkexec-constructor/container
                          (list #$(file-append quassel "/bin/quasselcore")
                                "--configdir=/var/lib/quassel"
                                "--logfile=/var/log/quassel/core.log"
                                (string-append "--loglevel=" #$loglevel)
                                (string-append "--port=" (number->string #$port))
                                (string-append "--listen=" #$interface))
                          #:user #$user
                          #:mappings (list (file-system-mapping
                                            (source "/tmp")
                                            (target source))
                                           (file-system-mapping
                                            (source "/var/lib/quassel")
                                            (target source)
                                            (writable? #t))
                                           (file-system-mapping
                                            (source "/var/log/quassel")
                                            (target source)
                                            (writable? #t)))))
               (stop  #~(make-kill-destructor))))))))

(define (quassel-account config)
  (let* ((name (quassel-configuration-user config)))
    (list (user-group (name name) (system? #t))
          (user-account
             (name name)
             (group name)
             (system? #t)
             (comment "Quassel daemon user")
             (home-directory "/var/lib/quassel")
             (shell (file-append shadow "/sbin/nologin"))))))

(define (quassel-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/quassel")
      (mkdir-p "/var/log/quassel")
      (let ((cert "/var/lib/quassel/quasselCert.pem"))
        (unless (file-exists? cert)
          (invoke #$(file-append openssl "/bin/openssl")
                  "req" "-x509" "-nodes" "-batch" "-days" "680" "-newkey"
                  "rsa" "-keyout" cert "-out" cert)
          (let* ((username #$(quassel-configuration-user config))
                 (user (getpwnam username)))
            (chown cert (passwd:uid user) (passwd:gid user))
            (chown "/var/lib/quassel" (passwd:uid user) (passwd:gid user))
            (chown "/var/log/quassel" (passwd:uid user) (passwd:gid user)))))))

(define quassel-service-type
  (service-type (name 'quassel)
                (extensions
                  (list (service-extension shepherd-root-service-type
                                           quassel-shepherd-service)
                        (service-extension profile-service-type
                                           (compose list quassel-configuration-quassel))
                        (service-extension account-service-type
                                           quassel-account)
                        (service-extension activation-service-type
                                           quassel-activation)))
                (default-value (quassel-configuration))
                (description
                 "Run @url{https://quassel-irc.org/,quasselcore}, the backend
for the distributed IRC client quassel, which allows you to connect from
multiple machines simultaneously.")))
