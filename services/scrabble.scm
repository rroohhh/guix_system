(define-module (services scrabble)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (vup scrabble)
  #:export (scrabble-configuration
            scrabble-service-type))

(define-record-type* <scrabble-configuration>
  scrabble-configuration make-scrabble-configuration
  scrabble-configuration?
  ;; <package>
  (scrabble               scrabble-configuration-scrabble
                          (default scrabble-server))

  ;; string
  (data-dir               scrabble-configuration-data-dir
                          (default "/var/lib/scrabble"))

  ;; string
  (address                scrabble-configuration-address
                          (default "ws://127.0.0.1:48443"))
  ;; string
  (user                   scrabble-configuration-user
                          (default "scrabble-server")))

(define (scrabble-shepherd-service config)
  "Return a <shepherd-service> for scrabble with CONFIG."

  (list (shepherd-service
         (documentation "scrabble server")
         (requirement '(networking))
         (provision '(scrabble-server))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (scrabble-configuration-scrabble config)
                                        "/bin/ScrabbleServer") "-Protocol" "-Logstdout" "-ServerUrl" #$(scrabble-configuration-address config))
                   #:user #$(scrabble-configuration-user config)
                   #:directory #$(scrabble-configuration-data-dir config)
                   #:log-file "/var/log/scrabble-server.log"))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))


(define (scrabble-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((data-dir #$(scrabble-configuration-data-dir config))
             (username #$(scrabble-configuration-user config))
             (user (getpwnam username)))
        (mkdir-p data-dir)
        (chown data-dir (passwd:uid user) (passwd:gid user)))))

(define* (scrabble-accounts config)
  (let* ((name (scrabble-configuration-user config)))
   (list
     (user-group (name name) (system? #t))
     (user-account
      (name name)
      (group name)
      (system? #t)
      (comment "scrabble server user")
      (home-directory "/var/lib/scrabble")
      (shell (file-append shadow "/sbin/nologin"))))))

(define scrabble-service-type
  (service-type (name 'scrabble)
                (description
                 "run scrabble server")
                (default-value (scrabble-configuration))
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          scrabble-shepherd-service)
                       (service-extension activation-service-type
                                          scrabble-activation)
                       (service-extension account-service-type
                                          scrabble-accounts)))))
