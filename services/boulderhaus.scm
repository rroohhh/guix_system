(define-module (services boulderhaus)
  #:use-module (vup python-xyz)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix records)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (bhbooking-configuration bhscraping-configuration))

(define-public boulderhaus-booking-webui
  (package
    (name "boulderhaus-booking-webui")
    (version "0.0.1-03308eb")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rroohhh/boulderhaus_booking")
                    (commit "03308eb72ce6be9845ef91bc2a6c3140354f76bb")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c7115z6k5cmpdv356dwpya3rlry08idzlj33z7v3j5criqs7r6l"))))
    (build-system copy-build-system)
    (synopsis "Boulderhaus Heidelberg booking webui")
    (description "Boulderhaus Heidelberg booking webui")
    (home-page "https://github.com/rroohhh/boulderhaus_booking")
    (license license:agpl3)))

(define-public boulderhaus-booking-server
  (package
    (name "boulderhaus-booking-server")
    (version "0.0.1-4c46c7e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rroohhh/boulderhaus_booking")
                    (commit "4c46c7ef1802dfed9bca6151cdf84eecaffba3f1")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15h12nz8cwm8xmraybvpfaqyssqm39f2y29vbpxrcgp5iwx7hjdq"))))
    (build-system python-build-system)
    (propagated-inputs `(("pyquery" ,python-pyquery)
                         ("flask" ,python-flask)
                         ("requests" ,python-requests)
                         ("influxdb" ,python-influxdb-client)
                         ("tinycss2" ,python-tinycss2)))
    (synopsis "Boulderhaus Heidelberg booking server")
    (description "Boulderhaus Heidelberg booking server")
    (home-page "https://github.com/rroohhh/boulderhaus_booking")
    (license license:agpl3)))

(define-record-type* <bhbooking-configuration>
  bhbooking-configuration make-bhbooking-configuration
  bhbooking-configuration?
  ;; <package>
  (server                 bhbooking-configuration-server
                          (default boulderhaus-booking-server))

  ;; user to run it under
  (user                   bhbooking-configuration-user
                          (default "bhbooking"))
  ;; string
  (work-dir               bhbooking-configuration-work-dir
                          (default "/var/lib/bhbooking")))


(define (bhbooking-shepherd-service config)
  "Return a <shepherd-service> for bhbooking with CONFIG."

  (list (shepherd-service
         (documentation "bhbooking node")
         (requirement '(networking))
         (provision '(bhbooking))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (bhbooking-configuration-server config) "/bin/server.py"))
                   #:log-file "/var/log/bhbooking.log"
                   #:directory #$(bhbooking-configuration-work-dir config)
                   #:user #$(bhbooking-configuration-user config)))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))

(define* (bhbooking-accounts config)
  (list
    (user-group (name (bhbooking-configuration-user config)) (system? #t))
    (user-account
     (name (bhbooking-configuration-user config))
     (group (bhbooking-configuration-user config))
     (system? #t)
     (comment "bhbooking user")
     (home-directory "/var/empty")
     (shell (file-append shadow "/sbin/nologin")))))

(define (bhbooking-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((work-dir #$(bhbooking-configuration-work-dir config))
             (username #$(bhbooking-configuration-user config))
             (user (getpwnam username)))
        (mkdir-p work-dir)
        (chown work-dir (passwd:uid user) (passwd:gid user)))))

(define-public bhbooking-service-type
  (service-type (name 'bhbooking)
                (description
                 "run bhbooking node")
                (default-value (bhbooking-configuration))
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          bhbooking-shepherd-service)
                       (service-extension account-service-type
                                          bhbooking-accounts)
                       (service-extension activation-service-type
                                          bhbooking-activation)))))

(define-record-type* <bhscraping-configuration>
  bhscraping-configuration make-bhscraping-configuration
  bhscraping-configuration?
  ;; <package>
  (server                 bhscraping-configuration-server
                          (default boulderhaus-booking-server))

  ;; user to run it under
  (user                   bhscraping-configuration-user
                          (default "bhscraping"))
  ;; string
  (influxdb-host          bhscraping-configuration-influxdb-host
                          (default "localhost:8086"))

  ;; <path>
  (influxdb-token-file    bhscraping-configuration-influxdb-token-file))


(define (bhscraping-shepherd-service config)
  "Return a <shepherd-service> for bhscraping with CONFIG."

  (list (shepherd-service
         (documentation "boulderhaus scraper node")
         (requirement '(networking))
         (provision '(bhscraping))
         (start #~ (lambda _
                     (let* ((env-vars (list
                                       (string-append
                                        "INFLUXDB_TOKEN="
                                        (call-with-input-file #$(bhscraping-configuration-influxdb-token-file config) (lambda (port) (get-string-all port))))
                                       (string-append "INFLUXDB_HOST=" #$(bhscraping-configuration-influxdb-host config))))
                            (ctor (make-forkexec-constructor
                                   (list #$(file-append (bhscraping-configuration-server config) "/bin/trafficlight.py"))
                                   #:user #$(bhscraping-configuration-user config)
                                   #:environment-variables env-vars
                                   #:log-file "/var/log/bhscraping.log")))
                       (ctor))))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))

(define* (bhscraping-accounts config)
  (list
    (user-group (name (bhscraping-configuration-user config)) (system? #t))
    (user-account
     (name (bhscraping-configuration-user config))
     (group (bhscraping-configuration-user config))
     (system? #t)
     (comment "bhscraping user")
     (home-directory "/var/empty")
     (shell (file-append shadow "/sbin/nologin")))))

(define-public bhscraping-service-type
  (service-type (name 'bhscraping)
                (description
                 "run bhscraping node")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          bhscraping-shepherd-service)
                       (service-extension account-service-type
                                          bhscraping-accounts)))))
