(define-module (services bme680)
  #:use-module (misc util)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:export (bme680-configuration))

(define bme680-reader
  (package
   (name "bme680-reader")
   (version "0.1.0")
   (source
    (local-file "./bme680" #:recursive? #t))
   (arguments '(#:tests? #f))
   (propagated-inputs (list python-pyserial))
   (native-inputs (list python-setuptools python-wheel))
   (build-system pyproject-build-system)
   (home-page "--")
   (synopsis "card10 bme680 reader")
   (description "card10 bme680 reader")
   (license (list license:gpl2))))

(define bme680-reader-script
  (local-file "bme680/test.py"))

(define bme680-service
  (program-file
   "bme680-service"
   #~(begin
       (use-modules (web client))
       (use-modules (web response))
       (use-modules (ice-9 textual-ports))
       (use-modules (ice-9 popen))
       (use-modules (ice-9 rdelim))

       (let ((port (open-input-pipe (string-append #$(file-append bme680-reader "/bin/pycard10") " " #$bme680-reader-script)))
             (auth (getenv "BME680_WRITER_INFLUX_TOKEN"))
             (host (getenv "BME680_WRITER_INFLUX_HOST")))
         (define (read)
           (let ((line (read-line port)))
             (display "\n")
             (display line)
             (display "\n")
             (let ((res (http-request (string-append host "/api/v2/write?org=infra&bucket=sensors")
                                      #:method 'POST
                                      #:body (string-trim-right line)
                                      #:headers `((Authorization . ,(string-append "Token " auth))))))
               (display res)))
           (read))
         (read)))))


(define-record-type* <bme860-configuration>
  bme680-configuration make-bme680-configuration
  bme680-configuration?
  ;; string
  (influxdb-host          bme680-configuration-influxdb-host
                          (default "localhost:8086"))
  ;; <path>
  (influxdb-token-file    bme680-configuration-influxdb-token-file))


(define bme680-log-file "/var/log/bme680.log")

(define (bme680-shepherd-service config)
  "Return a <shepherd-service> for bme680 with CONFIG."

  (list (shepherd-service
         (documentation "bme680 reader")
         (requirement '(networking))
         (provision '(bme680))
         (modules (append %default-modules (list '(ice-9 textual-ports))))
         (start #~ (lambda _
                     (use-modules (ice-9 textual-ports))
                     (let* ((env-vars (list
                                       (string-append
                                        "BME680_WRITER_INFLUX_TOKEN="
                                        (call-with-input-file #$(bme680-configuration-influxdb-token-file config) get-string-all))
                                       (string-append "BME680_WRITER_INFLUX_HOST=" #$(bme680-configuration-influxdb-host config))))
                            (ctor (make-forkexec-constructor
                                   (list #$bme680-service)
                                   #:environment-variables env-vars
                                   #:log-file #$bme680-log-file)))
                       (ctor))))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))

(define-public bme680-service-type
  (service-type (name 'bme680)
                (description
                 "run bme680 reader")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          bme680-shepherd-service)))))
