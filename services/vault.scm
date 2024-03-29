(define-module (services vault)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages tls)
  #:use-module (vup go-xyz)
  #:use-module (misc vault)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (services secrets)
  #:export (vault-configuration
            vault-service-type
            vault-unseal-configuration
            vault-unseal-service-type
            vault-cert-dir
            vault-generated-secret))

(define-record-type* <vault-configuration>
  vault-configuration make-vault-configuration
  vault-configuration?
  ;; <package>
  (vault                  vault-configuration-vault
                          (default vault))

  ;; string
  (data-dir               vault-configuration-data-dir
                          (default "/var/lib/vault"))

  ;; string
  (address                vault-configuration-address
                          (default "127.0.0.1:8200"))

  ;; ui
  (ui?                    vault-configuration-ui?
                          (default #f))

  ;; telemetry provider
  (telemetry              vault-configuration-telemetry
                          (default #f))

  (unauthenticated-metrics-access? vault-configuration-unauthenticated-metrics-access?
                          (default #f))

  ;; path
  (tls-key-file           vault-configuration-tls-key-file)

  ;; path
  (tls-cert-file          vault-configuration-tls-cert-file))

(define (vault-shepherd-service config)
  "Return a <shepherd-service> for vault with CONFIG."

  (define vault-config-file
    (computed-file
     "configuration.hcl"
     #~(begin
         (use-modules (ice-9 match))
         (use-modules (guix build utils))
         (call-with-output-file #$output
            (lambda (port)
              (display "# Generated by 'vault-service'.\n" port)
              (format port "ui = ~a\n" #$(if (vault-configuration-ui? config) "true" "false"))
              (format port "storage \"file\" {\n")
              (format port (string-append "    path = \"" #$(vault-configuration-data-dir config) "\"\n"))
              (format port "}\n")
              (format port "listener \"tcp\" {\n")
              (format port (string-append "    address = \"" #$(vault-configuration-address config) "\"\n"))
              (format port (string-append "    tls_key_file = \"" #$(vault-configuration-tls-key-file config) "\"\n"))
              (format port (string-append "    tls_cert_file = \"" #$(vault-configuration-tls-cert-file config) "\"\n"))
              (format port "telemetry {\n")
              (format port "      unauthenticated_metrics_access = ~a\n" (if #$(vault-configuration-unauthenticated-metrics-access? config) "true" "false"))
              ;; TODO(robin): is this a guix bug???
              (let ((telemetry '#$(vault-configuration-telemetry config)))
                (when telemetry
                 (format port (string-append "      " (car telemetry) " = \"" (cadr telemetry) "\"\n"))
                 (format port "      dogstatsd_tags = [\"vault\"]\n")))
              (format port "  }\n")
              (format port "}\n")
              (format port "telemetry {\n")
              (format port "}\n")
              #t)))
     #:options '(#:local-build? #t
                 #:modules ((guix build utils)))))

  (list (shepherd-service
         (documentation "vault server")
         (requirement '(networking))
         (provision '(vault))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (vault-configuration-vault config)
                                        "/bin/vault") "server" (string-append "-config=" #$vault-config-file))
                   #:log-file "/var/log/vault-server.log"))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))


(define (vault-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((data-dir #$(vault-configuration-data-dir config))
             (username "root")
             (user (getpwnam username)))
        (mkdir-p data-dir)
        (chown data-dir (passwd:uid user) (passwd:gid user)))))

(define vault-service-type
  (service-type (name 'vault)
                (description
                 "run vault server")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          vault-shepherd-service)
                       (service-extension activation-service-type
                                          vault-activation)))))

(define-record-type* <vault-unseal-configuration>
  vault-unseal-configuration make-vault-unseal-configuration
  vault-unseal-configuration?
  ;; <vault-connection-configuration>
  (vault-connection       vault-unseal-configuration-vault-connection)

  ;; file
  (unsealing-keys-file    vault-unseal-configuration-unsealing-keys-file))

(define (vault-unseal-shepherd-service config)
  "Return a <shepherd-service> for unsealing vault with CONFIG."
    (with-extensions (list guile-gnutls ;; (module-ref (resolve-interface '(gnu packages tls)) 'gnutls)
                           (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))
     (with-imported-modules (source-module-closure '((misc vault)) #:select? (lambda* (name) (if (guix-module-name? name) #t (match name (('gnutls) #t) (('json _) #t) (('misc _) #t) (_ #f)))))
         (define start
            #~(lambda args
                         (let* ((keys-file (open-input-file #$(vault-unseal-configuration-unsealing-keys-file config)))
                                (vault-connection #$(vault-unseal-configuration-vault-connection config)))
                           (vault-unseal vault-connection (read-line keys-file))
                           (vault-unseal vault-connection (read-line keys-file))
                           (vault-unseal vault-connection (read-line keys-file)))))
         (define stop
           #~(lambda args
                          (vault-seal #$(vault-unseal-configuration-vault-connection config))))
         (list (shepherd-service
                (documentation "vault unsealing service")
                (requirement '(vault))
                (provision '(vault-unseal))
                (start start)
                (stop stop)
                (modules `((misc vault) (ice-9 rdelim) (gnutls)
                           ,@%default-modules)))))))

(define vault-unseal-service-type
  (service-type (name 'vault-unseal)
                (description
                 "unseal the given vault server with the given file containing the sealing keys")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          vault-unseal-shepherd-service)))))

(define* (vault-generated-secret secret generator #:optional (extra-requirement '()))
  (service
   generated-secret-service-type
   (generated-secret-generator
    (target secret)
    (generator
     (with-extensions
      (list guile-gnutls ;; (module-ref (resolve-interface '(gnu packages tls)) 'gnutls)
            (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))
      (with-imported-modules
       (source-module-closure
        '((misc vault))
        #:select? (lambda* (name)
                    (if
                     (guix-module-name? name)
                     #t
                     (match name
                       (('gnutls) #t)
                       (('json _) #t)
                       (('json) #t)
                       (('misc _) #t)
                       (_ #f)))))
       #~(lambda () (#$generator)))))
    (requirement (append '(networking) extra-requirement))
    (modules `((misc vault) (ice-9 rdelim) (gnutls)
               ,@%default-modules)))))

(define* (vault-cert-dir cert-file)
  (let ((cert (get-string-all (open-input-file cert-file))))
    (computed-file
     "vault-cert"
     #~(begin
         (use-modules (guix build utils))
         (mkdir-p #$output)
         (with-output-to-port (open-output-file (string-append #$output "/cert.pem"))
           (lambda () (display #$cert))))
     #:options
     '(#:local-build? #t
       #:modules ((guix build utils))))))
