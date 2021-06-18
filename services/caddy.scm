(define-module (services caddy)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (vup caddy)
  #:use-module (srfi srfi-1)
  #:export (caddy-configuration
            caddy-service-type))

(define-record-type* <caddy-configuration>
  caddy-configuration make-caddy-configuration
  caddy-configuration?
  ;; <package>
  (caddy                  caddy-configuration-caddy
                          (default caddy))

  ;; string
  (user                   caddy-configuration-user
                          (default "caddy"))

  ;; config
  (config-blocks          caddy-configuration-config-blocks
                          (default '())))

(define (caddy-shepherd-service config)
  "Return a <shepherd-service> for caddy with CONFIG."

  (define caddy-config-file
    (computed-file "caddy.conf"
     #~(begin
         (use-modules (ice-9 match))
         (use-modules (guix build utils))
         (call-with-output-file #$output
           (lambda (port)
             (format port "~a" (string-join (list #$@(caddy-configuration-config-blocks config)) "\n"))))
         (invoke #$(file-append caddy "/bin/caddy") "fmt" "-overwrite" #$output)
         #t)
     #:options '(#:local-build? #t
                 #:modules ((guix build utils)))))

  (list (shepherd-service
         (documentation "caddy server")
         (requirement '(networking))
         (provision '(caddy))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (caddy-configuration-caddy config)
                                        "/bin/caddy") "run" "-adapter" "caddyfile" "-config" #$caddy-config-file)
                   ;; #:user #$(caddy-configuration-user config) ;; not possible at the moment, setcap is not supported / hard to do
                   #:log-file "/var/log/caddy.log"))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))


(define* (caddy-accounts config)
  (let* ((name (caddy-configuration-user config)))
   (list
     (user-group (name name) (system? #t))
     (user-account
      (name name)
      (group name)
      (system? #t)
      (comment "caddy server user")
      (home-directory "/var/empty")
      (shell (file-append shadow "/sbin/nologin"))))))

(define caddy-service-type
  (service-type (name 'caddy)
                (description
                 "run caddy server")
                (default-value (caddy-configuration))
                (compose concatenate)
                (extend (lambda (config blocks)
                          (caddy-configuration
                           (inherit config)
                           (config-blocks
                            (append (caddy-configuration-config-blocks config)
                                    blocks)))))
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          caddy-shepherd-service)
                       (service-extension account-service-type
                                          caddy-accounts)))))
