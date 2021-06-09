(define-module (services secrets)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:export (secret
            generated-secrets-root-service-type
            generated-secret-service-type
            generated-secret-generator
            with-generated-secrets
            define-secret))
            

(define-record-type* <generated-secret>
  generated-secret make-generated-secret
  generated-secret?
  (user                   generated-secret-user)
  (name                   generated-secret-name))

(define* (secret name #:key user)
  (generated-secret
   (user user)
   (name name)))

(define* (secret->path secret)
  (let* ((user (generated-secret-user secret))
         (name (generated-secret-name secret)))
    (format #f "/run/secrets/~a/~a" user name)))

(define* (secret->shepherd-dep secret)
  (symbol-append
   'generated-secret
   ':
   (string->symbol (generated-secret-user secret))
   '@
   (generated-secret-name secret)))

(define-record-type* <generated-secrets-configuraton>
  generated-secrets-configuration make-generated-secrets-configuration
  generated-secrets-configuration?
  ;; <generated-secret-generator>
  (secrets                   generated-secrets-configuration-secrets
                             (default '())))

(define-record-type* <generated-secret-generator
  generated-secret-generator make-generated-secret-generator
  generated-secret-generator?
  ;; <generated-secret>
  (target                      generated-secret-generator-target)
  ;; () -> (), outputs the secret
  (generator                   generated-secret-generator-generator)
  ;; requirements to add to the shepherd service
  (requirement                 generated-secret-generator-requirement
                               (default '()))
  ;; modules to add to the shepherd service
  (modules                     generated-secret-generator-modules
                               (default '())))

(define* (with-generated-secrets secrets ctor)
  (let* ((secret-files
          (map
           (lambda (secret)
             `(,(generated-secret-name secret) . ,(secret->path secret)))
           secrets))
         (secret-deps (map secret->shepherd-dep secrets))
         (orig-service (ctor secret-files))
         (orig-service-type (service-kind orig-service))
         (orig-service-value (service-value orig-service))
         (orig-service-extensions (service-type-extensions orig-service-type))
         (mapped-service-value (if (generated-secret-generator? orig-service-value)
                                   (generated-secret-generator
                                     (inherit orig-service-value)
                                     (requirement (append secret-deps (generated-secret-generator-requirement orig-service-value))))
                                   orig-service-value))
         (mapped-service-extensions
          (map (lambda (extension)
                 (let* ((extension-type (service-extension-target extension))
                        (compute (service-extension-compute extension)))
                   (if (eq? 'shepherd-root (service-type-name extension-type))
                       (service-extension
                        shepherd-root-service-type
                        (lambda (config)
                          (let* ((orig-config (compute config)))
                            (map (lambda (config)
                                   (shepherd-service
                                    (inherit config)
                                    (requirement (append secret-deps (shepherd-service-requirement config)))))
                                 orig-config))))
                       extension)))
               orig-service-extensions)))
    (service
     (service-type
      (inherit orig-service-type)
      (extensions mapped-service-extensions))
     mapped-service-value)))

(define (generated-secrets-shepherd-service config)
  "Return a <shepherd-service> for generating secrets according to CONFIG."
  (map
   (lambda (generator)
     (let* ((target      (generated-secret-generator-target generator))
            (requirement (generated-secret-generator-requirement generator))
            (modules     (generated-secret-generator-modules generator))
            (generator   (generated-secret-generator-generator generator))
            (target-user (generated-secret-user target))
            (user        #~(getpwnam #$target-user))
            (target-name (generated-secret-name target))
            (target-file (secret->path target))
            (target-dep  (secret->shepherd-dep target)))
       (shepherd-service
        (documentation (format #f "generator for secret ~a:~a" target-user target-name))
        (requirement (append '(file-system-/run/secrets) requirement))
        (provision `(,target-dep))
        (modules modules)
        (one-shot? #t)
        (start
         #~(lambda _
             (mkdir-p (dirname #$target-file))
             (call-with-output-file #$target-file
               (lambda (port)
                 (display (#$generator) port)))
             (chown (dirname #$target-file) (passwd:uid #$user) (passwd:gid #$user))
             (chown #$target-file (passwd:uid #$user) (passwd:gid #$user))
             (chmod (dirname #$target-file) #o400)
             (chmod #$target-file #o400))))))
   (car (generated-secrets-configuration-secrets config))))

(define generated-secrets-root-service-type
  (service-type
   (name 'generated-secrets-root)
   (compose concatenate)
   (extend (lambda (config extra-secrets)
            (generated-secrets-configuration
             (inherit config)
             (secrets
              (append
               (generated-secrets-configuration-secrets config)
               (list extra-secrets))))))

   (extensions (list (service-extension shepherd-root-service-type
                                        generated-secrets-shepherd-service)
                     (service-extension file-system-service-type
                                        (lambda _
                                         (list
                                          (file-system
                                           (device "none")
                                           (mount-point "/run/secrets")
                                           (type "tmpfs")
                                           (check? #f)
                                           (create-mount-point? #t)))))))

   (default-value (generated-secrets-configuration))
   (description "Generates secrets stored to /run/secrets/{user}/{secretname}")))

(define generated-secret-service-type
  (service-type
   (name 'generated-secrets)
   (extensions (list (service-extension generated-secrets-root-service-type (compose list identity))))
   (compose concatenate)
   (description "Generates a secret")))

(define-syntax define-secret
  (syntax-rules ()
    ((_ name user) (define name (secret 'name #:user user)))))
