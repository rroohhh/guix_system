(define-module (misc vault)
  #:use-module (web client)
  #:use-module (json)
  #:use-module (json parser)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (scheme base)
  #:use-module (guix records)
  #:use-module (gnutls)
  #:export (make-vault-connection-configuration
            vault-connection-configuration
            vault-auth-token-lookup-self
            vault-seal
            vault-unseal
            vault-list-mounts
            vault-enable-mount
            vault-delete-mount
            vault-list-policies
            vault-read-policy
            vault-set-policy
            vault-delete-policy
            vault-list-auth
            vault-enable-auth
            vault-disable-auth
            vault-list-approles
            vault-set-approle
            vault-get-approle
            vault-get-approle-role-id
            vault-delete-approle
            vault-generate-secret-id))

(define-record-type* <vault-connection-configuration>
  vault-connection-configuration make-vault-connection-configuration
  vault-connection-configuration?
  (address               vault-connection-address
                         (default "https://127.0.0.1:8200"))
  (cert-dir              vault-connection-cert-dir
                         (default "/etc/ssl/certs"))
  (auth-token-file       vault-connection-auth-token-file))

(define json 'json)
(define <=> '<=>)

(define-syntax-rule (define-json-reader json->record ctor spec ...)
  (define (json->record input)
    (let ((table (cond ((port? input)
                        (json->scm input))
                       ((string? input)
                        (json-string->scm input))
                       ((or (null? input) (pair? input))
                        input))))
      (let-syntax ((extract-field (syntax-rules (json)
                                    ((_ table (field (json key json->value _ (... ...)) _ (... ...)))
                                     (json->value (assoc-ref table key)))
                                    ((_ table (field (json key) _ (... ...)))
                                     (assoc-ref table key))
                                    ((_ table (field _ (... ...)))
                                     (assoc-ref table (symbol->string 'field))))))
        (ctor (extract-field table spec) ...)))))

(define-syntax-rule (define-json-writer record->json
                      (field-spec ...) ...)
  (define (record->json record)
    (let-syntax ((field->alist (syntax-rules (json)
                                 ((_ field getter
                                     (json key json->value value->json)
                                     _ (... ...))
                                  (cons key
                                        (value->json (getter record))))
                                 ((_ field getter
                                     (json key _ (... ...))
                                     _ (... ...))
                                  (cons key
                                        (getter record)))
                                 ((_ field getter _ (... ...))
                                  (cons (symbol->string 'field)
                                        (getter record))))))
      (list (field->alist field-spec ...) ...))))

(define-syntax define-json-mapping
  (syntax-rules (<=>)
    ((_ rtd ctor ctor-proc
        pred json->record <=> record->json
        (field getter spec ...) ...)
     (begin
       (define-json-mapping rtd ctor ctor-proc
         pred json->record
         (field getter spec ...) ...)

       (define-json-writer record->json
         (field getter spec ...) ...)))
    ((_ rtd ctor ctor-proc pred json->record
        (field getter spec ...) ...)
     (begin
       (define-record-type* rtd ctor ctor-proc
         pred
         (field getter spec ...) ...)

       (define-json-reader json->record ctor-proc
         (field spec ...) ...)))))

(define-json-mapping <token-info> token-info make-token-info token-info?
  json->token-info <=> token-info->json
  (accessor          token-accessor)
  (type              token-type)
  (creation-time     token-creation-time
                     (json "creation_time"))
  (creation-ttl      token-creation-ttl
                     (json "creation_ttle"))
  (display-name      token-display-name
                     (json "display_name"))
  (entity-id         token-entity-id
                     (json "entity_id"))
  (expire-time       token-expire-time
                     (json "expire_time"))
  (explicit-max-ttl  token-explicit-max-ttl
                     (json "explicit_max_ttl"))
  (id                token-explicit-id)
  (identity-policies token-identity-policies
                     (json "identity_policies"))
  (issue-time        token-issue-time
                     (json "issue_time"))
  (meta              token-meta)
  (num-uses          token-num-uses
                     (json "num_uses"))
  (orphan?           token-orphan?
                     (json "orphan"))
  (path              token-path)
  (renewable?        token-renewable?
                     (json "renewable"))
  (policies          token-policies)
  (ttl               token-ttl))

(define* (vault-api-call config path method output #:optional (body #f))
  (parameterize ((x509-certificate-directory (vault-connection-cert-dir config)))
    (receive (response body)
        (http-request (string-append (vault-connection-address config) "/v1" path)
                  #:body body
                  #:method method
                  #:headers `((X-Vault-Token . ,(call-with-input-file (vault-connection-auth-token-file config) (lambda (port) (read-line port))))))
      (begin
        (if body
            (match (json->scm (open-input-bytevector body))
              ((("errors" . error))
               (begin
                 (unless (eq? (vector-length error) 0)
                   (raise-exception
                    (format #f "vault error ~a" error)))))
              (no-error (output no-error)))
            (output body))))))

(define* (parse-field field-name #:optional (parser identity))
  (lambda* (data)
           (let* ((field-value (assoc field-name data)))
             (if field-value
                 (parser (cdr field-value))
                 (raise-exception
                   (format #f "vault error: field not found ~a in ~a" field-name data))))))

(define* (vault-auth-token-lookup-self config)
  (vault-api-call config "/auth/token/lookup-self" 'GET (parse-field "data" json->token-info)))

(define* (vault-unseal config key)
  (vault-api-call config "/sys/unseal" 'PUT identity (format #f "{ \"key\": \"~a\"}" key)))

(define* (vault-seal config key)
  (vault-api-call config "/sys/seal" 'PUT identity))

(define* (vault-list-mounts config)
  (vault-api-call config "/sys/mounts" 'GET identity))

(define* (vault-enable-mount conn path config)
  (vault-api-call conn (string-append "/sys/mounts/" path) 'POST identity (scm->json-string config)))

(define* (vault-delete-mount conn path)
  (vault-api-call conn (string-append "/sys/mounts/" path) 'DELETE identity))

(define* (vault-list-policies conn)
  (vault-api-call conn "/sys/policy" 'LIST (parse-field "data" identity)))

(define* (vault-read-policy conn name)
  (vault-api-call conn (string-append "/sys/policy/" name) 'GET (parse-field "data" identity)))

(define* (vault-set-policy conn name rules)
  (vault-api-call conn (string-append "/sys/policy/" name) 'PUT identity (scm->json-string `(("policy" . ,rules)))))

(define* (vault-delete-policy conn name)
  (vault-api-call conn (string-append "/sys/policy/" name) 'DELETE identity))

(define* (vault-list-auth conn)
  (vault-api-call conn "/sys/auth" 'GET (parse-field "data" identity)))

(define* (vault-enable-auth conn type #:key (path #f) (config 'null))
  (let ((path (if path path type)))
    (vault-api-call conn (string-append "/sys/auth/" path) 'POST identity (scm->json-string `(("type" . ,type) ("config" . ,config))))))

(define* (vault-disable-auth conn path)
  (vault-api-call conn (string-append "/sys/auth/" path) 'DELETE identity))

(define* (vault-list-approles conn #:key (path "approle"))
  (vault-api-call conn (string-append "/auth/" path "/role") 'LIST (parse-field "data" identity)))

(define* (vault-set-approle conn name policies #:key (path "approle"))
  (vault-api-call conn (string-append "/auth/" path "/role/" name) 'POST identity (scm->json-string `(("token_policies" . ,policies)))))

(define* (vault-get-approle conn name #:key (path "approle"))
  (vault-api-call conn (string-append "/auth/" path "/role/" name) 'GET (parse-field "data" identity)))

(define* (vault-get-approle-role-id conn name #:key (path "approle"))
  (vault-api-call conn (string-append "/auth/" path "/role/" name "/role-id") 'GET (parse-field "data" (parse-field "role_id"))))

(define* (vault-delete-approle conn name #:key (path "approle"))
  (vault-api-call conn (string-append "/auth/" path "/role/" name) 'DELETE identity))

(define* (vault-generate-secret-id conn name #:key (path "approle"))
  (vault-api-call conn (string-append "/auth/" path "/role/" name "/secret-id") 'POST (parse-field "data")))
