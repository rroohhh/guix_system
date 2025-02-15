(define-module (vup rust-import-lock)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (gnu packages web)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix base32)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:export (import-rust-lock))

(define* (toml->json input)
  (computed-file "json"
   #~(begin
       (use-modules (ice-9 popen)
                    (ice-9 textual-ports))
       (let* ((json-output-pipe
               (open-pipe* OPEN_READ
                           #+(file-append yq "/bin/yq") "-p" "toml" "-o" "json" #+input))
            (json-output (get-string-all json-output-pipe)))
       (with-output-to-file
           #$output
         (lambda _ (display json-output)))))))

(define* (import-rust-lock lock-source #:optional extras)
  (define (%numstr->bin str final base)
    (define len (string-length str))
    (let lp((i 0) (ret '()))
      (cond
       ((= i len) (final (reverse! ret)))
       (else (lp (+ i 2) (cons (string->number (substring str i (+ i 2)) base) ret))))))
  (define (hex->bin str)
    (%numstr->bin str u8-list->bytevector 16))
  (let* ((json-lock-gexp (toml->json (file-append lock-source "/Cargo.lock")))
         (json-lock-drv (with-store %store (run-with-store %store (lower-object json-lock-gexp))))
         (json-lock-out (with-store %store (build-derivations %store (list json-lock-drv))))
         (json-lock-path (and json-lock-out (derivation-output-path (assoc-ref (derivation-outputs json-lock-drv) "out"))))
         (packages (assoc-ref (call-with-input-file json-lock-path json->scm) "package")))
    (filter-map
     (lambda (package)
       (let* ((name (assoc-ref package "name"))
              (version (assoc-ref package "version"))
              (checksum (assoc-ref package "checksum"))
              (source (assoc-ref package "source"))
              (checksum-bv (and checksum (hex->bin checksum)))
              (checksum-bv (or checksum-bv (assoc-ref extras name)))
              (source (and source
                           (if (string-prefix? "registry" source)
                               (string-append "https://crates.io/api/v1/crates/" name "/" version "/download")
                               (let* ((split (string-split source #\#))
                                      (rest (car split))
                                      (commit (cadr split))
                                      (base (car (string-split rest #\?)))
                                      (codeload-base (string-replace-substring base "git+https://github" "https://codeload.github")))
                                 (string-append codeload-base "/tar.gz/" commit))))))
         (and source (origin
           (method url-fetch)
           (uri source)
           (file-name
            (string-append name "-" version ".tar.gz"))
           (sha256 checksum-bv)))))
     (vector->list packages))))
