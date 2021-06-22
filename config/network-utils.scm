
(define-module (config network-utils)
  #:use-module (config network)
  #:use-module (guix gexp)
  #:use-module ((services networking) #:prefix svc:)
  #:export (networking-for etc-hosts-file))

(define* (networking-for host)
  (svc:networking-for host network-config))

(define etc-hosts-file
  (let* ((hosts (assoc-ref network-config "hosts")))
    (plain-file "hosts"
     (string-join
      (append
       (map
        (lambda (host)
          (string-append (assoc-ref (cdr host) "address") " " (car host)))
        hosts)
       (list
        "127.0.0.1 localhost"
        "::1 localhost"))
      "\n"))))
