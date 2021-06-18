
(define-module (config network-utils)
  #:use-module (config network)
  #:use-module ((services networking) #:prefix svc:)
  #:export (networking-for))

(define* (networking-for host)
  (svc:networking-for host network-config))
