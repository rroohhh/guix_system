(define-module (services networking)
  #:use-module (gnu services)
  #:use-module (gnu services vpn)
  #:export (networking-for))

(define* (networking-for host config)
  (let* ((host-config  (assoc-ref (assoc-ref config "hosts") host))
         (host-address (assoc-ref host-config "address"))
         (root         (assoc-ref config "root"))
         (root?        (string=? root host))
         (subnet       (assoc-ref config "subnet"))
         (net          (assoc-ref config "net"))
         (all-peers    (map
                        (lambda (host)
                          (wireguard-peer
                           (name        (car host))
                           (keep-alive  "10")
                           (public-key  (assoc-ref (cdr host) "pubkey"))
                           (allowed-ips (if root?
                                            (list (string-append (assoc-ref (cdr host) "address") "/32"))
                                            (list (string-append net "/" subnet))))
                                            
                           (endpoint    (if (assoc-ref (cdr host) "endpoint")
                                            (string-append (assoc-ref (cdr host) "endpoint") ":51820")
                                            #f))))
                        (assoc-ref config "hosts")))
         (peers        (if root?
                           (filter
                            (lambda (peer) (not (string=? (wireguard-peer-name peer) root)))
                            all-peers)
                           (filter
                            (lambda (peer) (string=? (wireguard-peer-name peer) root))
                            all-peers))))
    (list (service
           wireguard-service-type
           (wireguard-configuration
            (addresses (list (string-append host-address "/" subnet)))
            (peers peers))))))
