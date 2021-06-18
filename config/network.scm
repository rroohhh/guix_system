(define-module (config network)
  #:export (network-config address-of))

(define network-config
  '(("hosts" .
     (("mel" .        (("address" .  "192.168.3.4")
                       ("pubkey" .   "3ekzNiOxSrY8of+jkr7CYaQcUE5/lezB4Et47OQeUjw=")))
      ("seshat" .     (("address" .  "192.168.3.1")
                       ("pubkey" .   "um05F71LGhNOhriCHAmsDXYCKMv4A4R07MLm990mAW4=")
                       ("endpoint" . "coroot.de")))
      ("rofydi" .     (("address" .  "192.168.3.3")
                       ("pubkey" .   "NjW75mxqmPa+pAJlvX/U7r5A2KDeLiqBzaYJQzrzMGM=")))
      ("vup-phone" .  (("address" .  "192.168.3.5")
                       ("pubkey" .   "AwyLbD0GsvjN5mC3dRkPNpDd3ao3a3pcLjAIeXw2flc=")))
      ("ada" .        (("address" .  "192.168.3.6")
                       ("pubkey" .   "KwyLbD0GsvjN5mC3dRkPNpDd3ao3a3pcLjAIeXw2flc=")))))
    ("subnet" .  "24")
    ("net" .    "192.168.3.0")
    ("root" .   "seshat")))

(define* (address-of target-host host)
  (let* ((is-localhost? (string=? target-host host))
         (target-address (assoc-ref (assoc-ref (assoc-ref network-config "hosts") target-host) "address")))
    (if is-localhost?
        "127.0.0.1"
        target-address)))
