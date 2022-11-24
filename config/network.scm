(define-module (config network)
  #:export (network-config address-of))

(define network-config
  '(("hosts" .
     (("mel" .         (("address" .  "192.168.3.4")
                        ("pubkey" .   "3ekzNiOxSrY8of+jkr7CYaQcUE5/lezB4Et47OQeUjw=")))
      ("seshat" .      (("address" .  "192.168.3.1")
                        ("pubkey" .   "um05F71LGhNOhriCHAmsDXYCKMv4A4R07MLm990mAW4=")
                        ("endpoint" . "coroot.de")))
      ("rofydi" .      (("address" .  "192.168.3.3")
                        ("pubkey" .   "NjW75mxqmPa+pAJlvX/U7r5A2KDeLiqBzaYJQzrzMGM=")))
      ("vup-phone" .   (("address" .  "192.168.3.5")
                        ("pubkey" .   "AwyLbD0GsvjN5mC3dRkPNpDd3ao3a3pcLjAIeXw2flc=")))
      ("ada" .         (("address" .  "192.168.3.6")
                        ("pubkey" .   "dfPEs++UVK85VMqwINC0LuKAC2c+dxWWcoVJIIDl1GQ=")))
      ("anuejn-beta" . (("address" .  "192.168.3.7")
                        ("pubkey" .   "JeW8amcDjrw0UAkm3YDoJQWoIm02CfW1B6Q7IW3RRTg=")))
      ("sebastian-beta" . (("address" .  "192.168.3.8")
                           ("pubkey" .   "yL/u9QETmjUExuTiXvGX3CtBgofIvQS/L2FO6eS0rlY=")))
      ("yogimaus" . (("address" .  "192.168.3.9")
                     ("pubkey" .   "N+D+KH5fdSn/vJvamtriRV68kHm5p8+AO8+GGnrZnhU=")))
      ("jaro_laptop" . (("address" .  "192.168.3.10")
                     ("pubkey" .   "b7grVmMjZmNB03JQhqFM8pOcx9Wqp+2lHVt8pZZV4XI=")))))
    ("subnet" .  "24")
    ("net" .    "192.168.3.0")
    ("root" .   "seshat")))

(define* (address-of target-host host)
  (let* ((is-localhost? (string=? target-host host))
         (target-address (assoc-ref (assoc-ref (assoc-ref network-config "hosts") target-host) "address")))
    (if is-localhost?
        "127.0.0.1"
        target-address)))
