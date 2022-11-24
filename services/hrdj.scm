(define-module (services hrdj)
  #:use-module (guix gexp)
  #:use-module (gnu services))

(define %hrdj-device-config
  `("modprobe.d/hrdj.conf"
    ,(plain-file "hrdj.conf"
                 "blacklist snd-usb-audio")))

(define-public hrdj-device-service-type
  (service-type
    (name 'hrdj)
    (default-value '())
    (description "hrdj modprobe config")
    (extensions
      (list (service-extension etc-service-type
                               (const (list %hrdj-device-config)))))))

