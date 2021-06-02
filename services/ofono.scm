(define-module (services ofono)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (vup misc))

(define-public (ofono-shepherd-service _)
  "Return a shepherd service for ofono"
  (list (shepherd-service
         (documentation "Run ofono")
         (provision '(ofono))
         (requirement
          `(user-processes dbus-system loopback))
         (start #~(make-forkexec-constructor
                   (list (string-append #$ofono
                                        "/sbin/ofonod") "-n")))
         (stop #~(make-kill-destructor)))))


(define-public ofono-service-type
  (let ((ofono-package (lambda (_) (list ofono))))
    (service-type (name 'ofono)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            ofono-shepherd-service)
                         (service-extension dbus-root-service-type
                                            ofono-package)))
                  (default-value '())
                  (description
                   "Run @url{https://01.org/ofono,ofono}"))))
