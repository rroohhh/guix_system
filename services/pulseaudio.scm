(define-module (services pulseaudio)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages pulseaudio))


(define-public (pulseaudio-shepherd-service _)
  "Return a shepherd service for pulseaudio"
  (list (shepherd-service
         (documentation "Run pulseaudio")
         (provision '(pulseaudio))
         (requirement
          `(user-processes dbus-system loopback))
         (start #~(make-forkexec-constructor
                   (list (string-append #$pulseaudio
                                        "/bin/pulseaudio") "--system")))
         (stop #~(make-kill-destructor)))))


(define-public pulseaudio-service-type
  (let ((pulseaudio-package (lambda (_) (list pulseaudio))))
    (service-type (name 'pulseaudio)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            pulseaudio-shepherd-service)
                         (service-extension dbus-root-service-type
                                            pulseaudio-package)))
                  (default-value '())
                  (description
                   "Run systemwide pulseaudio"))))
