(define-module (services misc)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux))

(define (root-remount-shepherd-service _)
  (list (shepherd-service
         (documentation "remount root to apply /etc/fstab settings")
         (provision '(root-remount))
         (one-shot? #t)
         (start #~(make-forkexec-constructor
                   (list (string-append #$util-linux "/bin/mount") "-o" "remount" "/")))
         (stop #~(make-kill-destructor)))))

(define-public root-remount-service-type
  (service-type (name 'root-remount)
                (description "remount root to apply /etc/fstab settings")
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     root-remount-shepherd-service)))
                (default-value '())))
