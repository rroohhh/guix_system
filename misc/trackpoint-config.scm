(define-module (misc trackpoint-config)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public trackpoint-udev-config
  (package
    (name "trackpoint-udev-config")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/lib/udev/rules.d/"))
         (with-output-to-file (string-append %output "/lib/udev/rules.d/10-trackpoint.rules")
           (lambda _
             (display "KERNEL==\"serio2\", SUBSYSTEM==\"serio\", DRIVERS==\"psmouse\", ATTR{sensitivity}:=\"255\", ATTR{speed}:=\"255\", ATTR{drift_time}:=\"0\""))))))
    (synopsis "my trackpoint configuration")
    (description "my trackpoint configuration")
    (license license:gpl3)
    (home-page #f)))
