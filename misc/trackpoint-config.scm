(define-module (misc trackpoint-config)
  #:use-module (guix gexp)
  #:use-module (gnu services)
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

(define %libinput-trackpoint-quirk-config
  `("libinput/local-overrides.quirks"
    ,(plain-file "local-overrides.quirks"
                 "[Lenovo P14s Gen 1 AMD Trackpoint]
MatchUdevType=pointingstick
MatchName=*TPPS/2 Elan TrackPoint*
MatchDMIModalias=dmi:*svnLENOVO:*:pvrThinkPadP14sGen1*
AttrTrackpointMultiplier=1.0")))

(define-public trackpoint-quirk-service-type
  (service-type
    (name 'trackpoint-quirk)
    (default-value '())
    (description "trackpoint quirk override")
    (extensions
      (list (service-extension etc-service-type
                               (const (list %libinput-trackpoint-quirk-config)))))))
