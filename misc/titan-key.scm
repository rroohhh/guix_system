(define-module (misc titan-key)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public titan-key-udev-config
  (package
    (name "titan-key-udev-config")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/lib/udev/rules.d/"))
         (with-output-to-file (string-append %output "/lib/udev/rules.d/70-titan-key.rules")
           (lambda _
             (display "KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", ATTRS{idVendor}==\"18d1|096e\", ATTRS{idProduct}==\"5026|0858|085b\", TAG+=\"uaccess\""))))))
    (synopsis "titan key udev rules")
    (description "titan key udev rules")
    (license license:gpl3)
    (home-page #f)))
