(define-module (misc ftdi)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ftdi-udev-config
  (package
    (name "ftdi-udev-config")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/lib/udev/rules.d/"))
         (with-output-to-file (string-append %output "/lib/udev/rules.d/70-ftdi-key.rules")
           (lambda _
             (display "KERNEL==\"*\", SUBSYSTEM==\"*\", ATTRS{idVendor}==\"0403\", ATTRS{idProduct}==\"6001\", TAG+=\"uaccess\""))))))
    (synopsis "ftdi udev rules")
    (description "ftdi udev rules")
    (license license:gpl3)
    (home-page #f)))
