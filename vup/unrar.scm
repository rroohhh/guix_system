(define-module (vup unrar))
(use-modules (guix packages))
(use-modules (guix build-system gnu))
(use-modules (guix download))
(use-modules ((guix licenses) #:prefix license:))

(define-public unrar
  (package
    (name "unrar")
    (version "6.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.rarlab.com/rar/unrarsrc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "185cqhw5frkia22gb8hi2779n7zlkhrw3sm698q9x7w75lwm7vqx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "DESTDIR=" %output))
       #:phases (alist-delete 'configure
                              (alist-delete 'check %standard-phases))))
    (inputs `())
    (home-page "http://www.rarlab.com/rar_add.htm/")
    (synopsis "unrar")
    (description "unrar, non free.")
    (license license:x11)))
