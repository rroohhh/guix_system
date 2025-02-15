(define-module (vup xkeylogger)
  #:use-module (gnu)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public xkeylogger
  (package
    (name "xkeylogger")
    (version "0.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rroohhh/xkeylogger")
                    (commit "e0e45faecf6cbbe313c300e6e6d91587e6f865cf")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15243aqin14syvadzgqg1f93c8acg8finj7qrv2w1fjsjrpdd768"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags `("CC=gcc")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check)
                  ;; The upstream makefile does not include an install phase.
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (for-each (lambda (file)
                                    (install-file file bin)
                                    (delete-file file))
                                  '("xkeylogger"))) #t)))))
    (inputs `(("libXi" ,libxi)
              ("libX11" ,libx11)))
    (home-page "https://github.com/rroohhh/xkeylogger")
    (synopsis "Simple keylogger for X.")
    (description "Simple keylogger for X.")
    (license #f)))
