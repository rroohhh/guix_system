(define-module (vup ip_addr)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public ip_addr
  (package
   (name "ip_addr")
   (version "0.0.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/rroohhh/ip_addr")
           (commit "c29ab92379b21ddb56c45aaccda8a212f5e5f8a4")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1rnddcxfn0ss9nrqkayqjxk2hmfnqk5s2vkrg107mwim89h67vj9"))))
   (build-system gnu-build-system)
   (arguments '(#:phases
                (modify-phases %standard-phases
                  (delete 'configure) 
                  (delete 'check) 
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (for-each (lambda (file)
                                    (install-file file bin)
                                    (delete-file file))
                                  '("ip_addr")))
                      #t)))))
   (home-page "https://github.com/rroohhh/ip_addr")
   (synopsis "Minimal ip address and wifi ssid tool")
   (description "Minimal ip address and wifi ssid tool")
   (license agpl3)))
