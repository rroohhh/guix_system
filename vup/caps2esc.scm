(define-module (vup caps2esc)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public caps2esc
  (package
   (name "caps2esc")
   (version "1.0.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/rroohhh/caps2esc")
           (commit "3fd30002f8d3900d8e68441f7228769347972231")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "17d1dzbwg6xd8km7j3xifa0jsf5n2kd4qm1s4rry7gpmnr4g7rzx"))))
   (build-system gnu-build-system)
   (arguments '(#:phases
                (modify-phases %standard-phases
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
                                  '("caps2esc")))
                      #t)))))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs `(("libevdev" ,libevdev)
             ("libudev" ,eudev)))
   (home-page "https://github.com/rroohhh/caps2esc")
   (synopsis "Transforming the most useless key ever in the most useful one")
   (description "Making caps work as esc and space as shift")
   (license gpl3)))

(define (caps2esc-shepherd-service _)
  (list (shepherd-service
         (documentation "Making caps work as esc and space as shift")
         (provision '(caps2esc))
         (start #~(make-forkexec-constructor
                   (list (string-append #$caps2esc "/bin/caps2esc"))))
         (stop #~(make-kill-destructor)))))

(define-public caps2esc-service-type
  (service-type (name 'caps2esc)
                (description "Making caps work as esc and space as shift")
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     caps2esc-shepherd-service)))
                (default-value '())))
