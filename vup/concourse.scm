(define-module (vup concourse)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define concourse-version "7.9.1")

(define-public concourse
  (package
   (name "concourse")
   (version concourse-version)
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/concourse/concourse/releases/download/v" version "/concourse-" version "-linux-amd64.tgz"))
     (sha256
      (base32 "0kpc4hkz64pikdjfds0fi15s0zj36y6b4g55nxs4pdm0lmhww5ys"))))
   (build-system copy-build-system)
   (synopsis "Concourse is an open-source continuous thing-doer.")
   (description "Concourse is an open-source continuous thing-doer.")
   (home-page "https://concourse-ci.org/")
   (license license:asl2.0)))

(define-public fly
  (package
    (name "fly")
    (version concourse-version)
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/concourse/concourse/releases/download/v" version "/fly-" version "-linux-amd64.tgz"))
       (sha256
        (base32 "1dsf8d3khvjlqmh6riz50q1kd3g23g0385dskhrq1abqvvyarxvc"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("fly" "bin/fly"))))
    (synopsis "A command line interface that runs a build in a container with ATC.")
    (description "A command line interface that runs a build in a container with ATC.")
    (home-page "https://concourse-ci.org/")
    (license license:asl2.0)))
