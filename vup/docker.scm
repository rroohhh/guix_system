(define-module (vup docker)
  #:use-module (gnu packages)
  #:use-module (gnu packages docker)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix packages))

(define docker-compose-plugin
  (package
    (inherit docker-compose)
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri
               (let ((arch (match (or (%current-target-system) (%current-system))
                             ("aarch64-linux" "aarch64")
                             ("armhf-linux" "armv7")
                             (_ "x86_64"))))
                 (string-append
                  "https://github.com/docker/compose/releases/download/v"
                  version "/docker-compose-linux-" arch)))
              (sha256
               (base32
                "1qdklhrxm3x7ybhmaycag4q7qqn7snc0yjd1pl5h95fks7hmndcl"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("docker-compose" "libexec/docker/cli-plugins/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (copy-file #$source "./docker-compose")
              (chmod "docker-compose" #o644)))
          (add-before 'install 'chmod
            (lambda _
              (chmod "docker-compose" #o555)))
          (add-after 'install 'setup-bin
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (lib (string-append #$output "/libexec/docker/cli-plugins")))
                (mkdir bin)
                (symlink (string-append lib "/docker-compose")
                         (string-append bin "/docker-compose"))))))))))

(define-public docker-cli-with-docker-compose
  (package
    (inherit docker-cli)
    (arguments
     (substitute-keyword-arguments (package-arguments docker-cli)
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'patch-plugin-path
             (lambda _
               (substitute* "src/github.com/docker/cli/cli-plugins/manager/manager_unix.go"
                 (("/usr/libexec/docker/cli-plugins")
                  (string-append #$output "/libexec/docker/cli-plugins")))))
           (add-after 'install 'symlink-plugin
             (lambda _
               (let ((plugins-directory
                      (string-append #$output "/libexec/docker/cli-plugins")))
                 (mkdir-p plugins-directory)
                 (symlink (string-append #$(this-package-input "docker-compose")
                                         "/libexec/docker/cli-plugins/docker-compose")
                          (string-append plugins-directory "/docker-compose")))))))))
    (inputs (list docker-compose-plugin))))
