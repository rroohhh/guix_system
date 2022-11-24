(define-module (services zfs)
  #:use-module (ice-9 match)
  #:use-module (vup linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages file-systems)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (config linux)
  #:export (zfs-service-type

            zfs-configuration
            zfs-configuration?
            zfs-configuration-kernel
            zfs-configuration-base-zfs
            zfs-configuration-base-zfs-auto-snapshot
            zfs-configuration-dependencies
            zfs-configuration-auto-mount?
            zfs-configuration-auto-scrub
            zfs-configuration-auto-snapshot?
            zfs-configuration-auto-snapshot-keep

            %zfs-zvol-dependency
            zfs-with-vup-kernel))

(define-public zfs-auto-snapshot
  (package
    (name "zfs-auto-snapshot")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/zfsonlinux/zfs-auto-snapshot/archive/upstream/"
               version ".tar.gz"))
        (sha256
          (base32 "16ry1w43i44xc67gr73x6fa48ninfhqxr498ad4m3kya93vp2zrh"))))
    (build-system gnu-build-system)
    (inputs
      ;; Note: if you are inheriting from the above zfs package in order
      ;; to provide a specific stable kernel version, you should also
      ;; inherit this package and replace the sole input below.
      `(("zfs" ,zfs)))
    (arguments
      `(#:tests? #f ; No tests
        #:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          ;; Guix System may not have a traditional cron system, but
          ;; the cron scripts installed by this package are convenient
          ;; to use as targets for an mcron job specification, so make
          ;; sure they can be run in-store.
          (add-before 'install 'fix-scripts
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out                (assoc-ref outputs "out"))
                     (zfs-auto-snapshot  (string-append
                                           out
                                           "/sbin/zfs-auto-snapshot"))
                     (zfs-package        (assoc-ref inputs "zfs"))
                     (zpool              (string-append
                                           zfs-package
                                           "/sbin/zpool"))
                     (zfs                (string-append
                                           zfs-package
                                           "/sbin/zfs")))
                (substitute* '("etc/zfs-auto-snapshot.cron.daily"
                               "etc/zfs-auto-snapshot.cron.frequent"
                               "etc/zfs-auto-snapshot.cron.hourly"
                               "etc/zfs-auto-snapshot.cron.monthly"
                               "etc/zfs-auto-snapshot.cron.weekly")
                  (("zfs-auto-snapshot")
                   zfs-auto-snapshot))
                (substitute* "src/zfs-auto-snapshot.sh"
                  (("LC_ALL=C zfs list")
                   (string-append "LC_ALL=C " zfs " list"))
                  (("LC_ALL=C zpool status")
                   (string-append "LC_ALL=C " zpool " status"))
                  (("zfs snapshot")
                   (string-append zfs " snapshot"))
                  (("zfs destroy")
                   (string-append zfs " destroy"))))))
          ;; Provide DESTDIR and PREFIX on make command.
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (invoke "make" "install"
                        "PREFIX="
                        (string-append "DESTDIR=" out)))
              #t)))))
    (home-page "https://github.com/zfsonlinux/zfs-auto-snapshot")
    (synopsis "Automatically create, rotate, and destroy periodic ZFS snapshots")
    (description
      "An alternative implementation of the zfs-auto-snapshot service for Linux
that is compatible with zfs-linux (now OpenZFS) and zfs-fuse.

On Guix System, you will need to invoke the included shell scripts as @code{job}
definitions in your @code{operating-system} declaration.")
    (license #f)))

(define-record-type* <zfs-configuration>
  zfs-configuration
  make-zfs-configuration
  zfs-configuration?
  ;; linux-libre kernel you want to compile the base-zfs module for.
  (kernel                     zfs-configuration-kernel)
  ;; the OpenZFS package that will be modified to compile for the
  ;; given kernel.
  (base-zfs                   zfs-configuration-base-zfs
                              (default zfs))
  ;; the zfs-auto-snapshot package that will be modified to compile
  ;; for the given kernel.
  (base-zfs-auto-snapshot     zfs-configuration-base-zfs-auto-snapshot
                              (default zfs-auto-snapshot))
  ;; 'weekly for weekly scrubbing, 'monthly for monthly scrubbing, an
  ;; mcron time specification that can be given to `job`, or #f to
  ;; disable.
  (auto-scrub                 zfs-configuration-auto-scrub
                              (default 'weekly))
  ;; association list of symbol-number pairs to indicate the number
  ;; of automatic snapshots to keep for each of 'frequent, 'hourly,
  ;; 'daily, 'weekly, and 'monthly.
  ;; e.g. '((frequent . 8) (hourly . 12))
  (auto-snapshot-keep         zfs-configuration-auto-snapshot-keep
                              (default '())))

(define %default-auto-snapshot-keep
  '((frequent . 4)
    (hourly . 24)
    (daily . 31)
    (weekly . 8)
    (monthly . 12)))

(define %auto-snapshot-mcron-schedule
  '((frequent .  "0,15,30,45 * * * *")
    (hourly .    "0 * * * *")
    (daily .     "0 0 * * *")
    (weekly .    "0 0 * * 7")
    (monthly .   "0 0 1 * *")))

;; A synthetic and unusable MAPPED-DEVICE intended for use when
;; the user has created a mountable filesystem inside a ZFS
;; zvol and wants it mounted inside the configuration.scm.
(define %zfs-zvol-dependency
  (mapped-device
    (source '())
    (targets '("zvol/*"))
    (type #f)))

(define (make-zfs-package conf)
  (let ((kernel    (zfs-configuration-kernel conf))
        (base-zfs  (zfs-configuration-base-zfs conf)))
    (package
      (inherit base-zfs)
      (arguments
        `(#:linux ,kernel
          ,@(package-arguments base-zfs))))))

(define (make-zfs-auto-snapshot-package conf)
  (let ((zfs                    (make-zfs-package conf))
        (base-zfs-auto-snapshot (zfs-configuration-base-zfs-auto-snapshot conf)))
    (package
      (inherit base-zfs-auto-snapshot)
      (inputs `(("zfs" ,zfs))))))

(define (zfs-shepherd-services conf)
  (let* ((zfs-package     (make-zfs-package conf))
         (zpool           (file-append zfs-package "/sbin/zpool"))
         (zfs             (file-append zfs-package "/sbin/zfs"))
         (scheme-modules  `((srfi srfi-1)
                            (srfi srfi-34)
                            (srfi srfi-35)
                            (rnrs io ports)
                            ,@%default-modules)))

      (list (shepherd-service
        (provision '(zfs-mount))
        (requirement `(root-file-system
                       kernel-module-loader
                       udev))
        (documentation "Scans for and imports ZFS pools.")
        (modules scheme-modules)
        (start #~(lambda _
                    (with-output-to-file "/dev/console"
                       (lambda ()
                        (with-input-from-file "/dev/console"
                           (lambda ()
                             (invoke #$zpool "import" "-a" "-N")
                             (usleep 100)
                             (invoke #$zfs "mount" "-a" "-l")))))))
        (stop #~(lambda _
                  (chdir "/")
                  (invoke #$zfs "unmount" "-a" "-f")))))))

(define (zfs-mcron-auto-snapshot-jobs conf)
  (let* ((user-auto-snapshot-keep      (zfs-configuration-auto-snapshot-keep conf))
         (auto-snapshot-keep           (append user-auto-snapshot-keep
                                               %default-auto-snapshot-keep))
         (zfs-auto-snapshot-package    (make-zfs-auto-snapshot-package conf))
         (zfs-auto-snapshot            (file-append zfs-auto-snapshot-package
                                                    "/sbin/zfs-auto-snapshot")))
    (map
      (lambda (label)
        (let ((keep   (assoc-ref auto-snapshot-keep label))
              (sched  (assoc-ref %auto-snapshot-mcron-schedule label)))
          #~(job '#$sched
                 (string-append #$zfs-auto-snapshot
                                " --quiet --syslog "
                                " --label=" #$(symbol->string label)
                                " --keep=" #$(number->string keep)
                                " //"))))
      '(frequent hourly daily weekly monthly))))

(define (zfs-mcron-auto-scrub-jobs conf)
  (let* ((zfs-package    (make-zfs-package conf))
         (zpool          (file-append zfs-package "/sbin/zpool"))
         (auto-scrub     (zfs-configuration-auto-scrub conf))
         (sched          (cond
                           ((eq? auto-scrub 'weekly)  "0 0 * * 7")
                           ((eq? auto-scrub 'monthly) "0 0 1 * *")
                           (else                      auto-scrub))))
    (list
      #~(job '#$sched
             ;; Suppress errors: if there are no ZFS pools, the
             ;; scrub will not be given any arguments, which makes
             ;; it error out.
             (string-append "(" #$zpool " scrub `" #$zpool " list -o name -H` "
                            "> /dev/null 2>&1) "
                            "|| exit 0")))))

(define (zfs-mcron-jobs conf)
  `(,@(zfs-mcron-auto-snapshot-jobs conf)
    ,@(zfs-mcron-auto-scrub-jobs conf)))

(define zfs-service-type
  (service-type
    (name 'zfs)
    (extensions
      (list
        (service-extension kernel-module-loader-service-type
                           (const '("zfs")))
        (service-extension shepherd-root-service-type
                           zfs-shepherd-services)
        (service-extension user-processes-service-type
                           (const '(zfs-mount)))
        (service-extension mcron-service-type
                           zfs-mcron-jobs)
        (service-extension profile-service-type
                           (compose list make-zfs-package))
        (service-extension udev-service-type
                           (compose list make-zfs-package))))
    (description "Installs ZFS, an advanced filesystem and volume manager.")))

(define zfs-with-vup-kernel
  (package
    (inherit zfs)
    (arguments
      `(#:linux ,linux-nonfree/extra_config
                      ,@(package-arguments zfs)))))
