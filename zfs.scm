(define-module (zfs)
  #:use-module (ice-9 match)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
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

            %zfs-zvol-dependency))

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

(define (file-system->shepherd-service-name file-system)
  "Return the symbol that denotes the service mounting and unmounting
FILE-SYSTEM."
  (symbol-append 'file-system-
                 (string->symbol (file-system-mount-point file-system))))

(define (mapped-device->shepherd-service-name md)
  "Return the symbol that denotes the shepherd service of MD, a <mapped-device>."
  (symbol-append 'device-mapping-
                 (string->symbol (string-join
                                  (mapped-device-targets md) "-"))))

(define dependency->shepherd-service-name
  (match-lambda
    ((? mapped-device? md)
     (mapped-device->shepherd-service-name md))
    ((? file-system? fs)
     (file-system->shepherd-service-name fs))))


(define-record-type* <zfs-configuration>
  zfs-configuration
  make-zfs-configuration
  zfs-configuration?
  ;; ; linux-libre kernel you want to compile the base-zfs module for.
  (kernel                     zfs-configuration-kernel)
  ;; the OpenZFS package that will be modified to compile for the
  ;; given kernel.
  (base-zfs                   zfs-configuration-base-zfs
                              (default zfs))
  ;; the zfs-auto-snapshot package that will be modified to compile
  ;; for the given kernel.
  (base-zfs-auto-snapshot     zfs-configuration-base-zfs-auto-snapshot
                              (default zfs-auto-snapshot))
  ;; list of <mapped-device> or <file-system> objects that must be
  ;; opened/mounted before we import any ZFS pools.
  (dependencies               zfs-configuration-dependencies
                              (default '()))
  ;; #t if mountable datasets are to be mounted automatically.
  ;; #f if not mounting.
  ;; #t is the expected behavior on other operating systems, the
  ;; #f is only supported for "rescue" operating systems where
  ;; the user wants lower-level control of when to mount.
  (auto-mount?                zfs-configuration-auto-mount?
                              (default #t))
  ;; 'weekly for weekly scrubbing, 'monthly for monthly scrubbing, an
  ;; mcron time specification that can be given to `job`, or #f to
  ;; disable.
  (auto-scrub                 zfs-configuration-auto-scrub
                              (default 'weekly))
  ;; #t if auto-snapshot is default (and `com.sun:auto-snapshot=false`
  ;; disables auto-snapshot per dataset), #f if no auto-snapshotting
  ;; is default (and `com.sun:auto-snapshot=true` enables auto-snapshot
  ;; per dataset).
  (auto-snapshot?             zfs-configuration-auto-snapshot?
                              (default #t))
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
      (arguments (cons* #:linux kernel
                        (package-arguments base-zfs))))))

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
         (zvol_wait       (file-append zfs-package "/bin/zvol_wait"))
         (scheme-modules  `((srfi srfi-1)
                            (srfi srfi-34)
                            (srfi srfi-35)
                            (rnrs io ports)
                            ,@%default-modules)))
    (define zfs-scan
      (shepherd-service
        (provision '(zfs-scan))
        (requirement `(root-file-system
                       kernel-module-loader
                       udev
                       ,@(map dependency->shepherd-service-name
                              (zfs-configuration-dependencies conf))))
        (documentation "Scans for and imports ZFS pools.")
        (modules scheme-modules)
        (start #~(lambda _
                   (guard (c ((message-condition? c)
                              (format (current-error-port)
                                      "zfs: error importing pools: ~s~%"
                                      (condition-message c))
                              #f))
                     ; TODO: optionally use a cachefile.
                     (invoke #$zpool "import" "-a" "-N"))))
        ;; Why not one-shot?  Because we don't really want to rescan
        ;; this each time a requiring process is restarted, as scanning
        ;; can take a long time and a lot of I/O.
        (stop #~(const #f))))

    (define device-mapping-zvol/*
      (shepherd-service
        (provision '(device-mapping-zvol/*))
        (requirement '(zfs-scan))
        (documentation "Waits for all ZFS ZVOLs to be opened.")
        (modules scheme-modules)
        (start #~(lambda _
                   (guard (c ((message-condition? c)
                              (format (current-error-port)
                                      "zfs: error opening zvols: ~s~%"
                                      (condition-message c))
                              #f))
                     (invoke #$zvol_wait))))
        (stop #~(const #f))))

    (define zfs-auto-mount
      (shepherd-service
        (provision '(zfs-auto-mount))
        (requirement '(zfs-scan))
        (documentation "Mounts all non-legacy mounted ZFS filesystems.")
        (modules scheme-modules)
        (start #~(lambda _
                   (guard (c ((message-condition? c)
                              (format (current-error-port)
                                      "zfs: error mounting file systems: ~s~%"
                                      (condition-message c))
                              #f))
                     ;; Output to current-error-port, otherwise the
                     ;; user will not see any prompts for passwords
                     ;; of encrypted datasets.
                     ;; XXX Maybe better to explicitly open /dev/console ?
                     (with-output-to-port (current-error-port)
                       (lambda ()
                         (invoke #$zfs "mount" "-a" "-l"))))))
        (stop #~(lambda _
                  ;; Make sure that Shepherd does not have a CWD that
                  ;; is a mounted ZFS filesystem, which would prevent
                  ;; unmounting.
                  (chdir "/")
                  (invoke #$zfs "unmount" "-a" "-f")))))

    `(,zfs-scan
      ,device-mapping-zvol/*
      ,@(if (zfs-configuration-auto-mount? conf)
            `(,zfs-auto-mount)
            '()))))

(define (zfs-user-processes conf)
  (if (zfs-configuration-auto-mount? conf)
      '(zfs-auto-mount)
      '(zfs-scan)))

(define (zfs-mcron-auto-snapshot-jobs conf)
  (let* ((user-auto-snapshot-keep      (zfs-configuration-auto-snapshot-keep conf))
         ;; assoc-ref has earlier entries overriding later ones.
         (auto-snapshot-keep           (append user-auto-snapshot-keep
                                               %default-auto-snapshot-keep))
         (auto-snapshot?               (zfs-configuration-auto-snapshot? conf))
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
  (append (zfs-mcron-auto-snapshot-jobs conf)
          (if (zfs-configuration-auto-scrub conf)
              (zfs-mcron-auto-scrub-jobs conf)
              '())))

(define zfs-service-type
  (service-type
    (name 'zfs)
    (extensions
      (list 
        ;; Load it.
        (service-extension kernel-module-loader-service-type
                           (const '("zfs")))
        ;; Make sure ZFS pools and datasets are mounted at
        ;; boot.
        (service-extension shepherd-root-service-type
                           zfs-shepherd-services)
        ;; Make sure user-processes don't start until
        ;; after ZFS does.
        (service-extension user-processes-service-type
                           zfs-user-processes)
        ;; Install automated scrubbing and snapshotting.
        (service-extension mcron-service-type
                           zfs-mcron-jobs)
        ;; Install ZFS management commands in the system
        ;; profile.
        (service-extension profile-service-type
                           (compose list make-zfs-package))
        ;; Install ZFS udev rules.
        (service-extension udev-service-type
                           (compose list make-zfs-package))))
    (description "Installs ZFS, an advanced filesystem and volume manager.")))
