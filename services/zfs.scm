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
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages llvm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix modules)
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

(define-public zfs-2.2
  (package
   (inherit zfs)
   (version "2.2.4")
   (source
    (origin
     (patches (list "zfs_no_init_conf.patch"))
     (method url-fetch)
     (uri (string-append "https://github.com/openzfs/zfs/releases"
                         "/download/zfs-" version
                         "/zfs-" version ".tar.gz"))
     (sha256
      (base32 "1h0yqchirzsn2gll1w2gclb13hr8511z67lf85cigm43frgr144p"))))

   (native-inputs (list autoconf automake libtool pkg-config clang-17 lld-17))
   (arguments
    (substitute-keyword-arguments
      (package-arguments zfs)
           ;; Source patching phases are broken up into discrete steps to allow
           ;; future versions to discard individual phases without having to
           ;; discard all source patching.
      ((#:phases phases)
       #~(modify-phases #$phases
           ;; (add-after 'unpack 'set-llvm
           ;;   (lambda *_
           ;;     (setenv "LLVM" "1")
           ;;     (setenv "CC" "clang")))
           (replace 'really-configure
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "configure"
                (("-/bin/sh") (string-append "-" (which "sh"))))
              (setenv "KBUILD_LDFLAGS=" "--thinlto-cache-dir=/tmp/.thinlto-cache")
              (invoke "./configure"
                      "KERNEL_LLVM=1"
                      "KERNEL_CC=clang"
                      "--with-config=all"
                      (string-append "--prefix=" #$output)
                      (string-append "--with-dracutdir=" #$output
                                     "/lib/dracut")
                      (string-append "--with-udevdir=" #$output
                                     "/lib/udev")
                      (string-append "--with-mounthelperdir=" #$output
                                     "/sbin")
                      (string-append "--with-linux="
                                     (search-input-directory
                                      inputs
                                      "lib/modules/build")))))
           (add-after 'patch-source 'reconf
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke #$(file-append autoconf "/bin/autoreconf") "-vfi")))
           (replace 'patch-source
             (lambda* (#:key inputs #:allow-other-keys)
               ;; New feature "compatibility=" in 2.1.0.
               ;; This feature looks up in two locations:
               ;;   /etc/zfs/compatibility.d/
               ;;   /usr/share/zfs/compatibility.d/
               ;; The first is intended for system-specific compatibility
               ;; sets, while the second is what is installed with the
               ;; OpenZFS package, so use the absolute path for the first
               ;; (which requires patching in the file) and the store path
               ;; for the second (which it gets by default).
               (substitute* "include/sys/fs/zfs.h"
                                              (("#define\tZPOOL_SYSCONF_COMPAT_D.*$")
                                               ;; Use absolute path.
                                               "#define\tZPOOL_SYSCONF_COMPAT_D\t\"/etc/zfs/compatibility.d\"\n"))
               ;; Also update the manual, which uses absolute paths, so that
               ;; /usr/share/zfs/compatibility.d/ is referred via the store.
               (substitute* '("man/man7/zpoolprops.7"
                              "man/man7/zpool-features.7")
                  (("/usr/share/zfs/compatibility.d")
                   (string-append #$output "/share/zfs/compatibility.d")))
               (substitute* "contrib/Makefile.am"
                 ;; This is not configurable nor is its hard-coded /usr prefix.
                 ((".*initramfs.*") ""))
               (substitute* "lib/libzfs/os/linux/libzfs_util_os.c"
                   ;; Use path to /gnu/store/*-kmod in actual path that is
                   ;; exec'ed.
                   (("\"/sbin/modprobe\"")
                    (string-append "\""
                                   (search-input-file inputs "/bin/modprobe")
                                   "\""))
                                              ;; Just use 'modprobe' in message to user, since Guix
                                              ;; does not have a traditional /sbin/
                   (("'/sbin/modprobe ") "'modprobe "))
               (substitute* "module/os/linux/zfs/zfs_ctldir.c"
                    (("/usr/bin/env\", \"umount")
                     (string-append (search-input-file inputs "/bin/umount")
                                    "\", \"-n"))
                    (("/usr/bin/env\", \"mount")
                     (string-append (search-input-file inputs "/bin/mount")
                                    "\", \"-n")))
               (substitute* "lib/libzfs/os/linux/libzfs_mount_os.c"
                    (("/bin/mount") (search-input-file inputs "/bin/mount"))
                    (("/bin/umount") (search-input-file inputs "/bin/umount")))
               (substitute* "lib/libshare/os/linux/nfs.c"
                    (("/usr/sbin/exportfs")
                     (search-input-file inputs "/sbin/exportfs")))
               (substitute* "config/zfs-build.m4"
                    (("\\$sysconfdir/init.d")
                     (string-append #$output "/etc/init.d"))
                    (("/etc/bash_completion.d")
                     (string-append #$output "/etc/bash_completi.on.d")))
               (substitute* '("etc/Makefile.am"
                              "cmd/zpool/Makefile.am"
                              "cmd/zed/zed.d/Makefile.am"
                              "scripts/Makefile.am")
                    (("\\$\\(sysconfdir)") (string-append #$output "/etc")))
               (substitute* "udev/vdev_id"
                    (("PATH=/bin:/sbin:/usr/bin:/usr/sbin")
                     (string-append "PATH="
                                    (dirname (which "chmod")) ":"
                                    (dirname (which "grep")) ":"
                                    (dirname (which "sed")) ":"
                                    (dirname (which "gawk")))))
               (substitute* "contrib/pyzfs/Makefile.am"
                    ((".*install-lib.*") ""))
               (substitute* '("Makefile.am" "Makefile.in")
                    (("\\$\\(prefix)/src") (string-append #$output:src "/src")))
               (substitute* (find-files "udev/rules.d/" ".rules.in$")
                    (("/sbin/modprobe")
                     (search-input-file inputs "/bin/modprobe")))))))))))
                       

(define-record-type* <zfs-configuration>
  zfs-configuration
  make-zfs-configuration
  zfs-configuration?
  ;; linux-libre kernel you want to compile the base-zfs module for.
  (kernel                     zfs-configuration-kernel
                              (default linux-for-modules))
  ;; the OpenZFS package that will be modified to compile for the
  ;; given kernel.
  (base-zfs                   zfs-configuration-base-zfs
                              (default zfs-2.2))
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
                            (gnu build file-systems)
                            ,@%default-modules)))

      (list (shepherd-service
             (provision '(zfs-mount))
             (requirement `(root-file-system
                            kernel-module-loader
                            udev))
             (documentation "Scans for and imports ZFS pools.")
             (modules scheme-modules)
             (start
              (with-imported-modules (source-module-closure
                                      '((gnu build file-systems)))
                #~(lambda _
                    (invoke #$zpool "import" "-a" "-N")
                    (system*/tty #$zfs "mount" "-a" "-l"))))
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
    (inherit zfs-2.2)
    (arguments
      `(#:linux ,linux-nonfree/extra_config-x86
        ,@(package-arguments zfs-2.2)))))
