(define-module (services btrbk)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (gnu packages backup)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (vup misc)
  #:export (btrbk-configuration
            btrbk-service-type))



(define-record-type* <btrbk-configuration>
  btrbk-configuration make-btrbk-configuration
  btrbk-configuration?
  ;; <package>
  (btrbk                 btrbk-configuration-btrbk
                         (default btrbk))
  ;; string
  (schedule              btrbk-configuration-schedule)

  ;; enum, 'long
  (timestamp-format      btrbk-configuration-timestamp-format
                         (default 'long))
  ;; string
  (snapshot-preserve-min btrbk-configuration-snapshot-preserve-min
                         (default "1d"))
  ;; string
  (snapshot-preserve     btrbk-configuration-snapshot-preserve
                         (default "7d"))
  ;; string
  (volume                btrbk-configuration-volume)

  ;; string
  (snapshot-dir          btrbk-configuration-snapshot-dir
                         (default ".snapshots"))

  ;; list of string
  (subvolumes            btrbk-configuration-subvolumes
                         (default '()))

  ;; string
  (target                btrbk-configuration-target
                         (default #f))
  ;; string
  (pre-run-hook          btrbk-configuration-pre-run-hook
                         (default #f)))


(define (btrbk-config-file config)
  "Return the btrbk configuration file corresponding to CONFIG."
  (computed-file
   "btrbk-config"
   #~(begin
       (use-modules (ice-9 match))
       (use-modules (guix build utils))
       (mkdir-p #$output)
       (call-with-output-file (string-append #$output "/btrbk.conf")
         (lambda (port)
           (display "# Generated by 'btrbk-service'.\n" port)
           (format port "timestamp_format ~a\n"
                   '#$(btrbk-configuration-timestamp-format config))
           (format port "snapshot_preserve_min ~a\n"
                   #$(btrbk-configuration-snapshot-preserve-min config))
           (format port "snapshot_preserve ~a\n"
                   #$(btrbk-configuration-snapshot-preserve config))
           (let ((target #$(btrbk-configuration-target config)))
             (when target
               (format port "target ~a\n" target)))
           (format port "volume ~a\n"
                   #$(btrbk-configuration-volume config))
           (format port "  snapshot_dir ~a\n"
                   #$(btrbk-configuration-snapshot-dir config))

           (for-each
            (lambda (subvolume)
             (format port "  subvolume ~a\n" subvolume))
            '#$(btrbk-configuration-subvolumes config))
           #t)))
   #:options
   '(#:local-build? #t
     #:modules ((guix build utils)))))

(define (btrbk-mcron-jobs config)
  (list
    #~(job #$(btrbk-configuration-schedule config)
           (string-append
             #$(let* ((pre-run-hook (btrbk-configuration-pre-run-hook config)))
                 (when pre-run-hook
                   #~(string-append #$pre-run-hook "; ")))
             (string-append #$(btrbk-configuration-btrbk config) "/bin/btrbk -c " #$(btrbk-config-file config) "/btrbk.conf run")))))

(define btrbk-service-type
  (service-type
    (name 'btrbk)
    (extensions
      (list
        (service-extension mcron-service-type
                           btrbk-mcron-jobs)))
    (description "Run btrbk periodically for the specified volumes and subvolumes. Optionally running pre-run-hook before that.")))
