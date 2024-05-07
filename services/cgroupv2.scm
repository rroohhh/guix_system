(define-module (services cgroupv2)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages virtualization)
  #:use-module (ice-9 match)
  #:export (elogind-service
            elogind-service-type
            elogind-configuration
            docker-service-type
            docker-configuration))

(define-record-type* <docker-configuration>
  docker-configuration make-docker-configuration
  docker-configuration?
  ;; <package>
  (docker                      docker-configuration-docker
                               (default docker))
  ;; <package>
  (docker-cli                  docker-configuration-docker-cli
                               (default docker-cli))
  ;; <package>
  (containerd                  docker-configuration-containerd
                               (default containerd))
  ;; <package>
  (proxy                       docker-configuration-proxy
                               (default docker-libnetwork-cmd-proxy))
  ;; boolean
  (enable-proxy?               docker-configuration-enable-proxy?
                               (default #t))
  ;; boolean
  (debug?                      docker-configuration-debug?
                               (default #f))
  ;; boolean
  (enable-iptables?            docker-configuration-enable-iptables?
                               (default #t))
  ;; file
  (config                      docker-configuration-config
                               (default #f)))

;; (define-configuration docker-configuration
;;   (docker
;;    (package docker)
;;    "Docker daemon package.")
;;   (docker-cli
;;    (package docker-cli)
;;    "Docker client package.")
;;   (containerd
;;    (package containerd)
;;    "containerd package.")
;;   (proxy
;;    (package docker-libnetwork-cmd-proxy)
;;    "The proxy package to support inter-container and outside-container
;; loop-back communications.")
;;   (enable-proxy?
;;    (boolean #t)
;;    "Enable or disable the user-land proxy (enabled by default).")
;;   (debug?
;;    (boolean #f)
;;    "Enable or disable debug output.")
;;   (enable-iptables?
;;    (boolean #t)
;;    "Enable addition of iptables rules (enabled by default).")
;;   (config
;;    (boolean #t)
;;    "Docker daemon config.json")
;;   (no-serialization))

(define %docker-accounts
  (list (user-group (name "docker") (system? #t))))

(define (containerd-config-file config)
  (computed-file
   "config.toml"
   #~(with-output-to-file #$output
       (lambda _
         (set-port-encoding! (current-output-port) "UTF-8") ;; shitty hack for unicode to work
         (format #t "~a"
                 (string-append
                  #$@(list
                      "[plugins]
  [plugins.linux]
    runtime = \"" (file-append runc "/sbin/runc") "\"
")))))))

(define (%containerd-activation config)
  (let ((state-dir "/var/lib/containerd"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (%docker-activation config)
  (%containerd-activation config)
  (let ((state-dir "/var/lib/docker"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (containerd-shepherd-service config)
  (let* ((package (docker-configuration-containerd config))
         (debug? (docker-configuration-debug? config))
         (containerd (docker-configuration-containerd config)))
    (shepherd-service
     (documentation "containerd daemon.")
     (provision '(containerd))
     (start #~(make-forkexec-constructor
               (list (string-append #$package "/bin/containerd")
                     "--address=/run/containerd/containerd_system.sock"
                     (string-append "--config=" #$(containerd-config-file config))
                     #$@(if debug?
                            '("--log-level=debug")
                            '("--log-level=trace")))
               ;; For finding containerd-shim binary.
               #:environment-variables
               (list (string-append "PATH=" #$containerd "/bin"))
               #:log-file "/var/log/containerd.log"))
     (stop #~(make-kill-destructor)))))

(define (docker-shepherd-service config)
  (let* ((docker (docker-configuration-docker config))
         (enable-proxy? (docker-configuration-enable-proxy? config))
         (enable-iptables? (docker-configuration-enable-iptables? config))
         (proxy (docker-configuration-proxy config))
         (debug? (docker-configuration-debug? config))
         (config (docker-configuration-config config)))
    (shepherd-service
           (documentation "Docker daemon.")
           (provision '(dockerd))
           (requirement '(containerd
                          dbus-system
                          elogind
                          file-system-/sys/fs/cgroup
                          networking
                          udev))
           (start #~(make-forkexec-constructor
                     (list (string-append #$docker "/bin/dockerd")
                           "-p" "/var/run/docker.pid"
                           "--containerd=/run/containerd/containerd_system.sock"
                           (string-append  "--default-runtime=" #$(file-append runc "/sbin/runc"))
                           #$@(if config
                                  (list #~(string-append
                                           "--config-file=" #$config))
                                  '())
                           #$@(if debug?
                                  '("--debug" "--log-level=debug")
                                  '())
                           #$@(if enable-proxy?
                                  (list "--userland-proxy=true"
                                        #~(string-append
                                           "--userland-proxy-path=" #$proxy "/bin/proxy"))
                                  '("--userland-proxy=false"))
                           (if #$enable-iptables?
                               "--iptables"
                               "--iptables=false"))
                     #:pid-file "/var/run/docker.pid"
                     #:log-file "/var/log/docker.log"))
           (stop #~(make-kill-destructor)))))

(define docker-service-type
  (service-type (name 'docker)
                (description "Provide capability to run Docker application
bundles in Docker containers.")
                (extensions
                 (list
                  ;; Make sure the 'docker' command is available.
                  (service-extension profile-service-type
                                     (compose list docker-configuration-docker-cli))
                  (service-extension activation-service-type
                                     %docker-activation)
                  (service-extension shepherd-root-service-type
                                     (lambda (config)
                                       (list (containerd-shepherd-service config)
                                             (docker-shepherd-service config))))
                  (service-extension account-service-type
                                     (const %docker-accounts))))
                (default-value (docker-configuration))))

(define %elogind-file-systems
  ;; We don't use systemd, but these file systems are needed for elogind,
  ;; which was extracted from systemd.
  (append
   (list (file-system
           (device "none")
           (mount-point "/run/systemd")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev no-exec))
           (options "mode=0755")
           (create-mount-point? #t))
         (file-system
           (device "none")
           (mount-point "/run/user")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev no-exec))
           (options "mode=0755")
           (create-mount-point? #t))
         (file-system
           (device "cgroup2")
           (mount-point "/sys/fs/cgroup")
           (type "cgroup2")
           (check? #f)))))

(define-record-type* <elogind-configuration> elogind-configuration
  make-elogind-configuration
  elogind-configuration?
  (elogind                          elogind-package
                                    (default elogind))
  (kill-user-processes?             elogind-kill-user-processes?
                                    (default #f))
  (kill-only-users                  elogind-kill-only-users
                                    (default '()))
  (kill-exclude-users               elogind-kill-exclude-users
                                    (default '("root")))
  (inhibit-delay-max-seconds        elogind-inhibit-delay-max-seconds
                                    (default 5))
  (handle-power-key                 elogind-handle-power-key
                                    (default 'poweroff))
  (handle-suspend-key               elogind-handle-suspend-key
                                    (default 'suspend))
  (handle-hibernate-key             elogind-handle-hibernate-key
                                    ;; (default 'hibernate)
                                    ;; XXX Ignore it for now, since we don't
                                    ;; yet handle resume-from-hibernation in
                                    ;; our initrd.
                                    (default 'ignore))
  (handle-lid-switch                elogind-handle-lid-switch
                                    (default 'suspend))
  (handle-lid-switch-docked         elogind-handle-lid-switch-docked
                                    (default 'ignore))
  (handle-lid-switch-external-power elogind-handle-lid-switch-external-power
                                    (default 'ignore))
  (power-key-ignore-inhibited?      elogind-power-key-ignore-inhibited?
                                    (default #f))
  (suspend-key-ignore-inhibited?    elogind-suspend-key-ignore-inhibited?
                                    (default #f))
  (hibernate-key-ignore-inhibited?  elogind-hibernate-key-ignore-inhibited?
                                    (default #f))
  (lid-switch-ignore-inhibited?     elogind-lid-switch-ignore-inhibited?
                                    (default #t))
  (holdoff-timeout-seconds          elogind-holdoff-timeout-seconds
                                    (default 30))
  (idle-action                      elogind-idle-action
                                    (default 'ignore))
  (idle-action-seconds              elogind-idle-action-seconds
                                    (default (* 30 60)))
  (runtime-directory-size-percent   elogind-runtime-directory-size-percent
                                    (default 10))
  (runtime-directory-size           elogind-runtime-directory-size
                                    (default #f))
  (remove-ipc?                      elogind-remove-ipc?
                                    (default #t))

  (suspend-state                    elogind-suspend-state
                                    (default '("mem" "standby" "freeze")))
  (suspend-mode                     elogind-suspend-mode
                                    (default '()))
  (hibernate-state                  elogind-hibernate-state
                                    (default '("disk")))
  (hibernate-mode                   elogind-hibernate-mode
                                    (default '("platform" "shutdown")))
  (hybrid-sleep-state               elogind-hybrid-sleep-state
                                    (default '("disk")))
  (hybrid-sleep-mode                elogind-hybrid-sleep-mode
                                    (default
                                      '("suspend" "platform" "shutdown"))))

(define (elogind-configuration-file config)
  (define (yesno x)
    (match x
      (#t "yes")
      (#f "no")
      (_ (error "expected #t or #f, instead got:" x))))
  (define char-set:user-name
    (string->char-set "abcdefghijklmnopqrstuvwxyz0123456789_-"))
  (define (valid-list? l pred)
    (and-map (lambda (x) (string-every pred x)) l))
  (define (user-name-list users)
    (unless (valid-list? users char-set:user-name)
      (error "invalid user list" users))
    (string-join users " "))
  (define (enum val allowed)
    (unless (memq val allowed)
      (error "invalid value" val allowed))
    (symbol->string val))
  (define (non-negative-integer x)
    (unless (exact-integer? x) (error "not an integer" x))
    (when (negative? x) (error "negative number not allowed" x))
    (number->string x))
  (define handle-actions
    '(ignore poweroff reboot halt kexec suspend hibernate hybrid-sleep lock))
  (define (handle-action x)
    (enum x handle-actions))
  (define (sleep-list tokens)
    (unless (valid-list? tokens char-set:user-name)
      (error "invalid sleep list" tokens))
    (string-join tokens " "))
  (define-syntax ini-file-clause
    (syntax-rules ()
      ((_ config (prop (parser getter)))
       (string-append prop "=" (parser (getter config)) "\n"))
      ((_ config str)
       (string-append str "\n"))))
  (define-syntax-rule (ini-file config file clause ...)
    (plain-file file (string-append (ini-file-clause config clause) ...)))
  (ini-file
   config "logind.conf"
   "[Login]"
   ("KillUserProcesses" (yesno elogind-kill-user-processes?))
   ("KillOnlyUsers" (user-name-list elogind-kill-only-users))
   ("KillExcludeUsers" (user-name-list elogind-kill-exclude-users))
   ("InhibitDelayMaxSec" (non-negative-integer elogind-inhibit-delay-max-seconds))
   ("HandlePowerKey" (handle-action elogind-handle-power-key))
   ("HandleSuspendKey" (handle-action elogind-handle-suspend-key))
   ("HandleHibernateKey" (handle-action elogind-handle-hibernate-key))
   ("HandleLidSwitch" (handle-action elogind-handle-lid-switch))
   ("HandleLidSwitchDocked" (handle-action elogind-handle-lid-switch-docked))
   ("HandleLidSwitchExternalPower" (handle-action elogind-handle-lid-switch-external-power))
   ("PowerKeyIgnoreInhibited" (yesno elogind-power-key-ignore-inhibited?))
   ("SuspendKeyIgnoreInhibited" (yesno elogind-suspend-key-ignore-inhibited?))
   ("HibernateKeyIgnoreInhibited" (yesno elogind-hibernate-key-ignore-inhibited?))
   ("LidSwitchIgnoreInhibited" (yesno elogind-lid-switch-ignore-inhibited?))
   ("HoldoffTimeoutSec" (non-negative-integer elogind-holdoff-timeout-seconds))
   ("IdleAction" (handle-action elogind-idle-action))
   ("IdleActionSec" (non-negative-integer elogind-idle-action-seconds))
   ("RuntimeDirectorySize"
    (identity
     (lambda (config)
       (match (elogind-runtime-directory-size-percent config)
         (#f (non-negative-integer (elogind-runtime-directory-size config)))
         (percent (string-append (non-negative-integer percent) "%"))))))
   ("RemoveIPC" (yesno elogind-remove-ipc?))
   "[Sleep]"
   ("SuspendState" (sleep-list elogind-suspend-state))
   ("SuspendMode" (sleep-list elogind-suspend-mode))
   ("HibernateState" (sleep-list elogind-hibernate-state))
   ("HibernateMode" (sleep-list elogind-hibernate-mode))
   ("HybridSleepState" (sleep-list elogind-hybrid-sleep-state))
   ("HybridSleepMode" (sleep-list elogind-hybrid-sleep-mode))))

(define (elogind-dbus-service config)
  (list (wrapped-dbus-service (elogind-package config)
                              "libexec/elogind/elogind"
                              `(("ELOGIND_CONF_FILE"
                                 ,(elogind-configuration-file config))))))

(define (pam-extension-procedure config)
  "Return an extension for PAM-ROOT-SERVICE-TYPE that ensures that all the PAM
services use 'pam_elogind.so', a module that allows elogind to keep track of
logged-in users (run 'loginctl' to see elogind's world view of users and
seats.)"
  (define pam-elogind
    (pam-entry
     (control "required")
     (module (file-append (elogind-package config)
                          "/lib/security/pam_elogind.so"))))

  (list (lambda (pam)
          (pam-service
           (inherit pam)
           (session (cons pam-elogind (pam-service-session pam)))))))

(define (elogind-shepherd-service config)
  "Return a Shepherd service to start elogind according to @var{config}."
  (list (shepherd-service
         (requirement '(dbus-system))
         (provision '(elogind))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (elogind-package config)
                                        "/libexec/elogind/elogind"))
                   #:environment-variables
                   (list (string-append "ELOGIND_CONF_FILE="
                                        #$(elogind-configuration-file
                                           config)))))
         (stop #~(make-kill-destructor)))))

(define elogind-service-type
  (service-type (name 'elogind)
                (description "elogind")
                (extensions
                 (list (service-extension dbus-root-service-type
                                          elogind-dbus-service)
                       (service-extension udev-service-type
                                          (compose list elogind-package))
                       (service-extension polkit-service-type
                                          (compose list elogind-package))

                       ;; Start elogind from the Shepherd rather than waiting
                       ;; for bus activation.  This ensures that it can handle
                       ;; events like lid close, etc.
                       (service-extension shepherd-root-service-type
                                          elogind-shepherd-service)

                       ;; Provide the 'loginctl' command.
                       (service-extension profile-service-type
                                          (compose list elogind-package))

                       ;; Extend PAM with pam_elogind.so.
                       (service-extension pam-root-service-type
                                          pam-extension-procedure)

                       ;; We need /run/user, /run/systemd, etc.
                       (service-extension file-system-service-type
                                          (const %elogind-file-systems))))
                (default-value (elogind-configuration))))

(define* (elogind-service #:key (config (elogind-configuration)))
  "Return a service that runs the @command{elogind} login and seat management
service.  The @command{elogind} service integrates with PAM to allow other
system components to know the set of logged-in users as well as their session
types (graphical, console, remote, etc.).  It can also clean up after users
when they log out."
  (service elogind-service-type config))
