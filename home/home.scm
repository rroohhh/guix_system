(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu home services sound)
             (gnu home services fontutils)
             (gnu home services guix)
             (gnu home services desktop)
             (gnu services)
             (gnu packages)
             (gnu packages admin)
             (gnu packages emacs)
             (gnu packages wm)
             (gnu packages xdisorg)
             (guix gexp)
             (guix channels)
             (guix transformations)
             (rosenthal packages wm)
             (home config)
             (home packages)
             (home programs)
             (vup rust-nightly))




;; (define disable-eee-on-enp0s25
;;   (list (shepherd-service
;;          (documentation "disable eee on enp0s25")
;;          (provision '(disable-eee))
;;          (requirement '(udev))
;;          (one-shot? #t)
;;          (start #~(lambda _
;;                     (invoke (string-append #$ethtool "/sbin/ethtool") "--set-eee" "enp0s25" "eee" "off")
;;                     (invoke (string-append #$ethtool "/sbin/ethtool") "-K" "enp0s25" "tso" "off")
;;                     (invoke (string-append #$ethtool "/sbin/ethtool") "-A" "enp0s25" "tx" "off" "rx" "off" "autoneg" "off"))))))


(define (wayland-shepherd-service delay)
  (list (shepherd-service
         (provision '(wayland-display))
         (modules '((ice-9 ftw)
                    (ice-9 match)
                    (srfi srfi-1)))
         (start
          #~(lambda* (#:optional (display (getenv "WAYLAND_DISPLAY")))
              (define wayland-directory
                (getenv "XDG_RUNTIME_DIR"))

              ;; (format #t "wayland-dir: `~a`" wayland-directory)

              (define (find-display delay)
                ;; Wait for an accessible socket to show up in WAYLAND-DIRECTORY,
                ;; up to DELAY seconds.
                (let loop ((attempts delay))
                  (define socket
                    (find (match-lambda
                            ((or "." "..") #f)
                            (name
                             (let ((abs-name (in-vicinity wayland-directory
                                                          name)))
                               (begin
                                 (and (access? abs-name O_RDWR) (string-prefix? "wayland-" name))))))
                          (or (scandir wayland-directory) '())))

                  (if (and socket (string-prefix? "wayland-" socket))
                      (let ((display (string-append
                                      "wayland-" (string-drop socket 8))))
                        (format #t "wayland display server found at ~s.~%"
                                display)
                        display)
                      (if (zero? attempts)
                          (begin
                            (format (current-error-port)
                                    "wayland display server did not show up; \
giving up.\n")
                            #f)
                          (begin
                            (sleep 1)
                            (loop (- attempts 1)))))))

              (let ((display (or display (find-display #$delay))))
                (when display
                  (setenv "WAYLAND_DISPLAY" display))
                display)))
         (stop #~(lambda (_)
                   (unsetenv "WAYLAND_DISPLAY")
                   #f))
         (respawn? #f))))

(define home-wayland-service-type
  (service-type
   (name 'home-wayland-display)
   (extensions (list (service-extension home-shepherd-service-type
                                        wayland-shepherd-service)))
   (default-value 10)
   (description "")))


(define (swaync-shepherd-service config)
  (list (shepherd-service
         (documentation "swaync program.")
         (provision '(swaync))
         (requirement '(wayland-display))
         (modules '((srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (fork+exec-command
                     (list #$(file-append
                              swaynotificationcenter
                              "/bin/swaync"))
                     #:environment-variables
                     (cons (string-append "WAYLAND_DISPLAY=" (getenv "WAYLAND_DISPLAY"))
                           (remove (cut string-prefix? "WAYLAND_DISPLAY=" <>)
                                   (default-environment-variables))))))
         (stop #~(make-kill-destructor)))))

(define home-swaync-service-type
  (service-type
   (name 'home-swaync)
   (extensions (list (service-extension home-shepherd-service-type
                                        swaync-shepherd-service)
                     (service-extension home-wayland-service-type
                                        (const #t))))
   (default-value #f)
   (description
    "Run swaync.")))


(define (waybar-shepherd-service config)
  (list (shepherd-service
         (documentation "waybar program.")
         (provision '(waybar))
         (requirement '(wayland-display))
         (modules '((srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (system*
                     #$(file-append hyprland "/bin/hyprctl") "-i" "0" "dispatch" "exec" #$(file-append waybar "/bin/waybar"))
                    #t))
         (stop #~(lambda _
                   (system* "pkill" "waybar"))))))

(define home-waybar-service-type
  (service-type
   (name 'home-waybar)
   (extensions (list (service-extension home-shepherd-service-type
                                        waybar-shepherd-service)
                     (service-extension home-wayland-service-type
                                        (const #t))))
   (default-value #f)
   (description
    "Run waybar.")))

(define (wlsunset-shepherd-service config)
  (list (shepherd-service
         (documentation "wlsunset program.")
         (provision '(wlsunset))
         (requirement '(wayland-display))
         (modules '((srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (fork+exec-command
                     (list #$(file-append
                              wlsunset
                              "/bin/wlsunset") "-l" "49.4" "-L" "8.7")
                     #:environment-variables
                     (cons (string-append "WAYLAND_DISPLAY=" (getenv "WAYLAND_DISPLAY"))
                           (remove (cut string-prefix? "WAYLAND_DISPLAY=" <>)
                                   (default-environment-variables))))))
         (stop #~(make-kill-destructor)))))

(define home-wlsunset-service-type
  (service-type
   (name 'home-wlsunset)
   (extensions (list (service-extension home-shepherd-service-type
                                        wlsunset-shepherd-service)
                     (service-extension home-wayland-service-type
                                        (const #t))))
   (default-value #f)
   (description
    "Run wlsunset.")))


(define emacs-daemon-service
  (list (shepherd-service
         (documentation "emacs daemon")
         (provision '(emacsd))
         (requirement '())
         (start #~(make-system-constructor
                   #$(file-append emacs "/bin/emacs --daemon")))
         (stop #~(make-kill-destructor)))))


(define ada-transform
  ;; The package transformation procedure.
  (options->transformation
   '((tune . "znver2"))))


(home-environment
 (packages (map ada-transform full-packages))
 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             ;; (bash-profile `(,user-shepherd-setup))
             (bashrc `(,bashrc))))

   (service home-shepherd-service-type)
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (service home-swaync-service-type)
   (service home-waybar-service-type)
   (service home-wlsunset-service-type)
                                        ;(service home-fontconfig-service-type)

   (simple-service 'extra-channels-service
                   home-channels-service-type
                   (list (channel
                          (name 'guix)
                          (url "https://git.savannah.gnu.org/git/guix.git")
                          (branch "master"))
                         (channel
                          (name 'guix-hpc)
                          (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
                          (branch "master"))
                         (channel
                          (name 'guix-science)
                          (url "https://github.com/guix-science/guix-science.git")
                          (branch "master")
                          (introduction
                           (make-channel-introduction
                            "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
                            (openpgp-fingerprint
                             "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
                         (channel
                          (name 'vup)
                          (url "https://github.com/rroohhh/guix_packages.git"))
                         (channel
                          (name 'guix-gaming-games)
                          (url "https://gitlab.com/guix-gaming-channels/games.git")
                          (introduction
                           (make-channel-introduction
                            "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
                            (openpgp-fingerprint
                             "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
                         (channel
                          (name 'nonguix)
                          (url "https://gitlab.com/nonguix/nonguix")
                          (introduction
                           (make-channel-introduction
                            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                            (openpgp-fingerprint
                             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
                         (channel
                          (name 'rosenthal)
                          (url "https://github.com/rakino/rosenthal")
                          (branch "trunk")
                          (introduction
                           (make-channel-introduction
                            "7677db76330121a901604dfbad19077893865f35"
                            (openpgp-fingerprint
                             "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7"))))))


   (service home-inputrc-service-type
            (home-inputrc-configuration
             (variables
              `(("colored-completion-prefix" . #t)
                ("colored-stats" . #t)
                ("visible-stats" . #t)
                ("editing-mode" . "vi")))
             (extra-content
              (string-append "set keymap vi-command
k: nop
j: nop
" (keys* up) ": previous-history
" (keys* down) ": next-history"))))
   ;; ("$include" . "/etc/inputrc")
   ;; ("$include" . ,(file-append
   ;;                 (specification->package "readline")
   ;;                 "/etc/inputrc"))))))
   ;; (service home-inputrc-service-type
   ;;          (home-inputrc-configuration

   ;;           (variables
   ;;            '(("editing-mode" . "vi")
   ;;              ("keymap" . "vi-command")))
   ;;           (key-bindings
   ;;            `(("k" . "nop")
   ;;              ("j" . "nop")
   ;;              (,(keys* up) . "previous-history")
   ;;              (,(keys* down) . "next-history")))))

   (simple-service 'emacs-daemon-service home-shepherd-service-type
                   emacs-daemon-service)

   (simple-service 'some-useful-env-vars-service
                   home-environment-variables-service-type
                   `(("QT_QPA_PLATFORM" . "wayland")
                     ("XDG_SESSION_TYPE" . "wayland")
                     ("GDK_BACKEND" . "wayland")
                     ("__CARGO_TESTS_ONLY_SRC_ROOT" . ,(file-append rust-nightly "/lib/rustlib/src/rust"))
                     ("_JAVA_AWT_WM_NONREPARENTING" . #t)))

   (simple-service 'simple-configs-config
                   home-files-service-type
                   basic-program-configs))))
