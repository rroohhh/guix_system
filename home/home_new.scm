(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages admin)
             (guix gexp)
             (home packages)
             (home programs))

(home-environment
 (packages full-packages)
 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile `(,user-shepherd-setup))
             (bashrc `(,bashrc))))

   (simple-service 'some-useful-env-vars-service
		home-environment-variables-service-type
		`(("QT_QPA_PLATFORM" . "wayland")
          ("XDG_SESSION_TYPE" . "wayland")
          ("GDK_BACKEND" . "wayland")
          ("_JAVA_AWT_WM_NONREPARENTING" . #t)))

   (simple-service 'simple-configs-config
                   home-files-service-type
                   basic-program-configs))))
