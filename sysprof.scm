(use-modules (guix packages))
(use-modules (guix build-system meson))
(use-modules (guix download))
(use-modules (guix gexp))
(use-modules (guix utils))


(define-public sysprof
  (package
    (name "sysprof")
    (version "45")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/sysprof/"
                           (version-major+minor version) "/"
                           "sysprof-" 45 ".tar.xz"))
       (sha256
        (base32 "16nkr1qs7s2ylhwj58zj6b7in72nw7z72glaz746f2g7dbqs00k4"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dsystemdunitdir=" #$output "/share/systemd"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-post-install
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false")))))))
    (propagated-inputs
     (list polkit))
    (inputs
     (list glib-next
           gtk
           json-glib
           libadwaita
           libdazzle
           libunwind
           polkit))
    (native-inputs
     (list gettext-minimal
           `(,glib-next "bin")          ;for gdbus-codegen, etc.
           itstool
           libxml2
           pkg-config))
    ;; This home page is so woefully out of date as to be essentially useless.
    ;; (home-page "http://www.sysprof.com")
    (home-page "https://wiki.gnome.org/Apps/Sysprof")
    (synopsis "System-wide performance profiler for GNU/Linux")
    (description
     "Sysprof performs detailed, accurate, and fast CPU profiling of an entire
GNU/Linux system including the kernel and all user-space applications.  This
helps find the function(s) in which a program spends most of its time.

It uses the kernel's built-in @code{ptrace} feature and handles shared
libraries.  Applications do not need to be recompiled--or even restarted.")
    (license license:gpl3+)))
