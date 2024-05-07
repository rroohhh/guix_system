(define-module (config sysprof)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system meson)
  #:use-module (guix packages))


(define-public libpanel-1.3
  (package
    (inherit libpanel)
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/libpanel")
             (commit version)))
       (file-name (git-file-name "libpanel" version))
       (sha256
        (base32 "0sp0pkccxpkrjk2bl1q12xl5w5hal015z68c507zym943skd28ry"))))))

(define-public sysprof-new
  (package
    (name "sysprof")
    (version "46")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/sysprof/"
                           version "/"
                           "sysprof-" version ".0.tar.xz"))
       (sha256
        (base32 "0xnil6ian02mlgdq9s5rwd4l5vp6ywyp4nm08q4lwgmbxdspxakk"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dsystemdunitdir=" #$output "/share/systemd")
              "-Dhelp=true")
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
     ;; Listed in sysprof-4.pc or sysprof-ui-5.pc
     (list glib json-glib libadwaita polkit))
    (inputs
     (list glib
           gtk
           json-glib
           libadwaita
           libdazzle
           libunwind
           libdex
           libpanel-1.3
           polkit))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")          ;for gdbus-codegen, etc.
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
    (license #f)))

sysprof
