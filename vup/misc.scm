(define-module (vup misc)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix licenses:)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system maven)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages shells)
  #:use-module ((gnu packages animation)
                #:prefix guix:)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages check)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages man)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages web)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages image)
  #:use-module (vup mesa))

(define-public mbuffer
  (let* ((version "20230301"))
    (package
      (name "mbuffer")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://www.maier-komor.de/software/mbuffer/mbuffer-"
                      version ".tgz"))
                (sha256
                 (base32
                  "009d4m48yjidb91vdnrfv84nnd76n0i57g607llan3y0vq4n5xsk"))))
      (build-system gnu-build-system)
      (native-inputs (list which))
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (delete 'check)))) ;too lazy to include test deps
      (home-page "https://www.maier-komor.de/mbuffer.html")
      (synopsis
       "mbuffer is a tool for buffering data streams with a large set of unique features")
      (description
       "mbuffer is a tool for buffering data streams with a large set of unique features")
      (license licenses:gpl3))))

(define-public libmodbus
  (let* ((version "3.1.10"))
    (package
      (name "libmodbus")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/stephane/libmodbus/releases/download/v"
                      version "/libmodbus-" version ".tar.gz"))
                (sha256
                 (base32
                  "0lr2kdprgzdn4vbns4sniwp3s1kg1m8ng59zsjcmgzmpbbif96w9"))))
      (build-system gnu-build-system)
      (home-page "https://libmodbus.org/")
      (synopsis "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Win32")
      (description
       "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Win32")
      (license licenses:gpl3))))

(define-public mbpoll
  (let* ((version "1.4.11"))
    (package
      (name "mbpoll")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/epsilonrt/mbpoll/archive/v" version
                      ".tar.gz"))
                (sha256
                 (base32
                  "00dh65jky7a97r538nb5n0pgy3r175s41hmbh3yasqri867jwcsx"))))
      (build-system cmake-build-system)
      (inputs `(("libmodbus" ,libmodbus)
                ("pkg-config" ,pkg-config)))
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (delete 'check)))) ;no tests
      (home-page "https://github.com/epsilonrt/mbpoll")
      (synopsis
       "mbpoll is a command line utility to communicate with ModBus slave (RTU or TCP).")
      (description
       "mbpoll is a command line utility to communicate with ModBus slave (RTU or TCP).")
      (license licenses:gpl3))))

(define-public antpm
  (let* ((version "1.20"))
    (package
      (name "antpm")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/ralovich/antpm/archive/v" version
                      ".tar.gz"))
                (sha256
                 (base32
                  "1rwp707fcg5w5qfhmadbjrbia8arjfz8knb7pvcycxl6f4hz3sn8"))))
      (build-system cmake-build-system)
      (inputs `(("libusb" ,libusb)
                ("boost" ,boost)
                ("libxml2" ,libxml2)))
      (arguments
       '(#:configure-flags `("-DUSE_BOOST_STATIC_LINK=False")
         #:phases (modify-phases %standard-phases
                    (add-before 'configure 'cd-to-src
                      (lambda _
                        (chdir "src") #t))
                    (delete 'check)))) ;no tests
      (home-page "https://github.com/ralovich/antpm")
      (synopsis "ANT+minus (ANT / ANT+ / ANT-FS)")
      (description "ANT+minus (ANT / ANT+ / ANT-FS)")
      (license licenses:gpl3))))

(define-public rdfind
  (let* ((version "1.5.0"))
    (package
      (name "rdfind")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/pauldreik/rdfind/archive/refs/tags/releases/"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "022gi48v8xmaqbawq9vyla4ypw7hy0ws7y8l8qbz157imsah1gss"))))
      (build-system gnu-build-system)
      (native-inputs (list automake autoconf autoconf-archive which))
      (inputs (list nettle))
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (delete 'check)))) ;too lazy to make tests work
      (home-page "https://github.com/pauldreik/rdfind")
      (synopsis "find duplicate files utility")
      (description "find duplicate files utility")
      (license licenses:gpl2+))))

(define-public xrestop
  (let* ((version "0.5"))
    (package
      (name "xrestop")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://xorg.freedesktop.org/archive/individual/app/xrestop-"
                      version ".tar.bz2"))
                (sha256
                 (base32
                  "06ym32famav8qhdms5k7y5i14nfq89hhvfn5g452jjqzkpcsbl49"))))
      (build-system gnu-build-system)
      (inputs `(("libxres" ,libxres)
                ("libx11" ,libx11)
                ("libxext" ,libxext)
                ("ncurses" ,ncurses)))
      (home-page "http://freedesktop.org/wiki/Software/xrestop")
      (synopsis
       "Uses the X-Resource extension to provide 'top' like statistics")
      (description
       "Uses the X-Resource extension to provide 'top' like statistics")
      (license licenses:gpl2+))))

(define-public libsigrokdecode-master
  (let ((commit "0235970293590f673a253950e6c61017cefa97df"))
    (package
      (inherit libsigrokdecode)
      (version (string-append "0.5.3-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://sigrok.org/libsigrokdecode")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name "libsigrokdecode" version))
                (sha256
                 (base32
                  "055nz79ai3ya2c4b461bh8c04ky3b436xqg85a6g51lkzjwi689p"))))
      (arguments
       (append `(#:modules ((guix build utils)
                            (guix build gnu-build-system)
                            ((guix build gnu-build-system)
                             #:prefix gnu:)))
               (substitute-keyword-arguments (package-arguments libsigrokdecode)
                 ((#:phases phases)
                  `(modify-phases ,phases
                    (replace 'bootstrap
                      (assoc-ref gnu:%standard-phases
                                 'bootstrap)))))))
      (native-inputs (modify-inputs (package-native-inputs libsigrokdecode)
                       (append autoconf automake libtool))))))

(define-public libsigrok-master
  (let ((commit "b503d24cdf56abf8c0d66d438ccac28969f01670"))
    (package
      (inherit libsigrok)
      (version (string-append "0.5.3-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://sigrok.org/libsigrok.git")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name "libsigrok" version))
                (sha256
                 (base32
                  "116lgqsv0iy6yj9bq25z5q1f9h06mf84zr5aby0wms6l5i8b8igl"))))
      (inputs (append (package-inputs libsigrok)
                      `(("glibmm" ,glibmm))))
      (native-inputs (append (package-native-inputs libsigrok)
                             `(("autoconf" ,autoconf)
                               ("automake" ,automake)
                               ("libtool" ,libtool)))))))

(define-public pulseview-libsigrok-master
  (let ((commit "d00efc65ef47090b71c4da12797056033bee795f"))
    (package
      (inherit ((package-input-rewriting/spec `(("libsigrok" unquote
                                                 (const libsigrok-master))
                                                ("libsigrokdecode" unquote
                                                 (const libsigrokdecode-master))))
                pulseview))
      (version (string-append (package-version pulseview) "-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://sigrok.org/pulseview.git")
                      (commit commit)))
                (file-name (git-file-name (package-name pulseview) version))
                (sha256
                 (base32
                  "1lcq9dv1yvkjh21c7l9b1v1627jljxbp1dqgn9g3qykplm9cq1rk"))))
      (inputs (append (package-inputs pulseview)
                      `(("glibmm" ,glibmm)))))))

(define-public ofono
  (package
    (name "ofono")
    (version "2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "git://git.kernel.org/pub/scm/network/ofono/ofono.git")
                    (commit "0f24601c553f3183a0843b50f6f271fd0eee174f")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vsxdykn68a9klc8r8pphr8dbn5zpdpj86na7gnpq4w1m62g0q8j"))
              (patches `("0001-Search-connectors-in-OFONO_PLUGIN_PATH.patch"))))
    (build-system gnu-build-system)
    (inputs `(("automake" ,automake)
              ("nettle" ,nettle)
              ("libtool" ,libtool)
              ("autoconf" ,autoconf)
              ("autoconf-archive" ,autoconf-archive)
              ("pkg-config" ,pkg-config)
              ("glib" ,glib)
              ("dbus" ,dbus)
              ("ell" ,ell)
              ("udev" ,eudev)
              ("mobile-broadband-provider-info" ,mobile-broadband-provider-info)
              ("bluez" ,bluez)))
    (arguments
     '(#:configure-flags (list "--enable-external-ell"
                               (string-append "--with-dbusconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc")
                               (string-append "--with-dbusdatadir="
                                              (assoc-ref %outputs "out")
                                              "/share"))
       #:phases (modify-phases %standard-phases
                  (delete 'check)))) ;there are no tests
    (home-page "https://01.org/ofono")
    (synopsis "ofono")
    (description "ofono")
    (license licenses:gpl2)))

(define %common-gstreamer-phases
  '((add-after 'unpack
        'increase-test-timeout
      (lambda _
        (substitute* "tests/check/meson.build"
          (("'CK_DEFAULT_TIMEOUT', '[0-9]*'")
           "'CK_DEFAULT_TIMEOUT', '600'")
          (("timeout ?: .*\\)")
           "timeout: 90 * 60)")) #t))))

(define-public gstreamer-vaapi
  (package
    (name "gstreamer-vaapi")
    (version (package-version gstreamer))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/src/"
                                  name
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (patches (list (origin
                               (method url-fetch)
                               (uri
                                "https://raw.githubusercontent.com/rroohhh/guix_packages/e99ecbb/gstreamer_vaapi.patch")
                               (sha256
                                "06sk93zy9ddq3iynswjjiq4gv7kn5qgy5rnygjld34jxvmp2gyl6"))))
              ;; (patches (search-patches "gstreamer_vaapi.patch"))
              (sha256
               (base32
                "0i8bd46sxhf45b7vfx6wdy7r2rrqijyiaa806ynykpdb2srrxsbf"))))
    (build-system meson-build-system)
    (arguments
     ;; FIXME: 16/22 failing tests.
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  ,@%common-gstreamer-phases)))
    (inputs (list libva
                  gstreamer
                  gst-plugins-base
                  gst-plugins-bad
                  libdrm
                  mesa))
    (native-inputs (list pkg-config python-wrapper))
    ;; `()); ("flex" ,flex)
    ;; ("gst-plugins-bad" ,gst-plugins-bad)
    ;; ("gst-plugins-good" ,gst-plugins-good)
    ;; ("perl" ,perl)
    ;; ("pkg-config" ,pkg-config)))
    ;; ("python" ,python)

    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer library for vaapi")
    (description
     "Hardware-accelerated video decoding, encoding and processing on Intel graphics through VA-API")
    (license licenses:gpl2+)))

(define-public carla-2.5.4
  (package
    (inherit carla)
    (version "2.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/falkTX/Carla")
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name carla) version))
              (sha256
               (base32
                "1db1951mzwp7pb91by5xlafv2a8xp6d7kxf4iz84rzbxy5m3xpaa"))))))

(define-public kicad-nightly
  (package
    (inherit kicad)
    (name "kicad-nightly")
    (version "7.0.1-27e2e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/kicad/code/kicad.git")
                    (commit "27e2e820cbf5d4ca50bc451177764bac939822bd")))
              (sha256
               (base32
                "1maqwhvmgw5x0fnc78fik3c73x7x61rs9h1kzzkbmpw900bfcpwp"))
              (file-name (git-file-name name version))))))

(define-public gst-rtsp-server
  (package
    (name "gst-rtsp-server")
    (version (package-version gstreamer))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/src/"
                                  name
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "1jlljz6ccpfarjvhg8rvr5p95r229wdlrjk6bq77y4lvpqc2fh7f"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f ;17/17 failing ^^
       #:phases (modify-phases %standard-phases
                  ,@%common-gstreamer-phases)))
    (inputs (list glib gstreamer gst-plugins-base gst-plugins-bad))
    (native-inputs (list pkg-config python-wrapper))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "RTSP server based on GStreamer")
    (description
     "gst-rtsp-server is a library on top of GStreamer for building an RTSP server")
    (license licenses:gpl2+)))

(define-public ayatana-ido3
  (let ((commit "0.9.3"))
    (package
      (name "ayatana-ido3")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AyatanaIndicators/ayatana-ido")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pi0qan239chglhi1vx8ndlx3ppr3xjrr7alan0vw6m06wx89kmd"))))
      (build-system cmake-build-system)
      (inputs (list glib gtk+ gobject-introspection))
      (native-inputs (list pkg-config googletest vala `(,glib "bin")))
      (arguments `(#:tests? #f))
      (synopsis "Ayatana Indicator Display Objects")
      (description "Ayatana Indicator Display Objects")
      (home-page "https://github.com/AyatanaIndicators/ayatana-ido")
      (license licenses:lgpl3))))


(define-public libayatana-indicator
  (let ((commit "0.9.3"))
    (package
      (name "libayatana-indicator")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AyatanaIndicators/libayatana-indicator")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wmsm6mj1sq1qhwbksqxj6g4lb1w78v63lgjhf1hm8wrrfnmrrml"))))
      (build-system cmake-build-system)
      (propagated-inputs (list ayatana-ido3))
      (inputs (list glib gtk+))
      (native-inputs (list pkg-config `(,glib "bin")))
      (arguments
       `(#:tests? #f
         #:configure-flags `("-DENABLE_LOADER=OFF")))
      (synopsis "Ayatana Indicators Shared Library")
      (description "Ayatana Indicators Shared Library")
      (home-page "https://github.com/AyatanaIndicators/libayatana-indicator")
      (license licenses:gpl3))))

(define-public libayatana-appindicator
  (let ((commit "0.5.92"))
    (package
      (name "libayatana-appindicator")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AyatanaIndicators/libayatana-appindicator")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1c2yxrg834pn5vjw669vnvz7a235nl2hmw3djdi3s4zn2r09cdip"))))
      (build-system cmake-build-system)
      (propagated-inputs (list libdbusmenu libayatana-indicator))
      (inputs (list glib gtk+ gobject-introspection))
      (native-inputs (list pkg-config vala `(,glib "bin")))
      (arguments
       `(#:tests? #f
         #:configure-flags `("-DENABLE_BINDINGS_MONO=NO")))
      (synopsis "Ayatana Application Indicators Shared Library")
      (description "Ayatana Application Indicators Shared Library")
      (home-page "https://github.com/AyatanaIndicators/libayatana-appindicator")
      (license licenses:lgpl3))))

(define-public whatsapp-for-linux
  (let ((commit "1.6.2"))
    (package
      (name "whatsapp-for-linux")
      (version (string-append commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/eneshecan/whatsapp-for-linux")
                      (commit (string-append "v" commit))
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "136dkz9aingbc6swh2bfikfn82xxx57k2mndn6lc2sh542rkkld1"))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f))
      (inputs (list gtkmm-3 webkitgtk libayatana-appindicator libcanberra))
      (native-inputs (list pkg-config gnu-gettext `(,glib "bin")))
      (synopsis "An unofficial WhatsApp desktop application for Linux.")
      (description "An unofficial WhatsApp desktop application for Linux.")
      (home-page "https://github.com/eneshecan/whatsapp-for-linux")
      (license licenses:gpl3))))

(define-public glslang-upstream
  (package
    (inherit glslang)
    (version "12.1.0")
    (arguments
     (substitute-keyword-arguments (package-arguments glslang)
       ((#:configure-flags flags)
        `(cons "-DENABLE_GLSLANG_INSTALL=Yes"
          ,flags))))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KhronosGroup/glslang")
                    (commit version)))
              (sha256
               (base32
                "06g5m0y94a021zqvn1xmqfg44id3hqnyxqcc7y2cv8rndpn7z3jk"))
              (file-name (git-file-name (package-name glslang) version))))))

(define-public spirv-headers-upstream
  (package
    (inherit spirv-headers)
    (version "1.3.243.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KhronosGroup/SPIRV-Headers")
                    (commit (string-append "sdk-" version))))
              (sha256
               (base32
                "1yf9bly4fp6wnc0gal0ga2gvjq1h1y5f10isqs332v2wlspvgsjl"))
              (file-name (git-file-name (package-name spirv-headers) version))))))

(define-public spirv-tools-upstream
  (package
    (inherit spirv-tools)
    (version "2023.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KhronosGroup/SPIRV-Tools")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0z8r8k7bl5zvfl8qpxjg6swbbn0s162djgym1h2f8i538nxi33lp"))
              (file-name (git-file-name (package-name spirv-tools) version))))
    (inputs (modify-inputs (package-inputs spirv-tools)
              (replace "spirv-headers" spirv-headers-upstream)))
    (arguments
     (substitute-keyword-arguments (package-arguments spirv-tools)
       ((#:configure-flags flags)
        `(cons "-DSPIRV_WERROR=No"
          ,flags))))))

(define-public shaderc-upstream
  (package
    (inherit shaderc)
    (version "2023.3")
    (source (origin
              (method git-fetch)
              (patches (search-patches "shaderc_glslang_libs.patch"))
              (uri (git-reference
                    (url "https://github.com/google/shaderc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "shaderc" version))
              (sha256
               (base32
                "0sa0nmcxklx6yy67wvkc46mqzpj1x44r7w0fcif4x8zvzzh5hrwz"))))
    (inputs (modify-inputs (package-inputs shaderc)
              (replace "glslang" glslang-upstream)
              (replace "spirv-tools" spirv-tools-upstream)
              (replace "spirv-headers" spirv-headers-upstream)))))

(define-public vulkan-headers-upstream
  (package
    (inherit vulkan-headers)
    (version "1.3.246")
    (name (package-name vulkan-headers))
    (source (origin
              (inherit (package-source vulkan-headers))
              (uri (git-reference
                    (url "https://github.com/KhronosGroup/Vulkan-Headers")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q4n81qrpr3x0rhh7r9r76jzn3a6qzi1q21dds9z5xc1hxz6kkdm"))))))

(define-public vulkan-hpp
  (package
    (name "vulkan-hpp")
    (version "1.3.246")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KhronosGroup/Vulkan-Hpp")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "027i5r1nak4jgjh8f8awa4gx4rdmxc09vrkr5mm4z398d7jcdhwf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check)
                  (add-after 'unpack 'remove-w-error
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        (("-Werror")
                         "")) #t)))
       #:configure-flags (list "-DVULKAN_HPP_INSTALL=Yes"
                               (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                                              (assoc-ref %outputs "out")
                                              "/include"))))
    (home-page "https://github.com/KhronosGroup/Vulkan-Hpp")
    (synopsis "Open-Source Vulkan C++ API")
    (description "Vulkan-Hpp: C++ Bindings for Vulkan")
    (license licenses:asl2.0)))

(define-public zfp
  (let* ((version "1.0.0"))
    (package
      (name "zfp")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/LLNL/zfp/releases/download/" version
                      "/zfp-" version ".tar.gz"))
                (sha256
                 (base32
                  "1i0pnqm6qnwlp1chrjm4qhmqks1ddrdvlhfgp3w94g0fwpiqm80f"))))
      (inputs (list python python-numpy python-cython))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags `("-DBUILD_ZFPY=YES")))
      (home-page "https://computing.llnl.gov/projects/zfp")
      (synopsis "zfp: Compressed Floating-Point and Integer Arrays")
      (description "zfp: Compressed Floating-Point and Integer Arrays")
      (license licenses:bsd-3))))

(define-public std_compat
  (let* ((version "0.0.16")
         (commit "5ac9cb295cf7988df297a06c434504a50647b0b4"))
    (package
      (name "std_compat")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/robertu94/std_compat")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11kvdk1s4qy0mffma0vfgr407pdn4j1rps495n6pnl6pdw29gyjy"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags `("-DBUILD_TESTING=NO")
         #:phases (modify-phases %standard-phases
                    (delete 'check))))
      (home-page "https://github.com/robertu94/std_compat")
      (synopsis "compatability header for older c++")
      (description "compatability header for older c++")
      (license #f))))

(define-public sz
  (let* ((version "2.1.12.5"))
    (package
      (name "sz")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/szcompressor/SZ/releases/download/v"
                      version "/SZ-" version ".tar.gz"))
                (sha256
                 (base32
                  "173jm1dhmkalwxxwxzysr6jrhi1f7798j0rhfykmd481yvd21a1j"))))
      (inputs (list python python-numpy python-cython zlib zstd-cmake))
      (build-system cmake-build-system)
      (home-page "https://github.com/szcompressor/SZ")
      (synopsis "SZ2: Error-bounded Lossy Compressor for HPC Data")
      (description "SZ2: Error-bounded Lossy Compressor for HPC Data")
      (license #f))))

(define-public SZauto
  (let* ((version "1.2.1"))
    (package
      (name "SZauto")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/szcompressor/SZauto/releases/download/"
                      version "/SZauto-" version ".tar.gz"))
                (sha256
                 (base32
                  "0fmh141swmfqsaghi32rwl0hbpq7g4jajr7q9rl4z1rsvxc8ziam"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake))
      (home-page "https://github.com/szcompressor/SZauto")
      (synopsis
       "SZauto: SZ C++ Version that Supports Second-Order Prediction and Parameter Optimization")
      (description
       "SZauto: SZ C++ Version that Supports Second-Order Prediction and Parameter Optimization")
      (license #f))))

(define-public SZ3
  (let* ((version "3.1.7"))
    (package
      (name "SZ3")
      (version version)
      (source (origin
                (method url-fetch)
                (patches (search-patches "sz3_zstd_cmake.patch"))
                (uri (string-append
                      "https://github.com/szcompressor/SZ3/archive/refs/tags/v"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "146klxxkwfkxpvj4a3z4rygarlcgdjjiycwmwn01xzwk0wqylz82"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake gsl))
      (home-page "https://github.com/szcompressor/SZ3")
      (synopsis
       "SZ3: A Modular Error-bounded Lossy Compression Framework for Scientific Datasets")
      (description
       "SZ3: A Modular Error-bounded Lossy Compression Framework for Scientific Datasets")
      (license #f))))

(define-public digitroundingZ
  (let* ((version "0.1")
         (commit "68555fade9ecb1b123f29436461e02f7acb4f738"))
    (package
      (name "digitroundingZ")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/disheng222/digitroundingZ")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1i8l41k6mm9hfipamhdm1sw35zxwcfh7p64h3chdsq37023hvyl6"))))
      (build-system cmake-build-system)
      (inputs (list zlib))
      (home-page "https://github.com/disheng222/digitroundingZ")
      (synopsis
       "The standalone digit rounding compressor which will be convenient for evaluation")
      (description
       "The standalone digit rounding compressor which will be convenient for evaluation")
      (license licenses:lgpl3))))

(define-public bitgroomingZ
  (let* ((version "0.1")
         (commit "4816b7f1b92765cac57bd01cd4e3cde1b8bdb65f"))
    (package
      (name "bitgroomingZ")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/disheng222/bitgroomingZ")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1yab8x15067z97559azqns7v57ldzsawgnvyr1nd80qzivyxmbs6"))))
      (build-system cmake-build-system)
      (inputs (list zlib))
      (home-page "https://github.com/disheng222/BitGroomingZ")
      (synopsis "BGZ: Bit Grooming Compressor")
      (description "BGZ: Bit Grooming Compressor")
      (license #f))))

(define-public mgard
  (let* ((version "1.5.0")
         (commit "0f6cdf9b59e837547e3298be7187de8df376bb18"))
    (package
      (name "mgard")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/CODARcode/MGARD")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0382ijm5pc8cvrkqzx9hv4g8pzk2r5vbn6gih374r3as04zj9swj"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake zlib python protobuf pkg-config))
      (arguments
       `(#:configure-flags '("-DMGARD_ENABLE_CLI=YES")))
      (home-page "https://github.com/CODARcode/MGARD")
      (synopsis "MGARD: MultiGrid Adaptive Reduction of Data")
      (description "MGARD: MultiGrid Adaptive Reduction of Data")
      (license #f))))

(define-public mgard-0.1
  (let* ((version "0.1.0")
         (commit "7d9715e612488731dfa5f8e488e7976539464c3e"))
    (package
      (name "mgard")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/CODARcode/MGARD")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pyk3p3q2jamc0i84g9pg4m5qgk8ic7b90y1j1fmfqqygb7whzim"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake zlib python protobuf pkg-config))
      (arguments
       `(#:configure-flags '("-DMGARD_ENABLE_CLI=YES")
         #:phases (modify-phases %standard-phases
                    (delete 'check))))
      (home-page "https://github.com/CODARcode/MGARD")
      (synopsis "MGARD: MultiGrid Adaptive Reduction of Data")
      (description "MGARD: MultiGrid Adaptive Reduction of Data")
      (license #f))))

(define-public ndzip
  (let* ((version "0.1")
         (commit "ff4e6702bf0abb86d4aeef8249bd30344dfeef75"))
    (package
      (name "ndzip")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/fknorr/ndzip")
                      (commit commit)))
                (file-name (git-file-name name version))
                (patches (search-patches "ndzip_install.patch"))
                (sha256
                 (base32
                  "0x58gglrw4fff2n31kda2il2rbpr68nng4i03jaf59i20nccjb0a"))))
      (build-system cmake-build-system)
      (inputs (list boost))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'check))))
      (home-page "https://github.com/fknorr/ndzip")
      (synopsis
       "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (description
       "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (license licenses:expat))))

(define-public ndzip-old
  (let* ((version "0.1")
         (commit "4e6e38e40af7f44fda05569a976445b226275997"))
    (package
      (name "ndzip")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/fknorr/ndzip")
                      (commit commit)))
                (file-name (git-file-name name version))
                (patches (search-patches "ndzip_install_old.patch"))
                (sha256
                 (base32
                  "0iahng8k6mhdg2xf3ric5zv2wdhcffz9sjvlix7v4cxixl846xi0"))))
      (build-system cmake-build-system)
      (inputs (list boost))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'check))))
      (home-page "https://github.com/fknorr/ndzip")
      (synopsis
       "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (description
       "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (license licenses:expat))))

(define-public fpzip
  (let* ((version "1.3.0"))
    (package
      (name "fpzip")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/LLNL/fpzip/releases/download/"
                      version "/fpzip-" version ".tar.gz"))
                (sha256
                 (base32
                  "0v0ky3sdqwg13k2zvy786kbzrhvp59mrb5s79jmgxqsr8bcgg394"))))
      (build-system cmake-build-system)
      (home-page "https://github.com/LLNL/fpzip")
      (synopsis
       "Lossless compressor of multidimensional floating-point arrays")
      (description
       "Lossless compressor of multidimensional floating-point arrays")
      (license licenses:bsd-3))))

(define-public sol2
  (let* ((version "3.2.2"))
    (package
      (name "sol2")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/ThePhD/sol2/archive/refs/tags/v"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "1nmjlfqfi3fqqwzgqpl48ljsg6z47m1raddcvg91v0n1w3d905ql"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'check))))
      (home-page "https://github.com/ThePhD/sol2")
      (synopsis "sol2 is a C++ library binding to Lua")
      (description "sol2 is a C++ library binding to Lua")
      (license licenses:expat))))

(define-public zstd-cmake
  (package
    (inherit zstd)
    (build-system cmake-build-system)
    (outputs '("out"))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check)
                  (add-before 'configure 'chdir
                    (lambda* _
                      (chdir "build/cmake") #t)))))))

(define-public libpressio
  (let* ((version "0.94.0"))
    (package
      (name "libpressio")
      (version version)
      (source (origin
                (modules '((guix build utils)))
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/robertu94/libpressio")
                      (commit version)))
                (file-name (git-file-name name version))
                (patches (search-patches
                          "libpressio_python_install_path.patch"))
                (snippet '(begin
                            (substitute* "CMakeLists.txt"
                              (("CMP0069 NEW)")
                               "SET CMP0069 NEW)\nfind_package(\"zstd\")\n")
                              (("NDZip")
                               "ndzip")) #t))
                (sha256
                 (base32
                  "0ld4fvcc8q03wdp8vz2qfzxqssg5ss3av0sdh6qlg1rlz78lp31y"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags `("-DBUILD_TESTING=NO"
                             "-DLIBPRESSIO_INTERPROCEDURAL_OPTIMIZATION=YES"
                             "-DLIBPRESSIO_HAS_OPENMP=YES"
                             "-DLIBPRESSIO_HAS_SZ_AUTO=YES"
                             "-DLIBPRESSIO_HAS_ZFP=YES"
                             "-DLIBPRESSIO_HAS_SZ=YES"
                             "-DLIBPRESSIO_HAS_BLOSC=YES"
                             "-DLIBPRESSIO_HAS_MAGICK=YES"
                             "-DLIBPRESSIO_HAS_HDF=YES"
                             "-DLIBPRESSIO_HAS_FPZIP=YES"
                             "-DLIBPRESSIO_HAS_PETSC=YES"
                             "-DLIBPRESSIO_HAS_LUA=YES"
                             "-DLIBPRESSIO_HAS_JSON=YES"
                             "-DBUILD_PYTHON_WRAPPER=YES"
                             "-DLIBPRESSIO_HAS_DIGIT_ROUNDING=YES"
                             "-DLIBPRESSIO_HAS_BIT_GROOMING=YES"
                             "-DLIBPRESSIO_HAS_LINUX=YES"
                             "-DLIBPRESSIO_BUILD_MODE=FULL"
                             "-DLIBPRESSIO_HAS_BZIP2=YES"
                             "-DLIBPRESSIO_HAS_SZ3=YES"
                             "-DLIBPRESSIO_HAS_NETCDF=YES"
                             "-DLIBPRESSIO_HAS_NDZIP=YES"
                             "-DLIBPRESSIO_HAS_MGARD=NO")
         #:phases (modify-phases %standard-phases
                    (delete 'check))))
      (inputs (list pkg-config
                    std_compat
                    boost
                    zfp
                    c-blosc
                    hdf5
                    imagemagick
                    sz
                    digitroundingZ
                    zlib
                    bitgroomingZ
                    SZauto
                    zstd-cmake
                    protobuf
                    SZ3
                    ndzip-old
                    netcdf
                    fpzip
                    sol2
                    luajit
                    json-modern-cxx
                    python-numpy
                    python
                    swig
                    petsc
                    gsl))
      (home-page "https://github.com/CODARcode/libpressio")
      (synopsis
       "LibPressio is a C++ library with C compatible bindings to abstract between different lossless and lossy compressors and their configurations")
      (description
       "LibPressio is a C++ library with C compatible bindings to abstract between different lossless and lossy compressors and their configurations")
      (license #f))))

(define-public ffmpeg-with-rubberband
  (package
    (inherit ffmpeg)
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:configure-flags flags)
        `(cons "--enable-librubberband"
          ,flags))))
    (inputs (modify-inputs (package-inputs ffmpeg)
              (append rubberband)))))

(define-public mpv-with-rubberband
  (package
    (inherit mpv)
    (name "mpv-rubberband")
    (inputs (modify-inputs (package-inputs mpv)
              (replace "ffmpeg" ffmpeg-with-rubberband)))))

(define-public advancecomp
  (let* ((version "2.5"))
    (package
      (name "advancecomp")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/amadvance/advancecomp/releases/download/v"
                      version "/advancecomp-" version ".tar.gz"))
                (sha256
                 (base32
                  "01vw669rslf84nnqbbfbfpiyk3ii485qbgl5irjp0ivv72nyrf4h"))))
      (build-system gnu-build-system)
      (inputs (list zlib))
      (home-page "https://www.advancemame.it/comp-readme.html")
      (synopsis
       "AdvanceCOMP is a collection of recompression utilities for your .ZIP archives, .PNG snapshots, .MNG video clips and .GZ files.")
      (description
       "AdvanceCOMP is a collection of recompression utilities for your .ZIP archives, .PNG snapshots, .MNG video clips and .GZ files.")
      (license licenses:gpl2))))

(define-public bash-preexec
  (let* ((version "0.5.0"))
    (package
      (name "bash-preexec")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/rcaloras/bash-preexec/archive/refs/tags/"
                      version ".tar.gz"))
                (sha256
                 (base32
                  "0z479chg7i6j9apwqs1f3fx345mi3gcciyljixcw02d23p6qki93"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("bash-preexec.sh" "bash-preexec.sh"))))
      (home-page "https://github.com/rcaloras/bash-preexec")
      (synopsis "preexec and precmd functions for Bash just like Zsh.")
      (description "preexec and precmd functions for Bash just like Zsh.")
      (license licenses:expat))))


(define-public xschem-newer
  (package
    (inherit xschem)
    (version "3.4.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/StefanSchippers/xschem")
                    (commit version)))
              (file-name (git-file-name "xschem" version))
              (sha256
               (base32
                "0w8dpi7phcgfpjkc7shm61hjk5v95vya2bh76ql6jphf8hg30a7v"))))
    (inputs (modify-inputs (package-inputs xschem)
              (append readline)
              (append libjpeg-turbo)))))


(define-public ngspice-git
  (let ((commit "b2c0f85c6d62433a651c9cff75668501199f43bb"))
    (package
      (inherit ngspice)
      (version (string-append (package-version ngspice) "-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.code.sf.net/p/ngspice/ngspice")
               (commit commit)))
         (file-name (git-file-name "ngspice" commit))
         (sha256
          (base32
           "0rqdwfq280fdhm1cz0sir25ihhn2b52ljwqi6y1h80ln805z07k6"))))
      (native-inputs (modify-inputs (package-native-inputs ngspice)
                       (append autoconf automake libtool perl)))
      (arguments
       (substitute-keyword-arguments (package-arguments ngspice)
         ((#:phases phases)
          `(modify-phases ,phases
            (delete 'delete-scripts)
            (add-before 'delete-include-files 'reconf
              (lambda _
                (invoke "autoreconf" "-vfi")))
            )))))))


(define-public magic
  (let* ((version "8.3.421"))
    (package
      (name "magic")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/RTimothyEdwards/magic")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1aizkx5s4h0bbnsgpwya51mwc5sbqvm0xgfp92486nhhzvjjg03s"))))
      (build-system gnu-build-system)
      (inputs (list tcsh python libx11 mesa cairo tcl glu tk git))
      (home-page "http://opencircuitdesign.com/magic/")
      (arguments
       (list #:configure-flags
             #~(list
                (string-append "--with-tcl="
                               (assoc-ref %build-inputs "tcl"))
                (string-append "--with-tk="
                               (assoc-ref %build-inputs "tk")))
             #:tests? #f))
      (synopsis
       "Magic VLSI Layout Tool")
      (description
       "Magic VLSI Layout Tool")
      (license #f))))


;; X11:          no

;; Magic requires X11 for all graphics operations.  Without it,
;; magic can only be run in batch mode using option '-dnull'.
;; Generally, not finding X11 means that paths to header files
;; and/or libraries are missing or in an unexpected place.  Try
;; using configure options --x-includes=<DIR> and --x-libraries=<DIR>.
;;
;; Python3:      no
;;
;; Magic installation will use the gcc preprocessor for forming
;; the default SCMOS technology files and the macro definitions.
;; This usually works, but in case of preprocessor failure, you
;; may need python3 installed.
;;
;; OpenGL:       no
;;
;; OpenGL graphics are considerably better than the standard 8-bit
;; and 24-bit X11 graphics, provided that you have a video card and
;; driver supporting accelerated OpenGL graphics.  If you get this
;; message, you may need to download OpenGL libraries and header
;; files, which are usually available from the video card manufacturer.
;; Magic with un-accelerated OpenGL, such as using Mesa GL without
;; a supported graphics card, is usually a very bad combination.
;;
;; Cairo:        no
;;
;; Cairo graphics are considerably better than the standard 8-bit
;; and 24-bit X11 graphics, provided that you have a video card and
;; driver supporting hardware-accelerated graphics.  If you get this
;; message, you may need to download Cairo and fontconfig libraries
;; and header files, which are usually found in package cairo-devel.
;;
;; Tcl/Tk:       no
;;
;; Without Tcl/Tk, you cannot run the enhanced version of magic
;; with the toolbar and menus, and a number of other functions
;; are disabled.  If you did not specifically disable Tcl/Tk on
;; the configure command line, then getting this message means
;; that you do not have Tcl/Tk headers and or libraries installed,
;; or they are not in a standard path.  Try using configure options
;; --with-tcl=<DIR> and --with-tk=<DIR>.

(define-public gtk4-layer-shell
  (package
    (name "gtk4-layer-shell")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wmww/gtk4-layer-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ha347f0dcnmmhxbvhpav6yiq6b4jnj8gzn1afmznfyjahsvg1ni"))))
    (build-system meson-build-system)
    (arguments `(#:tests? #f #:configure-flags (list "-Dtests=true")))
    (native-inputs (list pkg-config gobject-introspection vala))
    (inputs (list gtk))
    (home-page "https://github.com/wmww/gtk4-layer-shell")
    (synopsis "A library to create panels and other desktop components for Wayland using the Layer Shell protocol and GTK4")
    (description "A library to create panels and other desktop components for Wayland using the Layer Shell protocol and GTK4")
    (license #f)))

(define-public umr
  (let ((commit "39c91cdee3804ea209dc21619087212bc75d267e"))
    (package
      (name "umr")
      (version (string-append "0.1" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.freedesktop.org/tomstdenis/umr")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jwp45z24rj9lsq0wmmmxc50rmxp50qxz0gq11hwn6cg71kf9b48"))))
      (build-system cmake-build-system)
      (arguments `(#:tests? #f))
      (inputs (list ncurses nanomsg libglvnd pkg-config libpciaccess libdrm llvm-15 mesa sdl2 bash-completion))
      (synopsis "User Mode Register Debugger for AMDGPU Hardware")
      (description "User Mode Register Debugger for AMDGPU Hardware")
      (home-page "https://gitlab.freedesktop.org/tomstdenis/umr")
      (license #f))))


(define-public glfw-3.4
  (package
    (inherit glfw)
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/glfw/glfw"
                                  "/releases/download/" version
                                  "/glfw-" version ".zip"))
              (sha256
               (base32
                "1sd396kkn53myp61kxrd18h7b1q4ix173hhxhvl0iz8j4x5h1v5m"))))
    (native-inputs (modify-inputs (package-native-inputs glfw)
                     (prepend pkg-config)))
    ;; When building out of source, the install phase fails with:
    ;;  file INSTALL cannot find "/tmp/guix-build-glfw-3.4.drv-0/build/docs/html":
    ;;  No such file or directory
    (arguments (substitute-keyword-arguments (package-arguments glfw)
                 ((#:out-of-source? _ #f) #f)
                 ((#:configure-flags flags) #~(cons* "_DCMAKE_TESSESES=reiea" #$flags))))))

(define-public folly-tunable
  (package
    (name "folly-tunable")
    (version "2024.09.09.00")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebook/folly")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17fdigkaxivbrww5yhz9fh25d8pirqjp126zbv4kg4qsprywfww5"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Tests must be explicitly enabled
      ;;#:configure-flags #~(list "-DBUILD_TESTS=ON")
      ;; Leave tests disabled; see https://github.com/facebook/folly/issues/2246
      #:tests? #f
      #:configure-flags #~(list "-DFOLLY_SUPPORT_SHARED_LIBRARY=ON" "-DCMAKE_POSITION_INDEPENDENT_CODE=ON" "-DFOLLY_NO_EXCEPTION_TRACER=ON")))
    (propagated-inputs
     (list boost gflags glog liburing))
    (inputs
     (list bzip2
           double-conversion
           fast-float
           fmt
           libaio
           libevent
           libiberty
           libsodium
           libunwind
           lz4
           openssl
           snappy
           zlib
           `(,zstd "lib")))
    (native-inputs
     (list googletest))
    (synopsis "Collection of C++ components complementing the standard library")
    (description
     "Folly (acronymed loosely after Facebook Open Source Library) is a library
of C++14 components that complements @code{std} and Boost.")
    (home-page "https://github.com/facebook/folly/wiki")
    ;; 32-bit is not supported: https://github.com/facebook/folly/issues/103
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (properties `((tunable? . #t)))
    (license licenses:asl2.0)))
