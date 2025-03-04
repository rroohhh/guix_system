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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages flex)
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
  #:use-module (gnu packages bison)
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
  #:use-module (gnu packages image))

(define-public mbuffer
  (package
    (name "mbuffer")
    (version "20241007")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.maier-komor.de/software/mbuffer/mbuffer-" version
             ".tgz"))
       (sha256
        (base32 "1vbbsmqxfyqw517bhbshkhh6mfs9gc1gbdgn8qb5px2f1c0n6wwx"))))
    (build-system gnu-build-system)
    (native-inputs (list which))
    (arguments
     '(#:tests? #f))
    (home-page "https://www.maier-komor.de/mbuffer.html")
    (synopsis
     "mbuffer is a tool for buffering data streams with a large set of unique features")
    (description
     "mbuffer is a tool for buffering data streams with a large set of unique features")
    (license licenses:gpl3)))

(define-public libmodbus
  (package
    (name "libmodbus")
    (version "3.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/stephane/libmodbus/releases/download/v3.1.11/libmodbus-3.1.11.tar.gz"))
       (sha256
        (base32 "1rw0swadqy13hb99ajhbsjbrr6j8631yap8rkgdc48l1yvhb5d0m"))))
    (arguments
     '(#:tests? #f))
    (build-system gnu-build-system)
    (home-page "https://libmodbus.org/")
    (synopsis "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Win32")
    (description
     "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Win32")
    (license licenses:gpl3)))

(define-public mbpoll
  (package
    (name "mbpoll")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/epsilonrt/mbpoll/archive/v1.5.2.tar.gz"))
       (sha256
        (base32 "1v5zcgxhqm0y5d0xxlbmzla622x2jfx1midb28j7qpwv8pa0r5kx"))))
    (build-system cmake-build-system)
    (inputs `(("libmodbus" ,libmodbus)
              ("pkg-config" ,pkg-config)))
    (arguments
     '(#:tests? #f))
    (home-page "https://github.com/epsilonrt/mbpoll")
    (synopsis
     "mbpoll is a command line utility to communicate with ModBus slave (RTU or TCP).")
    (description
     "mbpoll is a command line utility to communicate with ModBus slave (RTU or TCP).")
    (license licenses:gpl3)))

(define-public antpm
  (package
    (name "antpm")
    (version "1.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ralovich/antpm/archive/v1.23.tar.gz"))
       (sha256
        (base32 "0vlzk8wjqhir1mzzvi6yhmg3ayd4w18fc2cqf258hjdrsl8rl1f8"))))
    (build-system cmake-build-system)
    (inputs (list libusb boost libxml2))
    (native-inputs (list pkg-config))
    (arguments
     '(#:tests? #f
       #:configure-flags `("-DUSE_BOOST_STATIC_LINK=False")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'cd-to-src
                    (lambda _
                      (chdir "src") #t)))))
    (home-page "https://github.com/ralovich/antpm")
    (synopsis "ANT+minus (ANT / ANT+ / ANT-FS)")
    (description "ANT+minus (ANT / ANT+ / ANT-FS)")
    (license licenses:gpl3)))

(define-public rdfind
  (package
    (name "rdfind")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pauldreik/rdfind")
             (commit "releases/1.7.0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f6h2cxgrx8cjfsl3g5vqw4ljsf8s3zkq27nmndk1l64a3gr1ygq"))))
    (build-system gnu-build-system)
    (native-inputs (list automake autoconf autoconf-archive which))
    (inputs (list nettle))
    (arguments
     '(#:tests? #f))
    (home-page "https://github.com/pauldreik/rdfind")
    (synopsis "find duplicate files utility")
    (description "find duplicate files utility")
    (license licenses:gpl2+)
    (properties '((release-tag-prefix . "^releases/")))))

(define-public xrestop
  (package
    (name "xrestop")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://xorg.freedesktop.org/archive/individual/app/xrestop-"
             version ".tar.xz"))
       (sha256
        (base32 "0a9v40fq6imh9phvh0z98qcjcssgmz3kwaywq9fqnydjqh8w2bif"))))
    (build-system gnu-build-system)
    (inputs (list libxres libx11 libxext ncurses))
    (native-inputs (list pkg-config))
    (home-page "http://freedesktop.org/wiki/Software/xrestop")
    (synopsis "Uses the X-Resource extension to provide 'top' like statistics")
    (description
     "Uses the X-Resource extension to provide 'top' like statistics")
    (license licenses:gpl2+)))

(define-public libsigrokdecode-master
  (package
    (inherit libsigrokdecode)
    (version "0.5.3-0.71f4514")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://sigrok.org/libsigrokdecode")
             (commit "71f451443029322d57376214c330b518efd84f88")
             (recursive? #t)))
       (file-name (git-file-name "libsigrokdecode" version))
       (sha256
        (base32 "11l8vnf2khqbaqas7cfnq3f8q5w7am6nbkkd5mqj5kpb3ya2avb9"))))
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
                     (append autoconf automake libtool)))))

(define-public libsigrok-master
  (package
    (inherit libsigrok)
    (version "0.5.2-2.f06f788")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://sigrok.org/libsigrok.git")
             (commit "f06f788118191d19fdbbb37046d3bd5cec91adb1")
             (recursive? #t)))
       (file-name (git-file-name "libsigrok" version))
       (sha256
        (base32 "1ahgpa0gaa4fl8c6frpgamvgxg0fisfwlqddr5x25456vkk2i9zi"))))
    (inputs (append (package-inputs libsigrok)
                    `(("glibmm" ,glibmm))))
    (native-inputs (append (package-native-inputs libsigrok)
                           `(("autoconf" ,autoconf)
                             ("automake" ,automake)
                             ("libtool" ,libtool))))))

(define-public pulseview-libsigrok-master
  (package
    (inherit ((package-input-rewriting/spec `(("libsigrok" unquote
                                               (const libsigrok-master))
                                              ("libsigrokdecode" unquote
                                               (const libsigrokdecode-master))))
              pulseview))
    (version "0.4.2-1.32ca796")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://sigrok.org/pulseview.git")
             (commit "32ca796643c62bfc8718ea5628cc7fb04b03f9e0")))
       (file-name (git-file-name (package-name pulseview) version))
       (sha256
        (base32 "1cc8zbym5jw15md264fjrrr8fqjjk8ww1009wkn0xcvlbk4p841d"))))
    (inputs (append (package-inputs pulseview)
                    `(("glibmm" ,glibmm))))))

(define ell-0.72
  (package
    (inherit ell)
    (version "0.72")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/libs/ell/ell.git")
             (commit version)))
       (file-name (git-file-name (package-name ell) version))
       (sha256
        (base32 "1m51mqlh2gv5v85mdhdx5wlinjmdg1da1263p18xnl74bm1rwzsb"))))))

(define-public ofono
  (package
    (name "ofono")
    (version "2.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.kernel.org/pub/scm/network/ofono/ofono.git")
             (commit "2.15")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g7h9bj1z7dwnhf1fr9zz14jf5qr0xxrwhgidq7qnsq4a0fs53sd"))
       (patches `("0001-Search-connectors-in-OFONO_PLUGIN_PATH.patch"))))
    (build-system gnu-build-system)
    (inputs (list nettle
                  glib
                  dbus
                  ell-0.72
                  eudev
                  mobile-broadband-provider-info
                  bluez))
    (native-inputs (list automake
                         libtool
                         autoconf
                         autoconf-archive
                         pkg-config
                         python))
    (arguments
     '(#:tests? #f ;has no tests
       #:configure-flags (list "--enable-external-ell"
                               (string-append "--with-dbusconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc")
                               (string-append "--with-dbusdatadir="
                                              (assoc-ref %outputs "out")
                                              "/share"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gstreamer.freedesktop.org/src/"
                           name
                           "/"
                           name
                           "-"
                           version
                           ".tar.xz"))
       ;; (patches (list (origin
       ;; (method url-fetch)
       ;; (uri
       ;; "https://raw.githubusercontent.com/rroohhh/guix_packages/e99ecbb/gstreamer_vaapi.patch")
       ;; (sha256
       ;; "06sk93zy9ddq3iynswjjiq4gv7kn5qgy5rnygjld34jxvmp2gyl6"))))
       (sha256
        (base32 "0b5kqw2x6nrrjin7awkx7rq8ncbwxl9kyzavr9gk0scb5q36fy52"))))
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
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer library for vaapi")
    (description
     "Hardware-accelerated video decoding, encoding and processing on Intel graphics through VA-API")
    (license licenses:gpl2+)))

(define-public kicad-nightly
  (package
    (inherit kicad)
    (name "kicad-nightly")
    (version "9.0.0-rc3-1.aafe615")
    (inputs (modify-inputs (package-inputs kicad)
              (append libglvnd)
              (append zstd-cmake)
              (append libgit2)
              (append protobuf)
              (append nng)
              (append libsecret)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/kicad/code/kicad.git")
             (commit "aafe6157e21f0a5de24e46937f7e18e97f36cd09")))
       (sha256
        (base32 "12qbp3pcxj79c157q6izwlmi327wz7127b06xcgfj6pknqripxxg"))
       (file-name (git-file-name name version))))))

(define-public gst-rtsp-server
  (package
    (name "gst-rtsp-server")
    (version (package-version gstreamer))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gstreamer.freedesktop.org/src/"
                           name
                           "/"
                           name
                           "-"
                           version
                           ".tar.xz"))
       (sha256
        (base32 "07anzhi1gc6hw8khryrnzqny8g1bjhbqwdrdvnirkgxxm7gkjanh"))))
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
  (package
    (name "ayatana-ido3")
    (version "0.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AyatanaIndicators/ayatana-ido")
             (commit "0.10.4")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1li6pzh9xvl66nlixzpwqsn8aaa0kj0azhzjan4cd65f7nnjpq99"))))
    (build-system cmake-build-system)
    (inputs (list glib gtk+ gobject-introspection))
    (native-inputs (list pkg-config googletest vala
                         `(,glib "bin")))
    (arguments
     `(#:tests? #f))
    (synopsis "Ayatana Indicator Display Objects")
    (description "Ayatana Indicator Display Objects")
    (home-page "https://github.com/AyatanaIndicators/ayatana-ido")
    (license licenses:lgpl3)))

(define-public libayatana-indicator
  (package
    (name "libayatana-indicator")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AyatanaIndicators/libayatana-indicator")
             (commit "0.9.4")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c0pymlpxabh7iackv6i47gh81b7pxx194r07lpbxnz5x1kjxj1s"))))
    (build-system cmake-build-system)
    (propagated-inputs (list ayatana-ido3))
    (inputs (list glib gtk+))
    (native-inputs (list pkg-config
                         `(,glib "bin")))
    (arguments
     `(#:tests? #f
       #:configure-flags `("-DENABLE_LOADER=OFF")))
    (synopsis "Ayatana Indicators Shared Library")
    (description "Ayatana Indicators Shared Library")
    (home-page "https://github.com/AyatanaIndicators/libayatana-indicator")
    (license licenses:gpl3)))

(define-public libayatana-appindicator
  (package
    (name "libayatana-appindicator")
    (version "0.5.93")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/AyatanaIndicators/libayatana-appindicator")
             (commit "0.5.93")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zx61qclgrxqvq7p38anhab9s2hqmnnn1ydbn4x2db1k87x5rb98"))))
    (build-system cmake-build-system)
    (propagated-inputs (list libdbusmenu libayatana-indicator))
    (inputs (list glib gtk+ gobject-introspection))
    (native-inputs (list pkg-config vala
                         `(,glib "bin")))
    (arguments
     `(#:tests? #f
       #:configure-flags `("-DENABLE_BINDINGS_MONO=NO")))
    (synopsis "Ayatana Application Indicators Shared Library")
    (description "Ayatana Application Indicators Shared Library")
    (home-page "https://github.com/AyatanaIndicators/libayatana-appindicator")
    (license licenses:lgpl3)))

(define-public whatsapp-for-linux
  (package
    (name "whatsapp-for-linux")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eneshecan/whatsapp-for-linux")
             (commit "v1.7.0")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i2n4fxrach09c2dhq1gj8602j5slhhb8hsmfdr0p7rfgrzx0kl7"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))
    (inputs (list gtkmm-3 webkitgtk-with-libsoup2 libayatana-appindicator
                  libcanberra))
    (native-inputs (list pkg-config gnu-gettext
                         `(,glib "bin")))
    (synopsis "An unofficial WhatsApp desktop application for Linux.")
    (description "An unofficial WhatsApp desktop application for Linux.")
    (home-page "https://github.com/eneshecan/whatsapp-for-linux")
    (license licenses:gpl3)))

(define-public glslang-upstream
  (package
    (inherit glslang)
    (version "15.1.0")
    (arguments
     (append (list #:cmake cmake-3.30)
             (substitute-keyword-arguments (package-arguments glslang)
               ((#:configure-flags flags)
                `(cons "-DENABLE_GLSLANG_INSTALL=Yes"
                       ,flags)))))
    (inputs (modify-inputs (package-inputs glslang)
              (replace "spirv-tools" spirv-tools-upstream)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/glslang")
             (commit version)))
       (sha256
        (base32 "02nykvsn059jxspf8i49m5krgvrjlp9wa88yrwl6lj6pzsfmwmhk"))
       (file-name (git-file-name (package-name glslang) version))))))

(define-public spirv-headers-upstream
  (package
    (inherit spirv-headers)
    (version "1.4.304.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Headers")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32 "1hlmjhavc6lyw39visr93rq4frrqxd785h7zci8pr3m6vj5kw91h"))
       (file-name (git-file-name (package-name spirv-headers) version))))
    (properties '((release-tag-prefix . "^vulkan-sdk-")))))

(define-public spirv-tools-upstream
  (package
    (inherit spirv-tools)
    (version "1.4.304.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Tools")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32 "08ipi93bi9idp8rvgmsv2l8k0gdjvnc7cid49q8knkwvp9gphlka"))
       (file-name (git-file-name (package-name spirv-tools) version))))
    (inputs (modify-inputs (package-inputs spirv-tools)
              (replace "spirv-headers" spirv-headers-upstream)))
    (arguments
     (substitute-keyword-arguments (package-arguments spirv-tools)
       ((#:configure-flags flags)
        `(cons "-DSPIRV_WERROR=No"
               ,flags))))
    (properties '((release-tag-prefix . "^vulkan-sdk-")))))

(define-public shaderc-upstream
  (package
    (inherit shaderc)
    (version "2024.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shaderc")
             (commit (string-append "v" version))))
       (file-name (git-file-name "shaderc" version))
       (sha256
        (base32 "0wjjnmzvlm86aqsaid7pb1kx9hxrj9x6g0pv8f450rh04qg612hc"))))
    (inputs (modify-inputs (package-inputs shaderc)
              (replace "glslang" glslang-upstream)
              (replace "spirv-tools" spirv-tools-upstream)
              (replace "spirv-headers" spirv-headers-upstream)))))

(define-public vulkan-headers-upstream
  (package
    (inherit vulkan-headers)
    (version "ulkan-sdk-1.4.304.1")
    (name (package-name vulkan-headers))
    (source
     (origin
       (inherit (package-source vulkan-headers))
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Headers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "158id9qgsjgsjcq9d67359incs6jc8n9bkvd0h399qfrj1qym8az"))))))

(define-public vulkan-hpp
  (package
    (name "vulkan-hpp")
    (version "1.4.307")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Hpp")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q318qq0vgcaidlfhl0fw1ggfn8m6m2cjmlilq308r7px148aww0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-w-error
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        (("-Werror")
                         "")) #t)))
       #:configure-flags (list "-DVULKAN_HPP_INSTALL=Yes"
                               (string-append "-DVulkanHeaders_INCLUDE_DIR=.")
                               (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                                              (assoc-ref %outputs "out")
                                              "/include"))))
    (home-page "https://github.com/KhronosGroup/Vulkan-Hpp")
    (synopsis "Open-Source Vulkan C++ API")
    (description "Vulkan-Hpp: C++ Bindings for Vulkan")
    (license licenses:asl2.0)))

(define-public zfp
  (package
    (name "zfp")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/LLNL/zfp/releases/download/1.0.1/zfp-1.0.1.tar.gz"))
       (sha256
        (base32 "0az5c2g5i16z4m0vf3dvypc3vs1zf8qavbigl7fzyi40x157n3ya"))))
    (inputs (list python python-numpy python-cython))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags `("-DBUILD_ZFPY=YES")))
    (home-page "https://computing.llnl.gov/projects/zfp")
    (synopsis "zfp: Compressed Floating-Point and Integer Arrays")
    (description "zfp: Compressed Floating-Point and Integer Arrays")
    (license licenses:bsd-3)))

(define-public std_compat
  (package
    (name "std_compat")
    (version "0.0.21-0.76b61ae")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/robertu94/std_compat")
             (commit "76b61aed61bff2a468949f9c620c3a8900fc8d91")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066na845xykg5n0v5210jsxhdp9009f75lzy7fxg0ikxxs7pzw4r"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags `("-DBUILD_TESTING=NO")
       #:tests? #f))
    (home-page "https://github.com/robertu94/std_compat")
    (synopsis "compatability header for older c++")
    (description "compatability header for older c++")
    (license #f)))

(define-public sz
  (package
    (name "SZ")
    (version "2.1.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/szcompressor/SZ/releases/download/v2.1.12.5/SZ-2.1.12.5.tar.gz"))
       (sha256
        (base32 "173jm1dhmkalwxxwxzysr6jrhi1f7798j0rhfykmd481yvd21a1j"))))
    (inputs (list python python-numpy python-cython zlib zstd-cmake))
    (build-system cmake-build-system)
    (home-page "https://github.com/szcompressor/SZ")
    (synopsis "SZ2: Error-bounded Lossy Compressor for HPC Data")
    (description "SZ2: Error-bounded Lossy Compressor for HPC Data")
    (license #f)))

(define-public SZauto
  (package
    (name "SZauto")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/szcompressor/SZauto/releases/download/1.2.1/SZauto-1.2.1.tar.gz"))
       (sha256
        (base32 "0fmh141swmfqsaghi32rwl0hbpq7g4jajr7q9rl4z1rsvxc8ziam"))))
    (build-system cmake-build-system)
    (inputs (list zstd-cmake))
    (home-page "https://github.com/szcompressor/SZauto")
    (synopsis
     "SZauto: SZ C++ Version that Supports Second-Order Prediction and Parameter Optimization")
    (description
     "SZauto: SZ C++ Version that Supports Second-Order Prediction and Parameter Optimization")
    (license #f)))

(define-public SZ3
  (package
    (name "SZ3")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (patches (search-patches "sz3_zstd_cmake.patch"))
       (uri (string-append
             "https://github.com/szcompressor/SZ3/releases/download/v3.2.1/SZ3-v3.2.1.zip"))
       (sha256
        (base32 "0mgfw3xkqlbyikcqflgj447yfsp8s0gm0fms3qrk6nxsfd5mc5cv"))))
    (build-system cmake-build-system)
    (inputs (list zstd-cmake gsl))
    (home-page "https://github.com/szcompressor/SZ3")
    (synopsis
     "SZ3: A Modular Error-bounded Lossy Compression Framework for Scientific Datasets")
    (description
     "SZ3: A Modular Error-bounded Lossy Compression Framework for Scientific Datasets")
    (license #f)))

(define-public digitroundingZ
  (package
    (name "digitroundingZ")
    (version "0.1-0.68555fa")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/disheng222/digitroundingZ")
             (commit "68555fade9ecb1b123f29436461e02f7acb4f738")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i8l41k6mm9hfipamhdm1sw35zxwcfh7p64h3chdsq37023hvyl6"))))
    (build-system cmake-build-system)
    (inputs (list zlib))
    (home-page "https://github.com/disheng222/digitroundingZ")
    (synopsis
     "The standalone digit rounding compressor which will be convenient for evaluation")
    (description
     "The standalone digit rounding compressor which will be convenient for evaluation")
    (license licenses:lgpl3)))

(define-public bitgroomingZ
  (package
    (name "bitgroomingZ")
    (version "0.0-1.046b8e9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/disheng222/bitgroomingZ")
             (commit "046b8e9bcbe0a43b99ed0d4d2f9529b62b36fa69")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f0givv9idcpa29yih4lr85gb1hh3ba0f375yvzh7521f65kl7w9"))))
    (build-system cmake-build-system)
    (inputs (list zlib))
    (home-page "https://github.com/disheng222/BitGroomingZ")
    (synopsis "BGZ: Bit Grooming Compressor")
    (description "BGZ: Bit Grooming Compressor")
    (license #f)))

(define-public mgard
  (package
    (name "mgard")
    (version "1.5.2-0.58f8d40")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CODARcode/MGARD")
             (commit "58f8d4064365014eb164c470696087aaaf40d0fc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09z1snq45rw65qlnkwap2l3qmnzh95dyrigv2mknrdlv99icmrn6"))))
    (build-system cmake-build-system)
    (inputs (list zstd-cmake zlib python protobuf pkg-config))
    (arguments
     `(#:configure-flags '("-DMGARD_ENABLE_CLI=YES")))
    (home-page "https://github.com/CODARcode/MGARD")
    (synopsis "MGARD: MultiGrid Adaptive Reduction of Data")
    (description "MGARD: MultiGrid Adaptive Reduction of Data")
    (license #f)))

(define-public ndzip
  (package
    (name "ndzip")
    (version "21.submission-0.ff4e670")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fknorr/ndzip")
             (commit "ff4e6702bf0abb86d4aeef8249bd30344dfeef75")))
       (file-name (git-file-name name version))
       (patches (search-patches "ndzip_install.patch"))
       (sha256
        (base32 "0x58gglrw4fff2n31kda2il2rbpr68nng4i03jaf59i20nccjb0a"))))
    (build-system cmake-build-system)
    (inputs (list boost))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/fknorr/ndzip")
    (synopsis
     "A High-Throughput Parallel Lossless Compressor for Scientific Data")
    (description
     "A High-Throughput Parallel Lossless Compressor for Scientific Data")
    (license licenses:expat)))

(define-public ndzip-old
  (let* ((version "0.1")
         (commit "4e6e38e40af7f44fda05569a976445b226275997"))
    (package
      (name "ndzip")
      (version (string-append version "-"
                              (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fknorr/ndzip")
               (commit commit)))
         (file-name (git-file-name name version))
         (patches (search-patches "ndzip_install_old.patch"))
         (sha256
          (base32 "0iahng8k6mhdg2xf3ric5zv2wdhcffz9sjvlix7v4cxixl846xi0"))))
      (build-system cmake-build-system)
      (inputs (list boost))
      (arguments
       `(#:tests? #f))
      (home-page "https://github.com/fknorr/ndzip")
      (synopsis
       "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (description
       "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (license licenses:expat))))

(define-public fpzip
  (package
    (name "fpzip")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/LLNL/fpzip/releases/download/1.3.0/fpzip-1.3.0.tar.gz"))
       (sha256
        (base32 "0v0ky3sdqwg13k2zvy786kbzrhvp59mrb5s79jmgxqsr8bcgg394"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/LLNL/fpzip")
    (synopsis "Lossless compressor of multidimensional floating-point arrays")
    (description
     "Lossless compressor of multidimensional floating-point arrays")
    (license licenses:bsd-3)))

(define-public sol2
  (package
    (name "sol2")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ThePhD/sol2")
             (commit "v3.3.0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g5q2ms9k97s78mh3kmdm11d50sb7rmdrppy6nc7n65mgicqh01l"))))
    (build-system cmake-build-system)
    (inputs (list lua))
    (arguments
     (list
      #:configure-flags
      #~(list "-DSOL2_BUILD_LUA=FALSE"
              (string-append "-DSOL2_LUA_VERSION="
                             #$(package-version (this-package-input "lua"))))
      #:tests? #f))
    (home-page "https://github.com/ThePhD/sol2")
    (synopsis "sol2 is a C++ library binding to Lua")
    (description "sol2 is a C++ library binding to Lua")
    (license licenses:expat)))

(define-public zstd-cmake
  (package
    (inherit zstd)
    (build-system cmake-build-system)
    (outputs '("out"))
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'chdir
                    (lambda* _
                      (chdir "build/cmake") #t)))))))

(define-public libpressio
  (package
    (name "libpressio")
    (version "1.0.4")
    (source
     (origin
       (modules '((guix build utils)))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/robertu94/libpressio")
             (commit "1.0.4")))
       (file-name (git-file-name name version))
       (patches (search-patches "libpressio_python_install_path.patch"))
       (snippet '(begin
                   (substitute* "CMakeLists.txt"
                     (("CMP0069 NEW)")
                      "SET CMP0069 NEW)\nfind_package(\"zstd\")\n")
                     (("NDZip")
                      "ndzip")) #t))
       (sha256
        (base32 "13wlngz5c8ryplakb3giiifpdxyqfzgabz1gs3w6ffbhnrlak69a"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags `("-DBUILD_TESTING=NO"
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
                           "-DLIBPRESSIO_HAS_MGARD=NO")))
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
    (native-inputs (list gcc-14))
    (home-page "https://github.com/CODARcode/libpressio")
    (synopsis
     "LibPressio is a C++ library with C compatible bindings to abstract between different lossless and lossy compressors and their configurations")
    (description
     "LibPressio is a C++ library with C compatible bindings to abstract between different lossless and lossy compressors and their configurations")
    (license #f)))

(define-public ffmpeg-with-rubberband
  (package
    (inherit ffmpeg)
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:configure-flags flags)
        #~(cons "--enable-librubberband"
                #$flags))))
    (inputs (modify-inputs (package-inputs ffmpeg)
              (append rubberband)))))

(define-public mpv-with-rubberband
  (package
    (inherit mpv)
    (name "mpv-rubberband")
    (inputs (modify-inputs (package-inputs mpv)
              (replace "ffmpeg" ffmpeg-with-rubberband)))))

(define-public advancecomp
  (package
    (name "advancecomp")
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/amadvance/advancecomp/releases/download/v2.6/advancecomp-2.6.tar.gz"))
       (sha256
        (base32 "13s9qf7ch3myj09g1n63p4dw2laibcbdzdp1rdqrfh20amrpfzdh"))))
    (build-system gnu-build-system)
    (inputs (list zlib))
    (home-page "https://www.advancemame.it/comp-readme.html")
    (synopsis
     "AdvanceCOMP is a collection of recompression utilities for your .ZIP archives, .PNG snapshots, .MNG video clips and .GZ files.")
    (description
     "AdvanceCOMP is a collection of recompression utilities for your .ZIP archives, .PNG snapshots, .MNG video clips and .GZ files.")
    (license licenses:gpl2)))

(define-public bash-preexec
  (package
    (name "bash-preexec")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rcaloras/bash-preexec/archive/refs/tags/0.5.0.tar.gz"))
       (sha256
        (base32 "0z479chg7i6j9apwqs1f3fx345mi3gcciyljixcw02d23p6qki93"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("bash-preexec.sh" "bash-preexec.sh"))))
    (home-page "https://github.com/rcaloras/bash-preexec")
    (synopsis "preexec and precmd functions for Bash just like Zsh.")
    (description "preexec and precmd functions for Bash just like Zsh.")
    (license licenses:expat)))

(define-public xschem-newer
  (package
    (inherit xschem)
    (version "3.4.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/StefanSchippers/xschem")
             (commit version)))
       (file-name (git-file-name "xschem" version))
       (sha256
        (base32 "118xx2ggga5bhdrn20a2cxbfzh126wrasqd781c0sayv6b6hv0vm"))))
    (inputs (modify-inputs (package-inputs xschem)
              (append readline)
              (append libjpeg-turbo)))))

(define-public ngspice-git
  (package
    (inherit ngspice)
    (version "19.2-0.77131a2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/ngspice/ngspice")
             (commit "77131a2a33440b052a1de4b656c2234a91bd8e0c")))
       (file-name (git-file-name "ngspice" version))
       (sha256
        (base32 "169ljqq16llzkcdqi1alnh3zj2z74r6pmgwaccxrhk2rfa50paar"))))
    (inputs (modify-inputs (package-inputs ngspice)
              (append libx11 libxaw fftw)))
    (native-inputs (modify-inputs (package-native-inputs ngspice)
                     (append autoconf-2.71
                             automake
                             libtool
                             perl
                             bison
                             flex)))
    (arguments
     (append (list #:tests? #f) ;need X
             (substitute-keyword-arguments (package-arguments ngspice)
               ((#:phases phases)
                #~(modify-phases #$phases
                    (delete 'delete-scripts)
                    (add-after 'unpack 'reconf
                      (lambda _
                        (invoke "autoreconf" "-vfi"))))))))))

(define-public magic
  (package
    (name "magic")
    (version "8.3.520")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RTimothyEdwards/magic")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a4vmsmv3qmffz4h8xf8ww99b51mrqdjphba0hphqx9rblgdkfj6"))))
    (build-system gnu-build-system)
    (inputs (list tcsh
                  python
                  libx11
                  mesa
                  cairo
                  tcl
                  glu
                  tk
                  git))
    (home-page "http://opencircuitdesign.com/magic/")
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-tcl="
                             (assoc-ref %build-inputs "tcl"))
              (string-append "--with-tk="
                             (assoc-ref %build-inputs "tk")))
      #:tests? #f))
    (synopsis "Magic VLSI Layout Tool")
    (description "Magic VLSI Layout Tool")
    (license #f)))

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
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-Dtests=true")))
    (native-inputs (list pkg-config gobject-introspection vala))
    (inputs (list gtk))
    (home-page "https://github.com/wmww/gtk4-layer-shell")
    (synopsis
     "A library to create panels and other desktop components for Wayland using the Layer Shell protocol and GTK4")
    (description
     "A library to create panels and other desktop components for Wayland using the Layer Shell protocol and GTK4")
    (license #f)))

(define-public umr
  (package
    (name "umr")
    (version (string-append "0.1-0.39c91cd"))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/tomstdenis/umr")
             (commit "39c91cdee3804ea209dc21619087212bc75d267e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jwp45z24rj9lsq0wmmmxc50rmxp50qxz0gq11hwn6cg71kf9b48"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs (list ncurses
                  nanomsg
                  libglvnd
                  pkg-config
                  libpciaccess
                  libdrm
                  llvm-18
                  mesa
                  sdl2
                  bash-completion))
    (synopsis "User Mode Register Debugger for AMDGPU Hardware")
    (description "User Mode Register Debugger for AMDGPU Hardware")
    (home-page "https://gitlab.freedesktop.org/tomstdenis/umr")
    (license #f)))

(define-public glfw-3.4
  (package
    (inherit glfw)
    (version "3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/glfw/glfw"
                           "/releases/download/"
                           version
                           "/glfw-"
                           version
                           ".zip"))
       (sha256
        (base32 "1sd396kkn53myp61kxrd18h7b1q4ix173hhxhvl0iz8j4x5h1v5m"))))
    (native-inputs (modify-inputs (package-native-inputs glfw)
                     (prepend pkg-config)))
    ;; When building out of source, the install phase fails with:
    ;; file INSTALL cannot find "/tmp/guix-build-glfw-3.4.drv-0/build/docs/html":
    ;; No such file or directory
    (arguments
     (substitute-keyword-arguments (package-arguments glfw)
       ((#:out-of-source? _ #f)
        #f)
       ((#:configure-flags flags)
        #~(cons* "_DCMAKE_TESSESES=reiea"
                 #$flags))))))

(define-public folly-tunable
  (package
    (name "folly-tunable")
    (version "2025.02.17.00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/facebook/folly")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yxma75a4hdd32d2nxq56v62lh276yfpnnki1j7x55vckz2ipqrs"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Tests must be explicitly enabled
      ;; #:configure-flags #~(list "-DBUILD_TESTS=ON")
      ;; Leave tests disabled; see https://github.com/facebook/folly/issues/2246
      #:tests? #f
      #:configure-flags
      #~(list "-DFOLLY_SUPPORT_SHARED_LIBRARY=ON"
              "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
              "-DFOLLY_NO_EXCEPTION_TRACER=ON")))
    (propagated-inputs (list boost gflags glog liburing))
    (inputs (list bzip2
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
    (native-inputs (list googletest))
    (synopsis
     "Collection of C++ components complementing the standard library")
    (description
     "Folly (acronymed loosely after Facebook Open Source Library) is a library
of C++14 components that complements @code{std} and Boost.")
    (home-page "https://github.com/facebook/folly/wiki")
    ;; 32-bit is not supported: https://github.com/facebook/folly/issues/103
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (properties `((tunable? . #t)))
    (license licenses:asl2.0)))

libpressio
