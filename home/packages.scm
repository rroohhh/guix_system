(define-module (home packages)
  #:use-module (config sysprof)
  #:use-module (vup patches)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (nongnu packages mozilla)
  #:use-module ((gnu packages python-xyz) #:prefix guix:)
  #:use-module ((gnu packages fpga) #:select (gtkwave))

  #:use-module (vup rust-nightly)
  #:use-module (vup tmp)
  #:use-module (vup python-xyz)
  #:use-module (vup qt-apps)
  #:use-module (vup root)
  #:use-module (vup mesa)
  #:use-module (vup linux)
  #:use-module (vup misc)
  #:use-module (vup fpga)
  #:use-module (vup go-xyz)
  #:use-module ((vup python-xyz) #:prefix vup:)
  #:use-module (vup horizon)
  #:use-module (vup unrar)
  #:use-module (vup sioyek)
  #:use-module (vup atuin)
  ;; #:use-module (vup docker)
  #:use-module (vup x)
  #:use-module ((vup solvespace) #:prefix vup:)
  ;; #:use-module (vup tmp)
  #:use-module (vup concourse)
  #:use-module (vup rust-apps)
  #:use-module (rosenthal packages wm)
  #:use-module (guix-science packages python)

  #:use-module (games packages factorio))

(use-package-modules task-management bash docker wm pciutils video xorg pulseaudio fonts messaging terminals rsync admin linux file flashing-tools freedesktop pv networking screen curl gnome image-viewers gl python python-xyz gdb graphviz engineering android pdf mpi wine mail tex compression irc vulkan ncurses pkg-config autotools pcre libusb boost commencement cmake xml qt glib fontutils ninja dns python-science code cryptsetup gimp maths libreoffice aspell man patchutils telephony node parallel photo game-development valgrind wget python-web serialization xdisorg java elf tls sqlite golang python-compression perl gtk version-control gstreamer llvm imagemagick ghostscript games bittorrent embedded libevent rust-apps nss aidc arcan inkscape prolog audio music crypto textutils electronics protobuf python-check check algebra astronomy sdl image-processing web lsof documentation vim python-build emacs bootloaders cpio gnupg vnc vpn python-crypto sphinx build-tools image cups license bison flex graph gcc xiph hunspell telegram texlive rust)


(define firefox-fixed
  (package
    (inherit firefox)
    (native-inputs '())
    (inputs
     `(("bash" ,bash-minimal)
       ("firefox" ,firefox)
       ("pipewire" ,pipewire)
       ("pciutils" ,pciutils)))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((bash    (assoc-ref %build-inputs "bash"))
                (firefox (assoc-ref %build-inputs "firefox"))
                (pipewire (assoc-ref %build-inputs "pipewire"))
                (pciutils (assoc-ref %build-inputs "pciutils"))
                (out     (assoc-ref %outputs "out"))
                (exe     (string-append out "/bin/firefox")))
           (mkdir-p (dirname exe))

           (call-with-output-file exe
             (lambda (port)
               (format port "#!~a \n
export LD_LIBRARY_PATH=~a:~a:$LD_LIBRARY_PATH \n
exec ~a $@\n"
                       (string-append bash "/bin/bash")
                       (string-append pipewire "/lib")
                       (string-append pciutils "/lib")
                       (string-append firefox "/bin/firefox"))))
           (chmod exe #o555)

           ;; Provide the manual and .desktop file.
           (copy-recursively (string-append firefox "/share")
                             (string-append out "/share"))
           (substitute* (string-append
                         out "/share/applications/firefox.desktop")
             ((firefox) out))
           #t))))))


(define-public base-packages
  (list
   rsync
   file
   elogind
   python
   curl
   git
   wget
   lsof
   dmidecode
   cryptsetup
   man-pages
   pv
   iperf
   parallel
   ripgrep
   mtr
   net-tools
   netcat
   socat
   tcpdump
   sshfs
   efibootmgr
   iptables
   openssl
   sqlite
   jq
   xxd
   mbuffer
   ; emacs-pgtk-native-comp
   emacs
   zip
   lz4
   unzip
   bzip2
   unrar
   p7zip
   xz
   zstd))


(define-public desktop-packages
  (list
   go-github-com-junegunn-fzf
   xdot
   (list isc-bind "utils")
   xdg-utils

   atuin

   qpdf
   poppler
   ghostscript

   gnuplot

   hunspell
   hunspell-dict-en-us
   
   vup:python-antfs-cli

   masscan
   aircrack-ng

   patchelf

   btrfs-progs

   fly

   sioyek
   feh
   scrot
   grim

   zoxide

   wl-clipboard

   tigervnc-client
   ;; tigervnc-server

   openconnect

   smartmontools
   lm-sensors

   baobab
   qdirstat

   ;; docker
   docker-cli
   ;; docker-compose

   ;; browser
   ;; ungoogled-chromium/wayland
   firefox-fixed

   ;; IM
   qtox
   quassel
   telegram-desktop
   mumble

   ;; desktop shit
   adwaita-icon-theme

   ;; desktop management
   pavucontrol
   wlr-randr

   ;; multimedia
   mpv
   youtube-dl
   amule
   ;; qbittorrent

   ;; fonts
   font-hack
   font-awesome
   font-google-noto

   modem-manager

   reuse

   zbar
   qrencode

   ;; wine
   ;; wine-staging
   wine64

   ;; video
   gimp
   darktable
   ;; rawtherapee
   imagemagick
   gstreamer
   ;; gstreamer-vaapi
   gst-plugins-good
   gst-plugins-bad
   gst-plugins-base
   gst-plugins-ugly
   ffmpeg
   inkscape
   xournalpp
   obs
   obs-wlrobs


   ;; xdk desktop
   xdg-desktop-portal
   ;; xdg-desktop-portal-wlr
   xdg-desktop-portal-hyprland
                                        ; needed to choose the display with the above
   wofi

   ;; encryption
   gnupg
   pinentry

   ;; audio / music
   alsa-utils
   gxtuner
   rakarrack
   carla
   rnnoise
   rev-plugins
   noisetorch

   ;; wacom
   xf86-input-wacom

   ;; vaapi
   libva
   libva-utils

   ;; games
   crawl-tiles
   factorio

   ;; cad
   ;; freecad-fixed temporary
   vup:solvespace

   ;; java
                                        ;(list openjdk17 "jdk")
   (list openjdk "jdk")

   ;; mail
   notmuch
   notifymuch
   msmtp
   ;; astroid

   ;; bitwarden
   ;; rbw

   ;; sway
   ;; sway
   ;; hyprland
   hyprland

   swaynotificationcenter

   ;; timekeeping
   timewarrior

   ;; latex
   texlive
   biber
   python-pygments))                      ; minted
   ;; texlive-luatex-luaotfload ; broken




(define-public dev-packages
  (list
   gdb
   (list gdb "debug")
   valgrind
   perf-nonfree
   sysprof-new
   cpupower-nonfree

   dtc
   dfu-util
   cpio
   screen


   ltrace
   strace

   kicad 
   ; kicad-templates
   ; kicad-footprints
   ; kicad-packages3d
   ; kicad-symbols
   horizon 

   meld
   cloc

   ;; pulseview-libsigrok-master
   openocd

   wireshark

   python-binwalk

   cutter
   radare2

   gnu-make
   meson
   ninja

   glade3

   d-spy

   spirv-tools
   vulkan-tools
   vulkan-loader
   vulkan-headers
   ;; shaderc-upstream
   glslang
   mesa-utils

   ;; user mode amdgpu debugger
   umr

   ;; fpga
   gtkwave
   ;; vup:python-amaranth
   ;; vup:python-amaranth-boards
   ;; vup:python-amaranth-stdio
   ;; vup:python-amaranth-soc
   yosys-git
   ;; nextpnr ; tmp
   trellis
   icestorm
   vup:symbiyosys


   ra-multiplex


   ;; lang
   go-1.21
   swi-prolog
   ;; rust
   ;; (list rust "cargo")
   rust-nightly
   (list rust-nightly "cargo")
   (list rust-nightly "tools")
   (list rust-nightly "rust-src")
   (list rust-nightly "doc")
   clang-17
   clang-toolchain-17
   clang-runtime-17
   ;; (list clang-14 "debug")
   libomp
   ;; (list clang-12 "extra") gone?
   lld
   gcc
   (list gcc "lib")
   gcc-toolchain ; keep this in sync withThe system toolchain, otherwise linking shit sometime breaks (fuck c++) 
   (list gcc-toolchain "debug") ; keep this in sync withThe system toolchain, otherwise linking shit sometime breaks (fuck c++)
   ;; binutils-gold
   gfortran-toolchain
   (list gfortran-toolchain "debug")
   perl
   node-lts))

(define-public laptop-packages
  (list
   powertop
   guvcview
   powerstat))

(define-public android-packages
  (list
   adb))

(define-public python-packages
  (list
   ;; python
   python-distro
   python-librosa
   python-wrapper
   python-pyquery
   vup:python-flameprof
   python-pyyaml
   python-sortedcontainers
   python-toposort
   python-jedi
   python-lz4
   ;; python-astropy ; broken
   python-epc
   ;; python-chipsec ; broken on linux >= 5.7.0
   python-pandas
   python-tqdm
   python-scipy
   vup:python-demjson
   python-matplotlib
   vup:python-quadpy
   python-fitsio
   vup:python-colorhash
   vup:python-pint
   python-requests
   python-ruamel.yaml
   python-toml
   python-xmltodict
   ;; python-pikepdf broken
   vup:python-bluepy
   python-pypng
   python-flask
   ;; python-solaredge-modbus
   python-libusb1
   ;; python-joblib
   ;; python-intelhex
   python-networkx
   python-shapely
   python-pyflakes
   ;; python-influxdb
   python-bcrypt
   python-sphinx-rtd-theme
   vup:python-descartes
   guix:jupyter
   ;; vup:python-nbconvert-new
   vup:python-huffman
   python-dill
   python-multiprocess
   vup:python-loky
   ;; python-billiard
   vup:python-line-profiler                 ; - broken
   python-tables
   vup:pmbootstrap
   ;; pypy3 - broken
   python-dbus
   ;; python-mypy
   python-pytest
   python-pytest-forked
   python-pytest-xdist
   python-bitstring
   python-bitarray
   python-imageio
   python-numba
   python-zstandard
   python-lmfit
   python-jax
   python-jaxopt
   ;; python-plotly
   python-zarr))


(define-public dev-dependencies
  (list
   podofo
   ;; opencascade-occt
   libgit2
   zeromq
   glm
   cppzmq
   ;; librsvg
   ;; gtkmm
   libzip
   cairomm
   glib
   (list glib "bin")
   ncurses
   pkg-config
   automake
   autoconf
   pcre2
   libusb
   eudev
   boost
   ;; fuse
   cmake
   expat
   cereal
   pybind11

   qtwayland-5
   (list qtwayland-5 "debug")
   qtsvg-5
   (list qtsvg-5 "debug")
   qtx11extras
   (list qtx11extras "debug")
   qtbase-5
   (list qtbase-5 "debug")

   dbus
   libx11
   libxcursor
   libxrandr
   libxi
   glu
   freeglut
   libxcb
   xcb-util-keysyms
   xcb-util-image
   libxtst
   xcb-util
   freetype
   libinput
   libinih
   alsa-lib
   libevent
   nspr
   java-swt
   libtool
   libsodium
   libimobiledevice
   libxmu
   libxt
   libxpm
   libxml2
   libxkbfile
   libsecret
   asciidoc
   fftw
   gsl
   cfitsio
   sdl2
   pciutils
   glfw
   glu
   glew
   openblas
   lapack
   sfml
   ; protobuf
   openmpi
   nss
   libxcomposite
   cups
   pipewire
   bison
   flex))



(define-public full-packages
  (append base-packages desktop-packages laptop-packages dev-dependencies python-packages dev-packages android-packages))
