(define-module (home packages)
  #:use-module (vup patches)
  #:use-module (vup firefox)
  #:use-module (gnu)
  #:use-module ((gnu packages python-xyz) #:prefix guix:)
  #:use-module ((gnu packages fpga) #:select (gtkwave))

  #:use-module (vup rust-nightly)
  #:use-module (vup fastlane)
  #:use-module (vup qt-apps)
  #:use-module (vup freecad)
  #:use-module (vup emacs)
  #:use-module (vup root)
  #:use-module (vup mesa)
  #:use-module (vup misc)
  #:use-module (vup fpga)
  #:use-module (vup python-xyz)
  #:use-module (vup horizon)
  #:use-module (vup unrar)
  #:use-module (vup docker)
  #:use-module (vup x)
  #:use-module (vup solvespace)
  #:use-module (vup tmp)
  #:use-module (vup concourse)
  #:use-module (vup rust-apps)

  #:use-module (games packages factorio))

(use-package-modules wm pciutils video xorg chromium pulseaudio fonts messaging terminals rsync admin linux file flashing-tools freedesktop pv networking screen curl gnome image-viewers gl python python-xyz gdb graphviz engineering android pdf mpi wine mail tex compression irc vulkan ncurses pkg-config autotools pcre libusb boost commencement cmake xml qt glib fontutils ninja dns python-science code cryptsetup gimp maths libreoffice aspell man patchutils telephony node parallel photo game-development valgrind wget python-web serialization xdisorg java elf tls sqlite golang python-compression perl gtk version-control gstreamer llvm imagemagick ghostscript games bittorrent embedded libevent rust-apps nss aidc arcan inkscape prolog audio music crypto textutils electronics protobuf python-check check algebra astronomy sdl image-processing web lsof documentation vim python-build emacs bootloaders cpio gnupg vnc vpn python-crypto sphinx build-tools image cups license bison flex)

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
   emacs-pgtk-native-comp
   zip
   lz4
   unzip
   bzip2
   unrar
   p7zip
   xz))


(define-public desktop-packages
  (list
  go-github-com-junegunn-fzf
   xdot
   (list isc-bind "utils")
   xdg-utils

   qpdf
   poppler
   ghostscript

   gnuplot

   hunspell
   hunspell-dict-en-us
   
   python-antfs-cli

   masscan
   aircrack-ng

   patchelf

   btrfs-progs

   fly

   evince
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
   ungoogled-chromium/wayland
   ;; firefox/wayland

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
   ;; wine64-staging

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
   xdg-desktop-portal-wlr
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
   solvespace

   ;; java
   (list icedtea "jdk")

   ;; mail
   notmuch
   notifymuch
   msmtp
   ;; astroid

   ;; latex
   texlive
   biber
   python-pygments))                      ; minted
   ;; texlive-luatex-luaotfload ; broken




(define-public dev-packages
  (list
   gdb
   valgrind
   perf

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

   d-feet

   spirv-tools
   vulkan-tools
   vulkan-loader
   vulkan-headers
   shaderc-2022
   glslang
   mesa-utils

   ;; fpga
   gtkwave
   python-amaranth
   python-amaranth-boards
   python-amaranth-stdio
   python-amaranth-soc
   yosys-git
   nextpnr
   trellis
   icestorm
   symbiyosys


   rust-ra-multiplex-0.1


   ;; lang
   go
   swi-prolog
   rust-nightly
   ;; (list rust-nightly "doc")
   clang-13
   ;; (list clang-12 "extra") gone?
   lld
   gcc-toolchain ; keep this in sync withThe system toolchain, otherwise linking shit sometime breaks (fuck c++) 
   ;; binutils-gold
   gfortran-toolchain
   perl
   node-lts))

(define-public laptop-packages
  (list
   powertop
   guvcview
   powerstat))

(define-public android-packages
  (list
   adb
   fastboot))

(define-public python-packages
  (list
    ;; python
   python-distro
   python-wrapper
   python-pyquery
   python-flameprof
   python-pyyaml
   python-sortedcontainers
   python-toposort
   python-jedi
   python-lz4
   ;; python-astropy
   python-epc
    ;; python-chipsec ; broken on linux >= 5.7.0
   ;; python-varname
   python-pandas-fixed
   python-tqdm
   python-scipy
   ;; python-demjson
   python-matplotlib
   python-quadpy
   python-fitsio
   python-colorhash
   ;; python-pint
   python-requests
   python-ruamel.yaml
   python-toml
   python-xmltodict
   ;; python-pikepdf
   python-bluepy
   python-pypng
   python-flask
   ;; python-solaredge-modbus
   python-libusb1
   ;; python-joblib
   ;; python-intelhex
   python-networkx
   python-shapely-fixed
   python-pyflakes
   python-influxdb
   python-bcrypt
   python-sphinx-rtd-theme
    ;; python-descartes
   guix:jupyter
    ;; vup:python-nbconvert-new
   python-huffman
   python-dill
   python-multiprocess
   python-loky
   ;; python-billiard
   ;; python-line-profiler - broken
   python-tables
   python-pmbootstrap
   ;; pypy3 - broken
   python-dbus
   python-mypy
   python-pytest
   python-pytest-forked
   python-pytest-xdist
   python-bitstring
   python-bitarray
   python-imageio
   python-numba
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
   fuse
   cmake
   expat
   qtwayland
   qtsvg
   qtx11extras
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
   protobuf
   openmpi
   nss
   libxcomposite
   cups
   pipewire-0.3
   bison
   flex
   ))


(define-public full-packages
  (append base-packages desktop-packages laptop-packages dev-dependencies python-packages dev-packages android-packages))
