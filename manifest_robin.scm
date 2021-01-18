(use-modules (vup patches))

(use-package-modules video xorg chromium pulseaudio fonts messaging terminals rsync admin linux file docker flashing-tools freedesktop pv networking screen curl gnome image-viewers gl python python-xyz gdb graphviz engineering android pdf mpi wine mail tex fpga compression irc vulkan ncurses pkg-config autotools pcre libusb boost commencement cmake xml qt glib fontutils ninja dns python-science code cryptsetup gimp maths libreoffice aspell man patchutils telephony node parallel photo game-development valgrind wget python-web serialization xdisorg java elf tls sqlite golang python-compression perl gtk version-control gstreamer llvm imagemagick ghostscript games bittorrent embedded libevent rust-apps nss aidc arcan inkscape prolog audio music crypto textutils electronics protobuf python-check check algebra astronomy sdl image-processing web lsof documentation)

(use-modules (vup rust-nightly))
(use-modules (vup fastlane))
(use-modules (vup qt-apps))
(use-modules (vup freecad))
(use-modules (vup emacs))
(use-modules (vup root))
(use-modules (vup mesa))
(use-modules (vup misc))
(use-modules (vup fpga))
(use-modules (vup python-xyz))
(use-modules (vup horizon))
(use-modules (vup unrar))
(use-modules (vup x))
(use-modules (vup solvespace))



(packages->manifest
 (list
  ; tools
  go-github-com-junegunn-fzf
  rsync
  dmidecode
  ltrace
  file
  docker-cli
  dfu-util
  elogind
  pv
  iperf
  screen
  strace
  curl
  evince
  feh
  mesa-utils
  perf
  python
  gdb
  xdot
  lm-sensors
  kicad
  kicad-templates
  kicad-footprints
  kicad-packages3d
  kicad-symbols
  fastboot
  adb
  rust-nightly
  (list rust-nightly "doc")
  lld
  wireshark
  (list isc-bind "utils")
  xdg-utils
  xorg-server
  qpdf
  openmpi
  wine-staging
  wine64-staging
  horizon
  baobab
  btrfs-progs
  cloc
  cryptsetup
  cutter
  gimp
  gnuplot
  hunspell
  hunspell-dict-en-us
  igt-gpu-tools
  man-pages
  meld
  mumble
  netcat
  node
  parallel
  radare2
  rawtherapee
  sfml
  socat
  unrar
  valgrind
  wget
  gnu-make
  scrot
  youtube-dl
  ffmpeg
  iptables
  patchelf
  openssl
  sqlite
  x2x
  perl
  p7zip
  glade3
  gstreamer
  gstreamer-vaapi
  gst-plugins-good
  gst-plugins-bad
  gst-plugins-base
  gst-plugins-ugly
  clang-11
  (list clang-11 "extra")
  imagemagick
  ghostscript
  smartmontools
  mbuffer
  xclip
  qbittorrent
  openocd
  python-antfs-cli
  ripgrep
  efibootmgr
  qrencode
  usbmuxd
  git
  emacs-pgtk-native-comp-no-xwidgets
  ;; arcan
  inkscape
  poppler
  ruby-fastlane
  qdirstat
  xinput
  dos2unix
  openblas
  lapack
  xhost
  xauth
  sshfs
  tcpdump
  root
  sigrok-cli
  pulseview
  obs
  paprefs
  pasystray
  d-feet
  aircrack-ng
  mtr
  xournalpp
  net-tools
  darktable
  protobuf
  spirv-tools
  vulkan-tools
  vulkan-loader
  vulkan-headers
  ungoogled-chromium
  powerstat
  mesa-fixed
  graphene
  pulseaudio
  ;; opencv temporary
  glslang
  astroid
  jq
  network-manager-applet
  lsof

  ; wacom
  xf86-input-wacom

  ; vaapi
  libva
  libva-utils
  intel-vaapi-driver

  ; games
  crawl-tiles

  ; cad
  ;; freecad-fixed temporary
  solvespace

  ; lang
  go
  swi-prolog

  ; java
  (list icedtea "jdk")

  ; mail
  notmuch
  notifymuch
  msmtp

  ; latex
  texlive
  biber
  python-pygments ; minted
  ;; texlive-luatex-luaotfload ; broken

  ; fpga
  gtkwave
  python-nmigen
  python-nmigen-boards
  python-nmigen-stdio
  python-nmigen-soc
  yosys-git
  nextpnr
  icestorm
  symbiyosys

  ; python
  python-wrapper
  python-flameprof
  python-pyyaml
  python-sortedcontainers
  python-toposort
  python-jedi
  python-lz4
  python-astropy
  python-epc
  ;; python-chipsec ; broken on linux >= 5.7.0
  python-varname
  python-pandas
  python-tqdm
  python-scipy
  python-matplotlib
  python-quadpy
  python-fitsio
  python-colorhash
  python-pint
  python-requests
  python-ruamel.yaml
  python-toml
  python-xmltodict
  python-pikepdf
  python-bluepy
  python-pypng
  python-flask
  python-solaredge-modbus
  python-libusb1
  python-joblib
  python-intelhex
  python-networkx
  python-shapely-fixed
  ;; python-descartes
  jupyter
  python-dill
  python-multiprocess
  python-loky
  python-billiard
  python-line-profiler
  python-tables
  python-pmbootstrap
  pypy3
  python-dbus
  python-mypy
  python-pytest
  python-bitstring
  python-imageio
  python-numba

  ; music
  jack-1
  alsa-utils
  gxtuner
  rakarrack
  qjackctl
  ;; powertabeditor

  ; compression
  zip
  lz4
  unzip
  bzip2


  ; IM
  qtox
  quassel

  ; desktop shit
  adwaita-icon-theme

  ; desktop management
  setxkbmap
  pavucontrol
  xrandr

  ; multimedia
  mpv

  ; fonts
  font-hack
  font-awesome
  font-google-noto

  ; dev dependencies
  podofo
  opencascade-occt
  libgit2
  zeromq
  glm
  cppzmq
  librsvg
  gtkmm
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
  gcc-toolchain-10
  gfortran-toolchain
  cmake
  expat
  qtbase
  qtsvg
  qtx11extras
  dbus
  libx11
  libxcursor
  libxrandr
  libxi
  mesa-fixed
  glu
  freeglut
  libxcb
  xcb-util-keysyms
  xcb-util
  freetype
  libinput
  ;; ninja
  alsa-lib
  libevent
  nspr
  java-swt
  libtool
  libsodium
  libimobiledevice
  libxpm
  libxml2
  libxkbfile
  libsecret
  asciidoc
  fftw
  gsl
  cfitsio
  sdl2
  glfw
  ))
