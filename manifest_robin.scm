(use-modules (vup patches))

(use-package-modules video xorg chromium pulseaudio fonts messaging terminals rsync admin linux file docker flashing-tools freedesktop pv networking screen curl gnome image-viewers gl python python-xyz gdb graphviz engineering android pdf mpi wine mail tex fpga compression irc vulkan ncurses pkg-config autotools pcre libusb boost commencement cmake xml qt glib fontutils ninja dns python-science code cryptsetup gimp maths libreoffice aspell man patchutils telephony node parallel photo game-development valgrind wget python-web serialization xdisorg java elf tls sqlite golang python-compression perl gtk version-control gstreamer llvm imagemagick ghostscript games bittorrent embedded libevent rust-apps nss)

(use-modules (vup rust-nightly))
(use-modules (vup misc))
(use-modules (vup fpga))
(use-modules (vup python-xyz))
(use-modules (vup lld))
(use-modules (vup biber-old))
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
  lld-10
  wireshark
  (list isc-bind "utils")
  xdg-utils
  xorg-server
  qpdf
  openmpi
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
  gst-plugins-good
  gst-plugins-bad
  gst-plugins-base
  gst-plugins-ugly
  clang-10
  (list clang-10 "extra")
  imagemagick
  ghostscript
  smartmontools
  mbuffer
  xclip
  qbittorrent
  openocd
  python-antfs-cli
  ripgrep

  ; games
  crawl-tiles

  ; cad
  ;; freecad currently broken
  solvespace

  ; lang
  go

  ; java
  (list icedtea "jdk")

  ; mail
  notmuch
  notifymuch
  msmtp

  ; latex
  texlive
  biber-2.11
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

  ; python
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
  pypy3

  ; compression
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
  vulkan-loader
  ncurses
  gnu-make
  pkg-config
  automake
  autoconf
  pcre2
  libusb
  eudev
  boost
  fuse
  gcc-toolchain-10
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
  mesa
  glu
  freeglut
  libxcb
  xcb-util-keysyms
  xcb-util
  freetype
  libinput
  ninja
  alsa-lib
  libevent
  nspr
  java-swt
  libtool
  ))
