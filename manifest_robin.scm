(use-modules (vup patches))

(use-package-modules video xorg chromium pulseaudio fonts messaging terminals rsync admin linux file docker flashing-tools freedesktop pv networking screen curl gnome image-viewers gl python python-xyz gdb graphviz engineering android pdf mpi wine mail tex fpga compression irc vulkan ncurses pkg-config autotools pcre libusb boost commencement cmake xml qt glib fontutils ninja dns)

(use-modules (vup rust-nightly))
(use-modules (vup fpga))
(use-modules (vup python-xyz))
(use-modules (vup lld))
(use-modules (vup biber-old))
(use-modules (vup pidgin-libnotify))

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
  fastboot
  adb
  rust-nightly
  (list rust-nightly "cargo")
  lld-8
  wireshark
  (list isc-bind "utils")
  xdg-utils
  xorg-server
  qpdf
  openmpi
  wine64-staging

  ; mail
  notmuch
  notifymuch
  msmtp

  ; latex
  texlive
  biber-2.11
  python-pygments ; minted
  texlive-luatex-luaotfload

  ; fpga
  gtkwave
  python-bitarray
  python-pyvcd
  python-jinja2
  yosys-git

  ; python
  python-pandas
  python-tqdm
  python-scipy
  python-matplotlib
  python-quadpy
  python-fitsio


  ; compression
  lz4
  unzip
  bzip2


  ; IM
  pidgin
  pidgin-libnotify
  telegram-purple
  quassel

  ; desktop shit
  adwaita-icon-theme

  ; desktop management
  setxkbmap
  pavucontrol
  xrandr

  ; multimedia
  ungoogled-chromium
  mpv

  ; fonts
  font-hack
  font-awesome
  font-google-noto

  ; dev dependencies
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
  gcc-toolchain-9
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
  ))
