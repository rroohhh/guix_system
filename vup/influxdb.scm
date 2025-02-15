(define-module (vup influxdb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module ((guix licenses) #:prefix license:))

;; https://dl.influxdata.com/influxdb/releases/influxdb2-2.7.4_linux_amd64.tar.gz
(define-public influxd
  (package
    (name "influxd")
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.influxdata.com/influxdb/releases/influxdb2-" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "12322iaxfkcmbng20cxyq17q1ysg4v5p4dmcjcy4iys25sjwqb1a"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("usr/bin/influxd" "bin/influxd"))))
    (home-page
      "https://www.influxdata.com/")
    (synopsis "InfluxDB is a time series platform")
    (description
      "InfluxDB is a time series platform")
    (license #f)))                      ; MIT

;; https://dl.influxdata.com/influxdb/releases/influxdb2-client-2.7.3-linux-amd64.tar.gz
(define-public influx
  (package
    (name "influx")
    (version "2.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.influxdata.com/influxdb/releases/influxdb2-client-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32 "1rmzixan29f5g26snmiixc6cqar5gpilbgw6g2ybcqvlah2g6rm2"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("influx" "bin/influx"))))
    (home-page
      "https://www.influxdata.com/")
    (synopsis "InfluxDB is a time series platform - cli")
    (description
      "InfluxDB is a time series platform - cli")
    (license #f)))                      ; MIT

(define-public telegraf
  (package
    (name "telegraf")
    (version "1.28.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.influxdata.com/telegraf/releases/telegraf-" (string-replace-substring version "-" "~") "_linux_amd64.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sgn3r8cpkl5sr98c4mlvxz4a4sq74h7rnz6yka9k4lrirg94bxf"))))
    (build-system copy-build-system)
    (inputs `(("libc" ,glibc)
              ("patchelf" ,patchelf)))
    (arguments
     `(#:install-plan
       '(("usr/bin/telegraf" "bin/telegraf"))))
    (home-page
      "https://www.influxdata.com/")
    (synopsis "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (description
      "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (license #f)))                      ; MIT
