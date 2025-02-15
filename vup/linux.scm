;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (vup linux)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix download))

(define (linux-nonfree-urls version)
  "Return a list of URLs for Linux-Nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/"
         "v" (version-prefix version 1) ".x/"
         "linux-" version ".tar.xz")))

(define-public linux-nonfree
  (let* ((version "6.10.14"))
    (package
      (inherit linux-libre)
      (name "linux-nonfree")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (linux-nonfree-urls version))
                (sha256
                 (base32
                  "0gj2z9ax1qv59n2mld0pg2svwi28lbq92ql98vy7crynd2ybrram"))))
      (synopsis "Mainline Linux kernel, nonfree binary blobs included.")
      (description "Linux is a kernel.")
      (license license:gpl2)
      (home-page "https://kernel.org/"))))

(define-public linux-nonfree-stable
  (let* ((version "6.6.72"))
    (package
      (inherit linux-libre)
      (name "linux-nonfree-stable")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (linux-nonfree-urls version))
                (sha256
                 (base32
                  "0fggpba886340xi8gkxc6hmzplcm69nliddql3d6hn8djcafbfgy"))))
      (synopsis "Mainline Linux kernel, nonfree binary blobs included.")
      (description "Linux is a kernel.")
      (license license:gpl2)
      (home-page "https://kernel.org/"))))

(define linux-firmware-version "20250109")
(define (linux-firmware-source version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                              "/git/firmware/linux-firmware.git"))
          (commit version)))
    (file-name (git-file-name "linux-firmware" (string-take version 8)))
    (sha256
     (base32
      "1016jklicdcvh62p49c2m0a2cmwn2f30fhgrx6rhn9qplsg0fmvr"))))

(define-public linux-firmware-nonfree
  (package
    (name "linux-firmware-nonfree")
    (version linux-firmware-version)
    (source (linux-firmware-source version))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware/")))
                     (mkdir-p fw-dir)
                     (copy-recursively source fw-dir)
                     #t))))
    (home-page "")
    (synopsis "Non-free firmware for Linux")
    (description "Non-free firmware for Linux")
    ;; FIXME: What license?
    (license (license:non-copyleft "https://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.radeon_firmware;hb=HEAD"))))

(define-public perf-nonfree
  (package
    (inherit perf)
    (name "perf-nonfree")
    (version (package-version linux-nonfree))
    (source (package-source linux-nonfree))
    (license (package-license linux-nonfree))))

(define-public cpupower-nonfree
  (package
    (inherit cpupower)
    (name "cpupower-nonfree")
    (version (package-version linux-nonfree))
    (source (package-source linux-nonfree))
    (license (package-license linux-nonfree))))
