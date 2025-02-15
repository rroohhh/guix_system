(define-module (vup mesa)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg))

(define-public libdrm-2.4.110
  (package
   (inherit libdrm)
   (version "2.4.110")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://dri.freedesktop.org/libdrm/libdrm-"
                   version ".tar.xz"))
             (sha256
              (base32
               "0dwpry9m5l27dlhq48j4bsiqwm0247cxdqwv3b7ddmkynk2f9kpf"))))))

(define-public mesa-updated-libdrm
  (package
   (inherit mesa)
   (name "mesa-new-libdrm")
   (propagated-inputs (modify-inputs (package-propagated-inputs mesa) (replace "libdrm" libdrm-2.4.110)))))
