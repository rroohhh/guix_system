(define-module (vup tmp)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages telegram)
  #:use-module (gnu packages glib)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public log4cxx
  (package
   (name "log4cxx")
   (version "0.12.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/apache/logging-log4cxx/archive/refs/tags/rel/v" version ".tar.gz"))
     (sha256
      (base32 "1vdyr1qbjlg8hpib63kmqx82jy4lhbcrirq1qhbah1dhql044yjn"))))
     ;; (patches (list
     ;;           (origin
     ;;             (method url-fetch)
     ;;             (uri "https://raw.githubusercontent.com/NixOS/nixpkgs/326f54d/pkgs/development/libraries/log4cxx/narrowing-fixes.patch")
     ;;             (sha256 "1kbc8z9349smlk4h8gdgdq5s84da9ln3bafl7kwjibrvx47gykq8"))))

   (build-system cmake-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        ;; (replace 'bootstrap
        ;;   (lambda _ (invoke "autoreconf" "-vfi")))
        (delete 'check))))
        ;; (add-after 'unpack 'patch
        ;;  (lambda* _
        ;;   (substitute* '("src/examples/cpp/console.cpp" "src/main/cpp/inputstreamreader.cpp" "src/main/cpp/socketoutputstream.cpp")
        ;;     (("^#include (.*)\n$" _ things)
        ;;      (string-append "#include " things "\n #include <cstring>\n")))))

   (inputs (list apr apr-util zip))
   (home-page "https://logging.apache.org/log4cxx/latest_stable/")
   (synopsis "Apache Log4cxx is a logging framework for C++ patterned after Apache log4j")
   (description "Apache Log4cxx is a logging framework for C++ patterned after Apache log4j")
   (license license:asl2.0)))


;; (define-public hime-fixed
;;   (package
;;    (inherit hime)
;;    (inputs
;;     (modify-inputs
;;      (package-inputs hime)
;;      (append qtbase-5)))
;;    (arguments
;;     (substitute-keyword-arguments (package-arguments hime)
;;       ((#:phases phases)
;;        `(modify-phases ,phases
;;           (replace 'qt-wrap
;;             (lambda* (#:key outputs inputs #:allow-other-keys)
;;               ((assoc-ref qt:%standard-phases 'qt-wrap) #:inputs inputs #:outputs outputs #:qtbase (assoc-ref inputs "qtbase"))
;;               #t))))))))

;; (define-public nimf-fixed
;;   (package
;;    (inherit nimf)
;;    (inputs
;;     (modify-inputs
;;      (package-inputs nimf)
;;      (append qtbase-5)))
;;    (arguments
;;     (substitute-keyword-arguments (package-arguments nimf)
;;       ((#:phases phases)
;;        `(modify-phases ,phases
;;           (replace 'qt-wrap
;;             (lambda* (#:key outputs inputs #:allow-other-keys)
;;               ((assoc-ref qt:%standard-phases 'qt-wrap) #:inputs inputs #:outputs outputs #:qtbase (assoc-ref inputs "qtbase"))
;;               #t))))))))
(define-public glibmm-2.66.6
   (package
    (inherit glibmm)
    (name "glibmm")
    (version "2.66.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/glibmm/"
                       (version-major+minor version)
                       "/glibmm-" version ".tar.xz"))
       (sha256
        (base32 "0bqm9vqwhas69q6n89wd2xgxvrlkpxra13dzsx8m67hqk0jp8n2k"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs glibmm)
       (replace "libsigc++" libsigc++-2)))))


(define-public telegram-desktop-fixed
  (package
   (inherit telegram-desktop)
   (inputs
    (modify-inputs
     (package-inputs telegram-desktop)
     (replace "glibmm" glibmm-2.66.6)))))

telegram-desktop-fixed
