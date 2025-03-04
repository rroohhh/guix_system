(define-module (home hy3)

  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages wm)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public hyprland-0.46
    (package
     (inherit hyprland)
     (version "0.46.0")
     (source (origin
              (inherit (package-source hyprland))
              (method url-fetch)
              (uri (string-append "https://github.com/hyprwm/Hyprland"
                                  "/releases/download/v" version
                                  "/source-v" version ".tar.gz"))
              (sha256
               (base32
                "1rnim5bwbkgpf4pknx193m7a6zgxjrfa9v448yj6506hcfdzg0sk"))))))

(define-public hyprland-hy3
  (let*
      ((hyprland-inputs (package-inputs hyprland))
       (get-input (lambda (name) (cadr (assoc name hyprland-inputs)))))
    (package
     (name "hyprland-hy3")
     (version (package-version hyprland-0.46))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/outfoxxed/hy3")
             ;; (commit "4c79361db9c065886c163d1cf873889e1e641e44")))
             (commit (string-append "hl" (package-version hyprland-0.46)))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ry1ylkblmilvha7hiag875qhlcf4pw1av7a4khf1w9pichy9lvs"))))
     (build-system cmake-build-system)
     (native-inputs (list pkg-config gcc-14))
     (arguments '(#:tests? #f)) ; no tests
     (inputs (modify-inputs hyprland-inputs
                            (append hyprland-0.46)))
     (home-page "https://github.com/outfoxxed/hy3")
     (synopsis "Hyprland plugin for an i3 / sway like manual tiling layout")
     (description "Hyprland plugin for an i3 / sway like manual tiling layout")
     (license (list license:gpl3)))))
