(define-module (home hy3)
  #:use-module (rosenthal packages wm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public hyprland-hy3
  (let*
      ((hyprland-inputs (package-inputs hyprland))
       (get-input (lambda (name) (cadr (assoc name hyprland-inputs)))))
    (package
     (name "hyprland-hy3")
     (version (package-version hyprland))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/outfoxxed/hy3")
             (commit (string-append "hl" (package-version hyprland)))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0g2szy938w017nmlf8h0q5lfh9iz4qd38lsnxnnj9kh5x9vnb99y"))))
     (build-system cmake-build-system)
     (native-inputs (list pkg-config gcc-13))
     (arguments
      '(#:phases
         (modify-phases %standard-phases
                        (delete 'check))))  ; no tests
     (inputs (modify-inputs hyprland-inputs
                            (append hyprland)))
     (home-page "https://github.com/outfoxxed/hy3")
     (synopsis "Hyprland plugin for an i3 / sway like manual tiling layout")
     (description "Hyprland plugin for an i3 / sway like manual tiling layout")
     (license (list license:gpl3)))))

hyprland-hy3
