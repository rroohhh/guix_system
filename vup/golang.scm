(define-module (vup golang)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-1.22
  (package
    (inherit go-1.21)
    (name "go")
    (version "1.22.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/go")
                    (commit (string-append "go" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00j6sn2zysk5pdzxw1wfdi31wggzw1h1026ah3x3mi85dwsijhjs"))))
    (arguments
     (substitute-keyword-arguments
      (package-arguments go-1.21)
      ((#:phases phases)
       #~(modify-phases #$phases
                       (delete 'unpatch-perl-shebangs)))))
    (native-inputs
     ;; Go 1.20 and later requires Go 1.17 as the bootstrap toolchain.
     ;; See 'src/cmd/dist/notgo117.go' in the source code distribution,
     ;; as well as the upstream discussion of this topic:
     ;; https://go.dev/issue/44505
     (alist-replace "go" (list go-1.21) (package-native-inputs go-1.21)))))
