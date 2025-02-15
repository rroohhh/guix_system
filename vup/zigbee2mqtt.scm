(define-module (vup zigbee2mqtt)
  #:use-module (vup prefetch-npm-deps)
  #:use-module (gnu packages node)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix build-system trivial))

(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define* (npm-deps-fetch ref hash-algo hash
                         #:optional name
                         #:key (system (%current-system))
                         (guile (default-guile)))

  (define modules
    (delete '(guix config)
            (source-module-closure '((guix build git)
                                     (guix build utils)))))


  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define guile-lzlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-lzlib))

  (define gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'guile-gnutls))

  (define nss-certs
    (module-ref (resolve-interface '(gnu packages certs)) 'nss-certs))

  (define glibc-locales
    ;; Note: pick the '-final' variant to avoid circular dependency on
    ;; i586-gnu, where 'glibc-utf8-locales' indirectly depends on Git.
    (module-ref (resolve-interface '(gnu packages commencement))
                'glibc-utf8-locales-final))

  (define build
    (with-imported-modules modules
      (with-extensions (list guile-json gnutls ;for (guix swh)
                             guile-lzlib)
        #~(begin
            (use-modules (guix build utils))
            (setenv "GUIX_LOCPATH"
                    #+(file-append glibc-locales "/lib/locale"))
            (setlocale LC_ALL "en_US.utf8")
            (setenv "SSL_CERT_DIR" #$(file-append nss-certs "/etc/ssl/certs"))
            (invoke #$prefetch-npm-deps-bin #$(file-append ref "/package-lock.json") #$output)))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "npm-deps") build
                      #:script-name "npm-deps-download"
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:local-build? #t ;don't offload repo cloning
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define-public zigbee2mqtt
  (package
    (name "zigbee2mqtt")
    (version "1.37.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/koenkk/zigbee2mqtt")
             (commit version)))
       (sha256
        (base32 "0vjpfl91sa92802vvzhbzpb7dz8x2rfqwg5xq6min7nyxgf734df"))))
    (build-system trivial-build-system)
    (synopsis "zigbee2mqtt")
    (description "zigbee2mqtt")
    (inputs (list node-lts libuv))
    (arguments
     (list
      #:modules '((guix build utils) (guix search-paths) (guix records))
      #:builder
      (with-extensions (list (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))
        #~(begin
            (use-modules (guix build utils)
                         (guix search-paths)
                         (ice-9 match)
                         (ice-9 popen)
                         (ice-9 regex)
                         (srfi srfi-43)
                         (json))
            (let* ((source (assoc-ref %build-inputs "source"))
                   (deps-dir
                    #$(origin
                        (method npm-deps-fetch)
                        (uri source)
                        (sha256
                         (base32 "02avp5z9nnlh4dd1dfdx5283qbj4hsbvi3x1p8mdbhy49x2rwc02"))))
                   (npm-flags (list "--omit=optional" "--loglevel=verbose"))
                   (sh #$(file-append bash "/bin/bash"))
                   (npm #$(file-append node-lts "/bin/npm"))
                   (node #$(file-append node-lts "/bin/node"))
                   (tmpdir (string-append (getenv "TMPDIR")))
                   (cache-map-path (string-append tmpdir "/MEOW"))
                   (prefetch-npm-deps-bin #$(file-append prefetch-npm-deps "/bin/prefetch-npm-deps"))
                   (src-dir (string-append tmpdir "/src"))
                   (home-dir (string-append tmpdir "/home"))
                   (input-directories (append (map (match-lambda
                                                     ((_ . input)
                                                      input))
                                                   %build-inputs)
                                              '#+(list
                                                  bash coreutils gcc-toolchain gnu-make python
                                                  linux-libre-headers sed grep findutils))))

              (for-each (match-lambda
                          (($ <search-path-specification> variable files separator type pattern)
                           (set-path-environment-variable variable files
                                                          input-directories
                                                          #:separator separator
                                                          #:type type
                                                          #:pattern pattern)))
                        (list $CPLUS_INCLUDE_PATH $C_INCLUDE_PATH $LIBRARY_PATH $PATH $PKG_CONFIG_PATH))

              (mkdir src-dir)
              (mkdir home-dir)
              (setenv "HOME" home-dir)
              (copy-recursively source src-dir)

              (for-each patch-shebang
                        (find-files src-dir))

              (setenv "npmDeps" deps-dir)
              (setenv "CACHE_MAP_PATH" cache-map-path)
              (invoke prefetch-npm-deps-bin "--map-cache")
              (invoke prefetch-npm-deps-bin "--fixup-lockfile" (string-append src-dir "/package-lock.json"))
              (invoke npm "config" "set" "cache" deps-dir)
              (invoke npm "config" "set" "offline" "true")
              (invoke npm "config" "set" "progress" "false")
              (display "installing dependencies")

              (chdir src-dir)
              (apply invoke `(,npm "ci" "--ignore-scripts"  ,@npm-flags))

              (for-each patch-shebang
                        (find-files "node_modules"))

              (apply invoke `(,npm "rebuild" ,@npm-flags))

              (for-each patch-shebang
                        (find-files "node_modules"))

              (delete-file cache-map-path)
              (setenv "CACHE_MAP_PATH" "")

              ;; building
              (apply invoke `(,npm "run" "build" ,@npm-flags))

              (setenv "npm_config_cache" (string-append home-dir "/.npm"))

              ;; installing
              (let*
                  ((package-json (call-with-input-file "package.json" json->scm))
                   (package-name (assoc-ref package-json "name"))
                   (package-out (string-append #$output "/lib/node_modules/" package-name))
                   (node-modules-dir (string-append package-out "/node_modules"))
                   (bin-dir (string-append #$output "/bin"))
                   (package-bin (assoc-ref package-json "bin")))
                (vector-for-each (lambda (_ file)
                                   (let* ((path (assoc-ref file "path"))
                                          (install-path (string-append package-out "/" path)))
                                     (if (not (string-prefix? "node_modules" path))
                                         (begin
                                           (display (string-append "installing " path " to " install-path "\n"))
                                           (mkdir-p (dirname install-path))
                                           (copy-file path install-path)))))
                                 (assoc-ref
                                  (vector-ref
                                   (json->scm
                                    (apply open-pipe* OPEN_READ `(,npm "pack" "--json" "--dry-run" "--loglevel=warn" ,@npm-flags))) 0) "files"))
                (for-each
                 (match-lambda
                   ((name . script)
                    (begin
                      (mkdir-p bin-dir)
                      (call-with-output-file (string-append bin-dir "/" name)
                        (lambda (port)
                          (format port
                                  "#!~a~%exec -a \"$0\" \"~a\" \"~a\" \"$@\"~%"
                                  sh
                                  node
                                  (canonicalize-path (string-append package-out "/" script)))
                          ))
                      (chmod (string-append bin-dir "/" name) #o755)
                      )))
                   package-bin)

              (apply invoke `(,npm "prune" "--omit=dev" "--no-save" ,@npm-flags))
              (invoke "find" "node_modules" "-maxdepth" "1" "-type" "d" "-empty" "-delete")
              (copy-recursively "node_modules" node-modules-dir)))))))
    (license #f)
    (home-page #f)))
