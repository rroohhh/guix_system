;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix import git-updater)
  #:use-module (git)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:export (%generic-git-updater-head))

(define (git-package? package)
  "Return true if PACKAGE is hosted on a Git repository."
  (and (match (package-source package)
         ((? origin? origin)
          (and (eq? (origin-method origin) git-fetch)
               (git-reference? (origin-uri origin))))
         (_ #f))
       (string-contains (package-version package) "-")))

(define honor-system-x509-certificates!
  (@@ (guix git) honor-system-x509-certificates!))
(define %certificates-initialized?
  (@@ (guix git) honor-system-x509-certificates!))

(define latest-git-tag-version
  (@@ (guix import git) latest-git-tag-version))

(define-syntax-rule (with-libgit2 thunk ...)
  (begin
    ;; XXX: The right thing to do would be to call (libgit2-shutdown) here,
    ;; but pointer finalizers used in guile-git may be called after shutdown,
    ;; resulting in a segfault. Hence, let's skip shutdown call for now.
    (libgit2-init!)
    (unless %certificates-initialized?
      (honor-system-x509-certificates!)
      (set! %certificates-initialized? #t))
    thunk
    ...))

(define* (find-latest-commit url cache-directory)
  (let* ((repository (repository-init cache-directory))
         ;; Create an in-memory remote so we don't touch disk.
         (remote (remote-create-anonymous repository url)))
    (remote-connect remote)

    (let* ((remote-heads (remote-ls remote))
           (head-ref (find (lambda (head)
                             (and (string=? "HEAD"
                                            (remote-head-name head)) head))
                           remote-heads)))
      ;; Wait until we're finished with the repository before closing it.
      (remote-disconnect remote)
      (repository-close! repository)
      (oid->string (remote-head-oid head-ref)))))

(define* (import-git-release pkg-in
                             #:key (version #f))
  "Return an <upstream-source> for the latest release of PACKAGE.
Optionally include a VERSION string to fetch a specific version."
  (let* ((pkg (package
                (inherit pkg-in)
                (properties (append (package-properties pkg-in)
                                    '((accept-pre-releases? #t))))))
         (name (package-name pkg))
         (old-version (package-version pkg))
         (old-reference (origin-uri (package-source pkg)))
         ;; for some reason using the patched pkg doesnt work sometimes
         (new-version-from-tag new-version-tag
                               (latest-git-tag-version pkg-in
                                                       #:version version))
         (new-version-from-tag-pre new-version-tag-pre
                                   (latest-git-tag-version pkg
                                                           #:version version))
         (new-version-from-tag (or (or (and new-version-from-tag-pre
                                            new-version-from-tag-pre)
                                       new-version-from-tag) "0.0"))
         (url (git-reference-url old-reference))
         (latest-commit (with-libgit2 (call-with-temporary-directory
                                       (lambda (cd) (find-latest-commit url cd)))))
         (new-version-from-tag-base (car (string-split new-version-from-tag #\-)))
         (old-ver-split (string-split old-version #\-))
         (old-version-base (car old-ver-split))
         (old-version-rev (last old-ver-split))
         (old-revision (string->number (version-prefix old-version-rev 1)))
         (old-commit (git-reference-commit old-reference))
         (revision (if (eq? '=
                            (version-compare old-version-base
                                             new-version-from-tag-base))
                       (number->string (if (string=? old-commit latest-commit)
                                           old-revision
                                           (+ 1 old-revision))) "0"))
         (new-version (and new-version-from-tag
                           (git-version new-version-from-tag revision
                                        latest-commit))))
    (and new-version
         (upstream-source (package
                            name)
                          (version new-version)
                          (urls (git-reference (url (git-reference-url
                                                     old-reference))
                                               (commit latest-commit)
                                               (recursive? (git-reference-recursive?
                                                            old-reference))))))))

(define %generic-git-updater-head
  (upstream-updater (name 'generic-git-head)
                    (description
                     "Updater for packages hosted on Git repositories to HEAD commit")
                    (pred git-package?)
                    (import import-git-release)))
