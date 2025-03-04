(define-module (local-packages)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix discovery)
  #:use-module (guix profiles))

(define ext ".scm")


(define-public local-packages
 (fold-module-public-variables
  (lambda (obj res)
	(if (package? obj) (cons obj res) res))
  '()
  (all-modules (list (dirname (current-filename))))))

(packages->manifest local-packages)



;; (format #t "~a\n" )
;; (for-each
;;  (lambda (file)
;;    (let ((interface (resolve-interface
;; 					 (map string->symbol
;; 						  (cdr
;; 						   (string-split
;; 							(string-drop-right file (string-length ext)) #\/))))))
;; 	 (format #t "~a\n" interface)))
;;    ;; (primitive-load file)
;;    ;; (format #t "~a\n" file))
;;  (list (car (find-files "." (string-append "\\" ext "$")))))
