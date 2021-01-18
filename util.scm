(define-module (util)
  #:export (read-file-as-string)
  #:use-module (ice-9 textual-ports))

(define (read-file-as-string file)
  (get-string-all (open-input-file file)))
