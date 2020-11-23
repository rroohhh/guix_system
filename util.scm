(define-module (util)
  #:export (read-file-as-string))

(define (read-file-as-string file)
  (symbol->string (read (open-input-file file))))
