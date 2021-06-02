(use-modules (config ada))
(use-modules (config transistor))
(use-modules (config mel))
(use-modules (config seshat))
(use-modules (srfi srfi-1))

(define available-configs
  `((("ada") ,ada-system-config)
    (("mel") ,mel-system-config)
    (("transistor") ,transistor-system-config)
    (("seshat") ,seshat-system-config)))

(cadr (let* ((hostname (gethostname)))
       (find (lambda (config)
                (member hostname (car config)))
          available-configs)))
