(define-module (vup home i3)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix import utils) ;; for flatten
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (i3-configuration
    i3-bar-configuration
    i3-bar-colors-configuration
    i3-bar-color
    make-i3-bar-color
    i3-font-configuration
    i3-color-configuration
    i3-color-spec
    make-i3-color-spec
    i3-home))

(define-record-type* <i3-configuration>
    i3-configuration make-i3-configuration i3-configuration?
    (extra-config i3-configuration-extra-config (default '()))
    (colors i3-configuration-colors)
    (font i3-configuration-font)
    (bar i3-configuration-bar)
    (inner-gaps i3-configuration-inner-gaps (default #f))
    (outer-gaps i3-configuration-inner-gaps (default #f))
    (smart-gaps i3-configuration-smart-gaps (default #f))
    (smart-borders i3-configuration-smart-gaps (default #f))
    (floating-modifier i3-configuration-floating-modifier)
    (key-bindings i3-configuration-key-bindings (default '())))

(define-record-type* <i3-bar-configuration>
    i3-bar-configuration make-i3-bar-configuration i3-bar-configuration?
    (bar-command i3-bar-configuration-bar-command))

(define-record-type* <i3-bar-colors-configuration>
    i3-bar-colors-configuration make-i3-bar-colors-configuration i3-bar-colors-configuration?
    (statusline i3-bar-colors-configuration-statusline)
    (background i3-bar-colors-configuration-background)
    (separator i3-bar-colors-configuration-separator)
    (focused-workspace i3-bar-colors-configuration-focused-workspace)
    (inactive-workspace i3-bar-colors-configuration-inactive-workspace)
    (active-workspace i3-bar-colors-configuration-active-workspace)
    (urgent-workspace i3-bar-colors-configuration-urgent-workspace)
    (binding-mode i3-bar-colors-configuration-binding-mode))

(define-record-type* <i3-bar-color>
    i3-bar-color make-i3-bar-color i3-bar-color?
    (border i3-bar-color-border)
    (background i3-bar-color-background)
    (text i3-bar-color-text))

(define-record-type* <i3-font-configuration> 
    i3-font-configuration make-i3-font-configuration i3-font-configuration?
    (families i3-font-configuration-name)
    (size i3-size-configuration-size))

(define-record-type* <i3-color-configuration>
    i3-color-configuration make-i3-color-configuration i3-color-configuration?
    (focused i3-color-configuration-focused)
    (focused-inactive i3-color-configuration-focused-inactive)
    (unfocused i3-color-configuration-unfocused)
    (urgent i3-color-configuration-urgent)
    (placeholder i3-color-configuration-placeholder)
    (background i3-color-configuration-background))


(define (generate-i3-color-config config)
  (match config
    (($ <i3-color-configuration> focused focused-inactive
        unfocused urgent placeholder background)
     (list
       (format #f "client.focused ~a" (generate-i3-color focused))
       (format #f "client.focused_inactive ~a" (generate-i3-color focused-inactive))
       (format #f "client.unfocused ~a" (generate-i3-color unfocused))
       (format #f "client.urgent ~a" (generate-i3-color urgent))
       (format #f "client.placeholder ~a" (generate-i3-color placeholder))
       (format #f "client.background ~a" background)))))


(define-record-type* <i3-color-spec>
    i3-color-spec make-i3-color-spec i3-color-spec?
    (border i3-color-spec-border)
    (background i3-color-spec-border)
    (text i3-color-spec-border)
    (indicator i3-color-spec-border)
    (child-border i3-color-spec-border))

(define (generate-i3-color config)
  (match config
    (($ <i3-color-spec> border background text
        indicator child-border)
     (format #f "~a ~a ~a ~a ~a" border background text indicator child-border))))

(define (generate-i3-font-config config)
  (match config
         (($ <i3-font-configuration> families size)
          (list (format #f "font pango:~a ~a"
            (string-join families ", ")
            size)))))

(define (generate-i3-key-bindings config)
  (map 
    (lambda (binding) 
      `("bindsym " ,(string-join (car binding) "+") " " ,(cadr binding)))
    config))

(define (i3-yes-no bool)
  (match bool
         (#t "yes")
         (#f "no")))

(define (generate-i3-bar-color config)
  (match config
    (($ <i3-bar-color> border background text)
     (format #f "~a ~a ~a" border background text))))

(define (generate-i3-bar-color-config config)
  (match config
    (($ <i3-bar-colors-configuration> statusline background 
        separator focused-workspace inactive-workspace 
        active-workspace urgent-workspace binding-mode)
     (list
       "colors {"
       (format #f "statusline ~a" statusline)
       (format #f "background ~a" background)
       (format #f "separator ~a" separator)
       (format #f "focused_workspace ~a" (generate-i3-bar-color focused-workspace))
       (format #f "inactive_workspace ~a" (generate-i3-bar-color inactive-workspace))
       (format #f "active_workspace ~a" (generate-i3-bar-color active-workspace))
       (format #f "urgent_workspace ~a" (generate-i3-bar-color urgent-workspace))
       (format #f "binding_mode ~a" (generate-i3-bar-color binding-mode))
       "}"))))

(define (generate-i3-bar-config config)
  (match config
    (($ <i3-bar-configuration> bar-command)
      `("bar {"
        ("swaybar_command " ,bar-command)
        "}")
     )))


(define (generate-i3-config config)
  (match config
     (($ <i3-configuration> extra-config colors font bar 
         inner-gaps outer-gaps smart-gaps 
         smart-borders floating-modifier 
         key-bindings)
     (flatten (map (lambda (line) `(,line "\n")) 
       (append 
         (list 
           (if inner-gaps
               (format #f "gaps inner ~d" inner-gaps))
           (if outer-gaps
               (format #f "gaps outer ~d" outer-gaps))
           (if smart-gaps
               (format #f "smart_gaps on"))
           (if smart-borders
               (format #f "smart_borders on"))
           (string-append "floating_modifier " floating-modifier))
         extra-config
         (generate-i3-font-config font)
         (generate-i3-bar-config bar)
         (generate-i3-color-config colors)
         (generate-i3-key-bindings key-bindings)))))))

(define (i3-home config)
  (computed-file "i3-home"
    #~(let ((config #$(computed-file "config" #~(with-output-to-file #$output 
	                                          (lambda _ 
						    (set-port-encoding! (current-output-port) "UTF-8")    ;; shitty hack for unicode to work
						    (format #t "~a" (string-append #$@(generate-i3-config config)))))))  
;;    #~(let ((config #$(apply mixed-text-file "config" (generate-i3-config config)))   ; can't use this, as it breaks unicode :(
	    (i3-dir (string-append #$output "")))
	(use-modules (guix build utils))
	(mkdir-p i3-dir)
	(copy-file config (string-append i3-dir "/config")))
    #:options
    '(#:local-build? #t
      #:modules ((guix build utils)))))
