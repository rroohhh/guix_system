(define-module (home config)
  #:use-module (vup emacs)
  #:use-module ((gnu packages emacs) #:prefix guix:)
  #:export (vup keys* keys gruvbox-dark theme* theme))

(define-public emacs emacs-pgtk-native-comp)
;; (define-public emacs guix:emacs)

(define-public monospace-font "Hack")

(define-syntax vup
  (syntax-rules (left right up down)
    ((_ left) "s")
    ((_ right) "l")
    ((_ up) "t")
    ((_ down) "n")))

(define-syntax keys*
  (syntax-rules ()
    ((_ args* ...) (vup args* ...))))

(define-syntax keys
  (syntax-rules ()
    ((_ args* ...) (string-append "\"" (keys* args* ...) "\""))))

(define-syntax gruvbox-dark
  (syntax-rules (bg bg1 fg bright black red green yellow blue magenta cyan white orange)
    ((_ bg)             "#282828")
    ((_ bg1)            "#3c3836")
    ((_ fg)             "#ebdbb2")
    ((_ black)          "#282828")
    ((_ red)            "#cc241d")
    ((_ green)          "#98971a")
    ((_ yellow)         "#d79921")
    ((_ blue)           "#458588")
    ((_ magenta)        "#b16286")
    ((_ cyan)           "#689d6a")
    ((_ white)          "#a89984")
    ((_ orange)         "#d65d0e")
    ((_ bright black)   "#928374")
    ((_ bright red)     "#fb4934")
    ((_ bright green)   "#b8bb26")
    ((_ bright yellow)  "#fabd2f")
    ((_ bright blue)    "#83a598")
    ((_ bright magenta) "#d3869b")
    ((_ bright cyan)    "#8ec07c")
    ((_ bright white)   "#ebdbb2")))

(define-syntax theme*
  (syntax-rules ()
    ((_ args* ...) (gruvbox-dark args* ...))))

(define-syntax theme
  (syntax-rules ()
    ((_ args* ...) (string-append "\"" (theme* args* ...) "\""))))

(define-public xauthority-file "/data/robin/.Xauthority")
