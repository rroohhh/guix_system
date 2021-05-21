(add-to-load-path (dirname (current-filename))) 
(use-modules (vup patches))

(use-modules (vup xkeylogger))
(use-modules (vup hwinfo))
(use-modules (vup emacs))
(use-modules (vup ip_addr))
(use-modules (vup home i3))

(use-modules (util))

(use-modules (home))
(use-modules (home bash))
(use-modules (home utils))
(use-modules (home profile))

(use-modules (gnu packages admin))
(use-modules (gnu packages dunst))
(use-modules (gnu packages glib))
(use-modules (gnu packages irc))
(use-modules (gnu packages linux))
(use-modules (gnu packages ncurses))
(use-modules (gnu packages perl))
(use-modules (gnu packages pulseaudio))
(use-modules (gnu packages rust-apps))
(use-modules (gnu packages telegram))
(use-modules (gnu packages terminals))
(use-modules (gnu packages tmux))
(use-modules (gnu packages wm))
(use-modules (gnu packages xdisorg))
(use-modules (gnu packages xorg))

(use-modules (guix gexp))

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

(use-modules (srfi srfi-1)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (reader-extension-raw-string chr port)
; open-close choices
  (define delim-begin "([\"")
  (define (char-please port)
    (let ((c (read-char port)))
      (if (eof-object? c)
        (throw 'end-of-file-reading-raw-string)
        c)))
  (let* ((fix-open (read-delimited delim-begin port 'split))
         (fix (car fix-open))
         (open (cdr fix-open))
; match open-close characters
         (close (case open
                  ((#\() #\)) ((#\[) #\]) ((#\") #\")
                  (else (throw 'raw-string-delimiter-not-found fix)))))
    (when (string-index fix char-whitespace?)
      (throw 'raw-string-delimiter-has-whitespace fix))
    (let search-delim ((c (char-please port)) (s '()))
      (if (eqv? close c)
        (let search-close ((ss (list close)) (i 0))
          (if (= i (string-length fix))
            (list->string (reverse! s))
            (let ((c (char-please port)))
              (if (eqv? (string-ref fix i) c)
                (search-close (cons c ss) (+ 1 i))
                (search-delim (char-please port) (append (cons c ss) s))))))
        (search-delim (char-please port) (cons c s))))))

(read-hash-extend #\R reader-extension-raw-string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define emacs emacs-pgtk-native-comp)

(define monospace-font "Hack")

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

(define xauthority-file "/data/robin/.Xauthority")

(define emacs-terminfo
  (computed-file "terminfo"
    #~(let ((terminfo-src #$(plain-file "terminfo-24bit.src" "xterm-24bits|xterm with 24-bit direct color mode,
  use=xterm-256color,
  setb24=\\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
  setf24=\\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,"))
            (out (string-append #$output "")))
        (use-modules (guix build utils))
        (mkdir-p out)
        (apply system* (list #$(file-append ncurses "/bin/tic") "-x" "-o" out terminfo-src)))
    #:options
    '(#:local-build? #t
      #:modules ((guix build utils)))))


(define alacritty-config
  (plain-file "alacritty.yml" (string-append "env:
  TERM: xterm-256color
  WINIT_HIDPI_FACTOR: \"1.0\"
  WINIT_X11_SCALE_FACTOR: \"1.0\"

window:
  dimensions:
    columns: 0
    lines: 0
  padding:
    x: 2
    y: 2
  dynamic_padding: false
  decorations: full

scrolling:
  history: 10000
  multiplier: 3
  auto_scroll: false

tabspaces: 4

font:
  normal:
    family: " monospace-font "
    style: Regular
  bold:
    family: " monospace-font "
    style: Bold
  italic:
    family: " monospace-font "
    style: Italic
#  size: 11.5
  size: 10.0
  offset:
    x: 0
    y: 0
  glyph_offset:
    x: 0
    y: 0
  use_thin_strokes: true

debug:
    render_timer: false
    persistent_logging: false

draw_bold_text_with_bright_colors: false

colors:
  cursor:
    cursor: " (theme red) "
    text: " (theme fg) "
  primary:
    background: " (theme bg) "
    foreground: " (theme fg) "
  normal:
    black:   " (theme black) "
    red:     " (theme red) "
    green:   " (theme green) "
    yellow:  " (theme yellow) "
    blue:    " (theme blue) "
    magenta: " (theme magenta) "
    cyan:    " (theme cyan) "
    white:   " (theme white) "
  bright:
    black:   " (theme bright black) "
    red:     " (theme bright red) "
    green:   " (theme bright green) "
    yellow:  " (theme bright yellow) "
    blue:    " (theme bright blue) "
    magenta: " (theme bright magenta) "
    cyan:    " (theme bright cyan) "
    white:   " (theme bright white) "
  indexed_colors: []
bell:
  animation: EaseOutExpo
  duration: 0
  color: '0xffffff'
background_opacity: 1.0
mouse_bindings:
  - { mouse: Middle, action: PasteSelection }
mouse:
  double_click: { threshold: 300 }
  triple_click: { threshold: 300 }
  hide_when_typing: true
  url:
    #launcher: xdg-open
    modifiers: None
selection:
  semantic_escape_chars: \",│`|:\\\"' ()[]{}<>\"
  save_to_clipboard: false
dynamic_title: true

cursor:
  style: Block
  unfocused_hollow: true
live_config_reload: true

#shell:
#  program: /bin/bash
#  args:
#    - --login

alt_send_esc: true
key_bindings:
  - { key: Space,    mods: Shift|Control,           chars: \"\\x00\"       }
  - { key: Return,   mods: Control, chars: \"\x1b[27;5;13~\"               }
  - { key: Paste,                   action: Paste                          }
  - { key: Copy,                    action: Copy                           }
  - { key: L,        mods: Control, action: ClearLogNotice                 }
  - { key: L,        mods: Control, chars: \"\x0c\"                        }
  - { key: Home,                    chars: \"\x1bOH\",   mode: AppCursor   }
  - { key: Home,                    chars: \"\x1b[H\",   mode: ~AppCursor  }
  - { key: End,                     chars: \"\x1bOF\",   mode: AppCursor   }
  - { key: End,                     chars: \"\x1b[F\",   mode: ~AppCursor  }
  - { key: PageUp,   mods: Shift,   chars: \"\x1b[5;2~\"                   }
  - { key: PageUp,   mods: Control, chars: \"\x1b[5;5~\"                   }
  - { key: PageUp,                  chars: \"\x1b[5~\"                     }
  - { key: PageDown, mods: Shift,   chars: \"\x1b[6;2~\"                   }
  - { key: PageDown, mods: Control, chars: \"\x1b[6;5~\"                   }
  - { key: PageDown,                chars: \"\x1b[6~\"                     }
  - { key: Tab,      mods: Shift,   chars: \"\x1b[Z\"                      }
  - { key: Back,                    chars: \"\x7f\"                        }
  - { key: Back,     mods: Alt,     chars: \"\x1b\x7f\"                    }
  - { key: Insert,                  chars: \"\x1b[2~\"                     }
  - { key: Delete,                  chars: \"\x1b[3~\"                     }
  - { key: Left,     mods: Shift,   chars: \"\x1b[1;2D\"                   }
  - { key: Left,     mods: Control, chars: \"\x1b[1;5D\"                   }
  - { key: Left,     mods: Alt,     chars: \"\x1b[1;3D\"                   }
  - { key: Left,                    chars: \"\x1b[D\",   mode: ~AppCursor  }
  - { key: Left,                    chars: \"\x1bOD\",   mode: AppCursor   }
  - { key: Right,    mods: Shift,   chars: \"\x1b[1;2C\"                   }
  - { key: Right,    mods: Control, chars: \"\x1b[1;5C\"                   }
  - { key: Right,    mods: Alt,     chars: \"\x1b[1;3C\"                   }
  - { key: Right,                   chars: \"\x1b[C\",   mode: ~AppCursor  }
  - { key: Right,                   chars: \"\x1bOC\",   mode: AppCursor   }
  - { key: Up,       mods: Shift,   chars: \"\x1b[1;2A\"                   }
  - { key: Up,       mods: Control, chars: \"\x1b[1;5A\"                   }
  - { key: Up,       mods: Alt,     chars: \"\x1b[1;3A\"                   }
  - { key: Up,                      chars: \"\x1b[A\",   mode: ~AppCursor  }
  - { key: Up,                      chars: \"\x1bOA\",   mode: AppCursor   }
  - { key: Down,     mods: Shift,   chars: \"\x1b[1;2B\"                   }
  - { key: Down,     mods: Control, chars: \"\x1b[1;5B\"                   }
  - { key: Down,     mods: Alt,     chars: \"\x1b[1;3B\"                   }
  - { key: Down,                    chars: \"\x1b[B\",   mode: ~AppCursor  }
  - { key: Down,                    chars: \"\x1bOB\",   mode: AppCursor   }
  - { key: F1,                      chars: \"\x1bOP\"                      }
  - { key: F2,                      chars: \"\x1bOQ\"                      }
  - { key: F3,                      chars: \"\x1bOR\"                      }
  - { key: F4,                      chars: \"\x1bOS\"                      }
  - { key: F5,                      chars: \"\x1b[15~\"                    }
  - { key: F6,                      chars: \"\x1b[17~\"                    }
  - { key: F7,                      chars: \"\x1b[18~\"                    }
  - { key: F8,                      chars: \"\x1b[19~\"                    }
  - { key: F9,                      chars: \"\x1b[20~\"                    }
  - { key: F10,                     chars: \"\x1b[21~\"                    }
  - { key: F11,                     chars: \"\x1b[23~\"                    }
  - { key: F12,                     chars: \"\x1b[24~\"                    }
  - { key: F1,       mods: Shift,   chars: \"\x1b[1;2P\"                   }
  - { key: F2,       mods: Shift,   chars: \"\x1b[1;2Q\"                   }
  - { key: F3,       mods: Shift,   chars: \"\x1b[1;2R\"                   }
  - { key: F4,       mods: Shift,   chars: \"\x1b[1;2S\"                   }
  - { key: F5,       mods: Shift,   chars: \"\x1b[15;2~\"                  }
  - { key: F6,       mods: Shift,   chars: \"\x1b[17;2~\"                  }
  - { key: F7,       mods: Shift,   chars: \"\x1b[18;2~\"                  }
  - { key: F8,       mods: Shift,   chars: \"\x1b[19;2~\"                  }
  - { key: F9,       mods: Shift,   chars: \"\x1b[20;2~\"                  }
  - { key: F10,      mods: Shift,   chars: \"\x1b[21;2~\"                  }
  - { key: F11,      mods: Shift,   chars: \"\x1b[23;2~\"                  }
  - { key: F12,      mods: Shift,   chars: \"\x1b[24;2~\"                  }
  - { key: F1,       mods: Control, chars: \"\x1b[1;5P\"                   }
  - { key: F2,       mods: Control, chars: \"\x1b[1;5Q\"                   }
  - { key: F3,       mods: Control, chars: \"\x1b[1;5R\"                   }
  - { key: F4,       mods: Control, chars: \"\x1b[1;5S\"                   }
  - { key: F5,       mods: Control, chars: \"\x1b[15;5~\"                  }
  - { key: F6,       mods: Control, chars: \"\x1b[17;5~\"                  }
  - { key: F7,       mods: Control, chars: \"\x1b[18;5~\"                  }
  - { key: F8,       mods: Control, chars: \"\x1b[19;5~\"                  }
  - { key: F9,       mods: Control, chars: \"\x1b[20;5~\"                  }
  - { key: F10,      mods: Control, chars: \"\x1b[21;5~\"                  }
  - { key: F11,      mods: Control, chars: \"\x1b[23;5~\"                  }
  - { key: F12,      mods: Control, chars: \"\x1b[24;5~\"                  }
  - { key: F1,       mods: Alt,     chars: \"\x1b[1;6P\"                   }
  - { key: F2,       mods: Alt,     chars: \"\x1b[1;6Q\"                   }
  - { key: F3,       mods: Alt,     chars: \"\x1b[1;6R\"                   }
  - { key: F4,       mods: Alt,     chars: \"\x1b[1;6S\"                   }
  - { key: F5,       mods: Alt,     chars: \"\x1b[15;6~\"                  }
  - { key: F6,       mods: Alt,     chars: \"\x1b[17;6~\"                  }
  - { key: F7,       mods: Alt,     chars: \"\x1b[18;6~\"                  }
  - { key: F8,       mods: Alt,     chars: \"\x1b[19;6~\"                  }
  - { key: F9,       mods: Alt,     chars: \"\x1b[20;6~\"                  }
  - { key: F10,      mods: Alt,     chars: \"\x1b[21;6~\"                  }
  - { key: F11,      mods: Alt,     chars: \"\x1b[23;6~\"                  }
  - { key: F12,      mods: Alt,     chars: \"\x1b[24;6~\"                  }
  - { key: F1,       mods: Super,   chars: \"\x1b[1;3P\"                   }
  - { key: F2,       mods: Super,   chars: \"\x1b[1;3Q\"                   }
  - { key: F3,       mods: Super,   chars: \"\x1b[1;3R\"                   }
  - { key: F4,       mods: Super,   chars: \"\x1b[1;3S\"                   }
  - { key: F5,       mods: Super,   chars: \"\x1b[15;3~\"                  }
  - { key: F6,       mods: Super,   chars: \"\x1b[17;3~\"                  }
  - { key: F7,       mods: Super,   chars: \"\x1b[18;3~\"                  }
  - { key: F8,       mods: Super,   chars: \"\x1b[19;3~\"                  }
  - { key: F9,       mods: Super,   chars: \"\x1b[20;3~\"                  }
  - { key: F10,      mods: Super,   chars: \"\x1b[21;3~\"                  }
  - { key: F11,      mods: Super,   chars: \"\x1b[23;3~\"                  }
  - { key: F12,      mods: Super,   chars: \"\x1b[24;3~\"                  }
  - { key: NumpadEnter,             chars: \"\n\"                          }")))

(define ideavim-config
  (plain-file "tmux.conf" (string-append "
nnoremap " (keys* left) " h
nnoremap " (keys* down) " j
nnoremap " (keys* up) " k
nnoremap m n
nnoremap M N
nnoremap h <Nop>
nnoremap j <Nop>
nnoremap k <Nop>

vnoremap " (keys* left) " h
vnoremap " (keys* down) " j
vnoremap " (keys* up) " k
vnoremap m n
vnoremap M N
vnoremap h <Nop>
vnoremap j <Nop>
vnoremap k <Nop>")))


(define tmux-config
  (plain-file "tmux.conf" (string-append "setw -g aggressive-resize on
set-window-option -g xterm-keys on
set-window-option -g mode-key vi
set-option -g history-limit 100000
set -sg escape-time 0

unbind C-Space

unbind C-b
set-option -g prefix C-a
bind-key C-a send-prefix

set-option -g set-titles on
set-option -g set-titles-string '#W'
set -g default-terminal 'xterm-256color'
# set -ga terminal-overrides ',*256col*:Tc'
set -ga terminal-overrides ',*:Tc'

bind -n M-v split-window -v -c '#{pane_current_path}'
bind -n M-f split-window -h -c '#{pane_current_path}'
bind -n M-i kill-pane
bind -n M-w kill-window
bind -n M-c new-window

bind r source-file ~/.tmux.conf

bind-key -n M-" (keys* left) " select-pane -L
bind-key -n M-" (keys* down) " select-pane -D
bind-key -n M-" (keys* up) " select-pane -U
bind-key -n M-" (keys* right) " select-pane -R

setw -g monitor-activity on
bind -n M-" (string-upcase (keys* left)) " previous-window
bind -n M-" (string-upcase (keys* right)) " next-window
bind -n M-m copy-mode

set-option -g renumber-windows on
set-option -g status on


set-option -g pane-active-border-style fg=colour250 #fg2
set-option -g pane-border-style fg=colour239 #bg1

setw -g clock-mode-colour colour109 #blue
setw -g window-status-bell-style fg=colour235,bg=colour167 #bg, red
setw -g window-status-current-style fg=colour237,bg=default
setw -g window-status-style fg=colour246,bg=colour237
setw -g window-status-activity-style bg=colour237,fg=colour246
setw -g window-status-separator \"\"

set -g message-command-style fg=colour246,bg=colour239
set -g message-style fg=colour246,bg=colour239

set -g status-style bg=colour237
set -g status-justify left

set -g status-left-length 100
set -g status-right-length 100

set -g status-left ''


set -g status-right \"#[fg=colour239,bg=colour237,nobold,nounderscore,noitalics]#[fg=colour246,bg=colour239] %Y-%m-%d  %H:%M #[fg=colour246,bg=colour239,nobold,nounderscore,noitalics]#[fg=colour235,bg=colour246] #h \"

setw -g window-status-format \"#[fg=colour246,bg=colour237] #I #[fg=colour246,bg=colour237] #W \"

setw -g window-status-current-format \"#[fg=colour237,bg=colour214,nobold,nounderscore,noitalics]#[fg=colour235,bg=colour214] #I #[fg=colour235,bg=colour214] #W #[fg=colour214,bg=colour237,nobold,nounderscore,noitalics]\"")))

(define gitconfig
  (plain-file "gitconfig" "[user]
        email = robin.ole.heinemann@gmail.com
        name = Robin Ole Heinemann
[pull]
ff = only

[init]
defaultBranch = main"))

(define sway-config
  (let*
    ((mod "Mod4")
     (left (keys* left))
     (down (keys* down))
     (up (keys* up))
     (right (keys* right))
     (bg-color (theme bg))
     (bg-hl-color "#383838")
     (fg-color (theme fg))
     (border-color (theme fg))
     (fonts `(,monospace-font "FontAwesome"))
     (config 
       (i3-configuration
         (extra-config 
           '("for_window [ title=\"^pdfpc - present\" ] border none floating enable"
             "for_window [ title=\"^org.anbox.*\" ] border none floating enable"
             "default_border pixel 3"
	     "input * {
  natural_scroll enabled
  xkb_layout de
  xkb_variant vup
}"))
        (bar (i3-bar-configuration
                 (bar-command (file-append sway "/bin/swaybar"))
                 (position "top") 
                 (workspace-buttons #t) 
                 (status-command (file-append i3blocks "/bin/i3blocks")) 
                 (font (i3-font-configuration
                         (families fonts)
                         (size 11))) 
                 (strip-workspace-numbers #t)
                 (colors (i3-bar-colors-configuration
                           (statusline fg-color)
                           (background bg-color)
                           (separator bg-color)
                           (focused-workspace 
                             (make-i3-bar-color bg-color bg-color (theme green)))
                           (inactive-workspace 
                             (make-i3-bar-color bg-color bg-color (theme bright black)))
                           (active-workspace 
                             (make-i3-bar-color bg-color bg-color (theme green)))
                           (urgent-workspace 
                             (make-i3-bar-color bg-color bg-color (theme red)))
                           (binding-mode
                             (make-i3-bar-color bg-color bg-color (theme red)))))))
         (colors (i3-color-configuration 
                   (focused 
                     (make-i3-color-spec bg-color bg-color fg-color "#666666" "#5f676a"))
                   (focused-inactive 
                     (make-i3-color-spec bg-color bg-color fg-color bg-hl-color bg-hl-color))
                   (unfocused 
                     (make-i3-color-spec bg-hl-color bg-hl-color fg-color bg-hl-color "#222222"))
                   (urgent 
                     (make-i3-color-spec "#2f343a" "#900000" fg-color bg-hl-color "#900000"))
                   (placeholder 
                     (make-i3-color-spec "#000000" "#0c0c0c" fg-color bg-hl-color "#ffffff"))
                   (background bg-color)))
         (font (i3-font-configuration
                 (families fonts)
                 (size 8)))
         (floating-modifier mod)
         (inner-gaps 10)
         (outer-gaps 7)
         (smart-gaps #t)
         (smart-borders #t)
         (key-bindings
           `(((,mod "f") "fullscreen toggle")
             ((,mod "Return") ("exec " ,(file-append alacritty "/bin/alacritty")))
             ((,mod "Shift" "q") "kill")
             ((,mod "d") ("exec " ,(file-append rofi "/bin/rofi") " -show run"))
             ((,mod ,left) "focus left")
             ((,mod ,down) "focus down")
             ((,mod ,up) "focus up")
             ((,mod ,right) "focus right")
             ((,mod "a") "focus parent")
             ((,mod "Shift" ,left) "move left")
             ((,mod "Shift" ,down) "move down")
             ((,mod "Shift" ,up) "move up")
             ((,mod "Shift" ,right) "move right")
             ((,mod "Shift" "v") "split h")
             ((,mod "v") "split v")
             ((,mod "w") "layout tabbed")
             ((,mod "e") "layout toggle split")
             ((,mod "space") "floating toggle")
             ((,mod "Shift" "c") "reload")
             ((,mod "Shift" "r") "restart")
             ((,mod "Shift" "e") "exit")
             (("XF86MonBrightnessUp") "exec sh -c 'hwinfo backlight brightness $(($(hwinfo backlight brightness) + 20))'")
             (("XF86MonBrightnessDown") "exec sh -c 'hwinfo backlight brightness $(($(hwinfo backlight brightness) - 20))'")
             (("XF86AudioRaiseVolume") ("exec " ,(file-append pulseaudio "/bin/pactl") " set-sink-volume 0 +5%"))
             (("XF86AudioLowerVolume") ("exec " ,(file-append pulseaudio "/bin/pactl") " set-sink-volume 0 -5%"))
             (("XF86AudioMute") ("exec " ,(file-append pulseaudio "/bin/pactl") " set-sink-mute 0 toggle"))
             ,@(fold-right append '() (map (lambda (x)
                     (let*
                       ((ws-num (+ x 1))
                        (ws (format #f "~a" ws-num))
                        (ws-key (modulo ws-num 10))
                        (ws-key (format #f "~a" ws-key))
                        (ws-pretty #("¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹" "⁰"))
                        (ws-name (format #f "~a:~a" ws (array-ref ws-pretty x))))
                       `(((,mod ,ws-key) 
                          ,(string-join (list "workspace" ws-name)))
                         ((,mod "Shift" ,ws-key) 
                          ,(string-join (list "move container to workspace" ws-name)))))) 
                   (iota 10))))))))
    config))

(define shepherd-config
  (scheme-file "init.scm"
    #~(begin
(use-modules
 (ice-9 popen)
 (ice-9 rdelim)
 (ice-9 regex)
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-26))

(define (environment-excursion env-thunk body-thunk)
  (let ((old-env (environ)))
    (dynamic-wind
      env-thunk
      body-thunk
      (lambda () (environ old-env)))))

(define-syntax-rule (with-environment-excursion env body ...)
  (environment-excursion
   (lambda () (environ env))
   (lambda () body ...)))

(define (display-string->number string)
  (string->number
   (substring string
              (+ 1 (string-index string #\:)))))

(define (plist-fold proc init plist)
  (let loop ((result init)
             (current plist))
    (match current
      (()
       result)
      ((prop val rest ...)
       (loop (proc prop val result)
             rest)))))

(define (plist-get plist property)
  (match plist
    ((prop val rest ...)
     (if (eq? prop property)
         val
         (plist-get rest property)))
    (_ #f)))

(define (plist-add plist property value)
  (cons* property value plist))

(define (plist-delete plist property)
  (plist-fold (lambda (prop val res)
                (if (eq? prop property)
                    res
                    (plist-add res prop val)))
              '()
              plist))

(define (plist-put plist property value)
  (plist-add (plist-delete plist property)
             property value))

(define (plist-new old-plist . add-plist)
  (plist-fold (lambda (prop val res)
                (plist-put res prop val))
              old-plist
              add-plist))

(define (make-service . args)
  (apply make <service> args))


(define (%dbus-address display)
  (string-append (format #f "unix:path=/tmp/dbus-~a-" (getuid)) (substring display 1)))

(define %ssh-socket #f)         ; set by 'run-gpg-agent'

(define (->symbol string-or-symbol)
  (if (symbol? string-or-symbol)
      string-or-symbol
      (string->symbol string-or-symbol)))

(define (display->vt display)
  "Convert DISPLAY string into a string with VT number.
Use 'vt1' for display ':0', vt2 for ':1', etc."
  (let ((display-num (display-string->number display)))
    (string-append "vt" (number->string (+ 1 display-num)))))

(define* (environ* display #:optional home)
  (environment-excursion
   (lambda ()
     (setenv "DBUS_SESSION_BUS_ADDRESS" (%dbus-address display))
     (when %ssh-socket
       (setenv "SSH_AUTH_SOCK" %ssh-socket))
     (when home
       (setenv "HOME" home))
     (setenv "DISPLAY" display)
     (setenv "XDG_CURRENT_DESKTOP" "sway")
     (setenv "WAYLAND_DISPLAY" (string-append
                                "wayland-" (substring display 1))))
   environ))

(define (run-command command)
  (zero? (status:exit-val (apply system* command))))

(define (make-system-constructor command)
  (lambda _
    (run-command command)))

(define (make-system-destructor command)
  (lambda _
    (not (run-command command))))

(define* (make-system-constructor-with-env command #:key display)
  (lambda _
    (with-environment-excursion (environ* display)
      (run-command command))))

(define* (make-forkexec-constructor-with-env command #:key display)
  (lambda args
    (apply (make-forkexec-constructor
            command
            #:environment-variables (environ* display))
           args)))

(define (display-service-name display base-name)
  (symbol-append base-name (string->symbol display)))

(define (display-services-names display base-names)
  (map (cut display-service-name display <>)
       base-names))

(define (display-service-description display base-description)
  (format #f "~a (DISPLAY=~a)" base-description display))

(define* (make-display-service #:key display
                               (docstring "Unknown")
                               (provides '())
                               (requires '())
                               #:allow-other-keys
                               #:rest args)
  (apply make-service
         (plist-new args
           #:docstring (display-service-description display docstring)
           #:provides (display-services-names display provides)
           #:requires (display-services-names display requires))))

(define (make-simple-display-service display . args)
  (apply make-display-service
         #:display display args))

(define* (make-simple-forkexec-display-service display #:key command
                                               #:allow-other-keys
                                               #:rest args)
  (apply make-simple-display-service display
         #:start (make-forkexec-constructor-with-env
                  command
                  #:display display)
         #:stop (make-kill-destructor)
         args))

(define* (make-simple-system-display-service display #:key command
                                             #:allow-other-keys
                                             #:rest args)
  (apply make-simple-display-service display
         #:start (make-system-constructor-with-env
                  command
                  #:display display)
         args))

(define (dbus display)
  (make-simple-forkexec-display-service display
    #:docstring "D-Bus Session Daemon"
    #:provides '(dbus)
    #:command (list #$(file-append dbus "/bin/dbus-daemon") "--session" "--nofork"
                   "--address" (%dbus-address display))))

(define emacs-daemon
  (make-service
    #:docstring "Emacs daemon"
    #:provides '(emacsd)
    #:start
    (make-system-constructor
     `(,#$(file-append emacs "/bin/emacs") "--daemon"))
    #:stop
    (make-system-destructor
     '(,#$(file-append emacs "/bin/emacsclient") "--eval" "(let (kill-emacs-hook) (kill-emacs))"))))

(define (make-simple-fork-constructor command)
  (lambda _
    (let ((pid (primitive-fork)))
      (if (zero? pid)
          (zero? (status:exit-val (apply system* command)))
      pid))))

(define* (pulseaudio-service display)
  (make-display-service 
    #:display display
    #:docstring "pulseaudio"
    #:provides '(pulseaudio)
    #:start (lambda _
          (with-environment-excursion (environ* display (format #f "/tmp/pa-~a" (getuid)))
                      (run-command (list #$(file-append pulseaudio "/bin/pulseaudio") "--start"))))
    #:stop (make-kill-destructor)))

(define (pipewire-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Pipewire daemon"
    #:requires '(dbus)
    #:provides '(pipewire)
    #:command `(,#$(file-append pipewire-0.3 "/bin/pipewire"))))

(define (pipewire-media-session-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Pipewire media session daemon"
    #:requires '(pipewire)
    #:provides '(pipewire-media-session)
    #:command `(,#$(file-append pipewire-0.3 "/bin/pipewire-media-session"))))

(define (pipewire-pulse-service display)
  (make-simple-forkexec-display-service display
    #:docstring "Pipewire pulse daemon"
    #:requires '(pipewire)
    #:provides '(pipewire-pulse)
    #:command `(,#$(file-append pipewire-0.3 "/bin/pipewire-pulse"))))

(define (xkeylogger-service display)
  (make-simple-forkexec-display-service display
    #:docstring "xkeylogger"
    #:provides '(xkeylogger)
    #:command `(,#$(file-append xkeylogger "/bin/xkeylogger") "/data/projects/keyboard/xkeylogger.log")))

;; (define (sway-service display)
;;   (make-simple-forkexec-display-service display
;;     #:docstring "sway"
;;     #:provides '(sway wm)
;;     #:command `(,#$(file-append sway "/bin/sway"))))

(define* (pulseaudio-service display)
  (make-display-service 
    #:display display
    #:docstring "pulseaudio"
    #:provides '(pulseaudio)
    #:start (lambda _
          (with-environment-excursion (environ* display (format #f "/tmp/pa-~a" (getuid)))
                      (run-command (list #$(file-append pulseaudio "/bin/pulseaudio") "--start"))))
    #:stop (make-kill-destructor)))


(define sway
  (make-service
    #:docstring "sway"
    #:requires '(dbus:0)
    #:provides '(sway wm)
    #:start (make-forkexec-constructor (list #$(file-append sway "/bin/sway"))
				                       #:environment-variables (cons*
                                                                (string-append "DBUS_SESSION_BUS_ADDRESS=" (%dbus-address ":0"))
                                                                "XDG_CURRENT_DESKTOP=sway"
                                                                (default-environment-variables)))
    #:stop
    (make-kill-destructor)))

(define (i3-service display)
  (make-simple-forkexec-display-service display
    #:docstring "i3"
    #:provides '(i3 wm)
    #:command `(,#$(file-append i3-gaps "/bin/i3"))))

(define (emacsclient-service display)
   (make-simple-forkexec-display-service display
     #:docstring "Emacsclient"
     #:provides '(emacsclient)
     #:command `(,#$(file-append emacs "/bin/emacsclient") "-c")))

(define (tmux-service display)
   (make-simple-system-display-service display
     #:docstring "tmux server"
     #:provides '(tmux)
     #:command `(,#$(file-append tmux "/bin/tmux") "new-session" "-t" "robin" "-d")))

(define (quasselclient-service display)
   (make-simple-forkexec-display-service display
     #:docstring "Quasselclient"
     #:provides '(quasselclient)
     #:command `(,#$(file-append quassel "/bin/quasselclient"))))

(define (telegram-service display)
   (make-simple-forkexec-display-service display
     #:docstring "Telegram"
     #:provides '(telegram)
     #:command `(,#$(file-append telegram-desktop "/bin/telegram-desktop"))))

(define (pavucontrol-service display)
   (make-simple-forkexec-display-service display
     #:docstring "Pavucontrol"
     #:provides '(pavucontrol)
     #:command `(,#$(file-append pavucontrol "/bin/pavucontrol"))))

(define (dunst-service display)
   (make-simple-forkexec-display-service display
     #:docstring "Dunst"
     #:provides '(dunst)
     #:command `(,#$(file-append dunst "/bin/dunst"))))

(define (redshift-service display)
   (make-simple-forkexec-display-service display
     #:docstring "Redshift"
     #:provides '(redshift)
     #:command `(,#$(file-append redshift "/bin/redshift") "-l" "49.4057072:8.6135749" "-b" "1.0:0.7" "-t" "6500k:2500k")))

(define (natural-scrolling-service display)
   (make-simple-forkexec-display-service display
     #:one-shot? #t
     #:docstring "set natural scrolling"
     #:provides '(natural-scrolling)
     #:command `(,#$(file-append xinput "/bin/xinput") "set-prop" "TPPS/2 IBM TrackPoint" "libinput Natural Scrolling Enabled" "1")))

(define (make-display-services display)
  (map (cut <> display)
       (list pipewire-service
             pipewire-pulse-service
             pipewire-media-session-service
             i3-service
             pulseaudio-service
             dbus
             emacsclient-service
             tmux-service
             quasselclient-service
             telegram-service
             dunst-service
             pavucontrol-service
             xkeylogger-service
             redshift-service
             natural-scrolling-service
             )))

(apply register-services
       (append
     (list
       sway
       emacs-daemon)
     (make-display-services ":0")))

(action 'shepherd 'daemonize)
(start 'dbus:0)
(start 'sway)
(start 'pipewire:0)
;; (start 'pipewire-media-session:0)
(start 'pipewire-pulse:0)
;; (start 'dunst:0)
(start 'pavucontrol:0)
;; (start 'tmux:0)
(start 'emacsd)
;; (start 'emacsclient:0)
(start 'quasselclient:0)
 (start 'telegram:0)
;; (start 'xkeylogger:0)
;; (start 'redshift:0)
;; (start 'natural-scrolling:0)
)))

(define prompt-config
  (plain-file "prompt.sh" (read-file-as-string "prompt.sh")))

(define notifymuch-config
  (plain-file "notifymuch.cfg" "[notifymuch]
query = is:unread
recency_interval_hours = 0
hidden_tags = inbox unread attachment replied sent encrypted signed"))

(define gtk3-settings
  (plain-file "settings.ini" "[Settings]
gtk-application-prefer-dark-theme = true"))

(define inputrc
  (plain-file "inputrc" (string-append "set editing-mode vi
set keymap vi-command
\"k\": nop
\"j\": nop
" (keys up) ": previous-history
" (keys down) ": next-history")))

(define battery-script
  (computed-file "battery.sh"
    #~(let ((script #$(apply mixed-text-file "script" 
              (list "#!/usr/bin/env sh
CHARGE=$(printf '%.0f\\n' $(" (file-append hwinfo/amd "/bin/hwinfo") " battery capacity))

LEVEL=$(((CHARGE-1)/20))
ICON=\"f$(( 244 - LEVEL ))\"
echo -en \" \\u${ICON} ${CHARGE}%\"

if [ 'Charging' = $(" (file-append hwinfo/amd "/bin/hwinfo") " battery 0 status) ] 
then 
    echo -e ' (Charging)'
fi

echo ''

[[ \"${CHARGE}\" -lt '25' ]] && exit 33

exit 0"))))
      (use-modules (guix build utils))
          (copy-file script #$output)
          (chmod #$output #o755))
    #:options
    '(#:local-build? #t
      #:modules ((guix build utils)))))

(define mako-config
  (plain-file  "config"
               "height=900
width=1920
margin=0
padding=0"))

(define i3blocks-config
  (computed-file 
   "config"
   #~(with-output-to-file
         #$output
       (lambda _
         (set-port-encoding! (current-output-port) "UTF-8") ;; shitty hack for unicode to work
         (format #t "~a"
                 (string-append
                  #$@(list "separator=false
separator_block_width=5
[wireless]
label=  
command=if ! " (file-append ip_addr "/bin/ip_addr") " wlp3s0; then echo \"​\"; exit 33; fi
interval=10

[wired]
label=  
command=if ! " (file-append ip_addr "/bin/ip_addr") " enp0s25; then echo \"​\"; exit 33; fi
interval=10

[battery]
command=" battery-script " 
interval=10

[time]
label=  
command=echo \"$(date +'%a %d %b') <b>$(date +'%T')</b> \"
markup=pango
interval=1
"))))))) ;; this newline is necessary for i3block to get the last config item


(define (rasi-bool->string bool)
  (if bool
      "true"
      "false"))

(define (rasi-symbol->string symbol)
  (let*
      ((str (symbol->string symbol)))
    (if (string=? (string-take-right str 1) "?") (string-drop-right str 1) str)))

(define (rasi-config->string config)
  (match config
         ((config-name config)
          (let*
              ((config-value
                (match config
                       ((? string?) (format #f "\"~a\"" config))
                       ((? exact-integer?) (format #f "~a" config))
                       ((? boolean?) (rasi-bool->string config))
                       ((? list?) (format #f "\"~a\"" (string-join config ","))))))
            (format #f "~a: ~a;\n" (rasi-symbol->string config-name) config-value)))))

(define (rasi-section->string section)
  (match section
         ((section-name configs)
          (apply string-append `(,(symbol->string section-name) " {\n"
                                 ,@(map rasi-config->string configs)
                                 "}\n")))))

(define (rasi->string config)
  (apply string-append (map rasi-section->string config)))

(define rofi-config
  (plain-file
   "rofi-config.rasi"
   (rasi->string
    `((configuration
       ((font "Hack 12")
        (padding 400)
        (fixed-num-lines? #f)
        (show-icons? #t)
        (eh 2)
        (parse-hosts? #f)
        (parse-known-hosts? #t)
        (hide-scrollbar? #t)
        (fullscreen? #t)
        (fake-transparency? #t)
        (color-normal ("#0000"     ,(theme* fg)      "#0000" "#0000" ,(theme* green)))
        (color-urgent ("#0000"     ,(theme* red)     "#0000" "#0000" ,(theme* green)))
        (color-active ("#0000"     ,(theme* magenta) "#0000" "#0000" ,(theme* green)))
        (color-window ("#cc2f343f" "#0000"         "#0000"))))))))

(define bashrc (list "export SHELL

if [[ $- != *i* ]]; then
      # Non interative shell. For SSH session, load /etc/profile to get
      # PATH and other variables set up.
        [[ -n \"$SSH_CLIENT\" ]] && source /etc/profile
      return
fi

source /etc/bashrc
source " prompt-config "
set -o vi

source " (file-append go-github-com-junegunn-fzf "/src/github.com/junegunn/fzf/shell/key-bindings.bash") "

source /run/current-system/profile/etc/profile.d/nix.sh

export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
 --color=fg:"     (theme* fg)     ",bg:"      (theme* bg)      ",hl:"      (theme* yellow) "
 --color=fg+:"    (theme* fg)     ",bg+:"     (theme* bg1)     ",hl+:"     (theme* red)    "
 --color=info:"   (theme* green)  ",prompt:"  (theme* magenta) ",pointer:" (theme* red)    "
 --color=marker:" (theme* blue)   ",spinner:" (theme* magenta) ",header:"  (theme* orange) "'

export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
export PROMPT_COMMAND=\"history -a;history -n;__prompt\"
HISTSIZE=
HISTFILESIZE=

export XAUTHORITY=" xauthority-file "

export EDITOR='TERM=xterm-24bits " (file-append emacs "/bin/emacsclient") " -c -t'
export VISUAL=$EDITOR

export PATH=\"$PATH:/data/projects/dias/dart-sdk/flutter/bin\"
export LM_LICENSE_FILE=/data/projects/fpga/diamond/license.dat


alias pw-jack=" (file-append pipewire-0.3 "/bin/pw-jack") "
alias ls='" (file-append exa "/bin/exa") " --color=auto'
alias l='ls -la'

alias grep='grep --color=auto'

alias nvim=$EDITOR
alias vim=$EDITOR
alias vi=$EDITOR

[ -z \"$TMUX\" ] && (grep -e '/dev/tty[1-9]' <(tty) > /dev/null || " (file-append tmux "/bin/tmux") " new-session -t robin)"))

(define bash-profile-additionals (list "if [ -f ~/.home-profile/etc/profile ]; then
	GUIX_PROFILE=~/.home-profile ; . ~/.home-profile/etc/profile
	export MANPATH=~/.home-profile/share/man:$MANPATH
	export INFOPATH=~/.home-profile/share/info:$INFOPATH
	export XDG_DATA_DIRS=~/.home-profile/share:$XDG_DATA_DIRS
	export XDG_CONFIG_DIRS=~/.home-profile/etc/xdg:$XDG_CONFIG_DIRS
fi\n"
"[[ -z $(pgrep -U $(id --user) '^shepherd$') ]] && " (file-append shepherd "/bin/shepherd") " -l ~/log/shepherd.log -c " shepherd-config " >> ~/log/shepherd.log 2>&1"))

(home
  (data-directory "/data/robin")
  (configurations
    (list
      (user-home bash-home-type 
		 (bash-configuration 
		   (history "/home/robin/.local/share/bash/history") 
		   (rc bashrc)
		   (profile 
		     (append default-bash-profile 
			     bash-profile-additionals))))
      (user-home package-profile-home-type 
		 (list 
		   go-github-com-junegunn-fzf
		   perl))
      (simple-file-home rofi-config ".config/rofi/config.rasi")
      (simple-file-home mako-config ".config/mako/config")
      (simple-file-home i3blocks-config ".config/i3blocks/config")
      (simple-file-home gtk3-settings ".config/gtk-3.0/settings.ini")
      (simple-file-home inputrc ".inputrc")
      (simple-file-home notifymuch-config ".config/notifymuch/notifymuch.cfg")
      (simple-file-home tmux-config ".tmux.conf")
      (simple-file-home ideavim-config ".ideavimrc")
      (simple-file-home alacritty-config ".config/alacritty/alacritty.yml")
      (simple-file-home (i3-home sway-config) ".config/sway")
      (simple-file-home gitconfig ".gitconfig")
      (simple-file-home emacs-terminfo ".terminfo")
      (symlink-file-home "/data/.mail" ".mail")
      (symlink-file-home "/data/.notmuch-config" ".notmuch-config") ; TODO(robin): move to generated config
      (symlink-file-home "/data/.mbsyncrc" ".mbsyncrc") ; SECRETS, TODO(robin): move to generated config
      (symlink-file-home "/data/.msmtprc" ".msmtprc") ; SECRETS, TODO(robin): move to generated config
      (symlink-file-home "/data/projects/guix_system/dunstrc" ".config/dunst/dunstrc") ; TODO(robin): move to generated config
      (symlink-file-home "/data/robin/log/shepherd.log" "log/shepherd.log")
      (symlink-file-home "/data/robin/.ssh/id_ed25519.pub" ".ssh/id_ed25519.pub")
      (symlink-file-home "/data/robin/.ssh/id_ed25519" ".ssh/id_ed25519")
      (symlink-file-home "/data/.ssh/known_hosts" ".ssh/known_hosts")
      ; (symlink-file-home "/data/robin/.config/pavucontrol.ini" ".config/pavucontrol.ini") ; TODO(robin): figure out why this is not working
      (symlink-file-home "/data/.config/quassel-irc.org" ".config/quassel-irc.org") ; SECRETS, TODO(robin): figure out what to do about quassel config, it is quite shitty (and semi binary?)
      (symlink-file-home "/data/.config/horizon" ".config/horizon") ; TODO(robin): maybe generate the color scheme from here?
      (symlink-file-home "/data/robin/.config/kicad" ".config/kicad") ; TODO(robin) what
      (symlink-file-home "/data/robin/.purple" ".purple") ; SECRETS, TODO(robin): figure out what to do about    this one
      (symlink-file-home "/data/projects/doom-emacs" ".emacs.d") ; TODO(robin): figure out this one
      (symlink-file-home "/data/projects/guix_system/.doom.d" ".doom.d") ; TODO(robin): figure out this one
      (symlink-file-home "/data/projects/guix_system/.clang-format" ".clang-format") ; TODO(robin): figure out this one
      (symlink-file-home "/data/robin/.texlive2018" ".texlive2018")
      (symlink-file-home "/data/robin/.texlive2019" ".texlive2019")
      (symlink-file-home "/data/robin/.Xilinx" ".Xilinx")
      (symlink-file-home "/data/.config/guix-gaming-channels" ".config/guix-gaming-channels")  ; fuck it
      (symlink-file-home "/data/.config/configstore" ".config/configstore")  ; fuck it
      (symlink-file-home "/data/.config/skypeforlinux" ".config/skypeforlinux")  ; fuck it
      (symlink-file-home "/data/.config/unity3d" ".config/unity3d")  ; fuck it
      (symlink-file-home "/data/.config/dconf" ".config/dconf")  ; fuck it
      (symlink-file-home "/data/.config/Signal" ".config/Signal")  ; fuck it
      (symlink-file-home "/data/.config/astroid" ".config/astroid")  ; fuck it
      (symlink-file-home "/data/.config/pmbootstrap.cfg" ".config/pmbootstrap.cfg")  ; fuck it
      (symlink-file-home "/data/.config/VSCodium" ".config/VSCodium")  ; fuck it
      (symlink-file-home "/data/.config/Code" ".config/Code")  ; fuck it
      (symlink-file-home "/data/.config/pipewire-media-session" ".config/pipewire-media-session")  ; fuck it
      (symlink-file-home "/data/.fastlane" ".fastlane")  ; fuck it
      (symlink-file-home "/data/.config/Google" ".config/Google")  ; fuck it
      (symlink-file-home "/data/.frida" ".frida")  ; fuck it
      (symlink-file-home "/data/.npmrc" ".npmrc")  ; fuck it
      (symlink-file-home "/data/.yarnrc" ".yarnrc")  ; fuck it
      (symlink-file-home "/data/.yarn" ".yarn")  ; fuck it
      (symlink-file-home "/data/.vscode" ".vscode")  ; fuck it
      (symlink-file-home "/data/.vscode-oss" ".vscode-oss")  ; fuck it
      (symlink-file-home "/data/.cargo" ".cargo")  ; fuck it
      (symlink-file-home "/data/.fonts" ".fonts")  ; fuck it
      (symlink-file-home "/data/.npm" ".npm")      ; fuck it
      (symlink-file-home "/data/.AndroidStudio3.6" ".AndroidStudio3.6")      ; fuck it
      (symlink-file-home "/data/.android" ".android")      ; fuck it
      (symlink-file-home "/data/.gradle" ".gradle")      ; fuck it
      (symlink-file-home "/data/.ghidra" ".ghidra")      ; fuck it
      (symlink-file-home "/data/.FreeCAD/" ".FreeCAD")      ; fuck it
      (symlink-file-home "/data/.factorio" ".factorio")      ; fuck it
      (symlink-file-home "/data/.hashcat" ".hashcat")      ; fuck it
      (symlink-file-home "/data/.m2" ".m2")      ; fuck it
      (symlink-file-home "/data/.aws" ".aws")      ; fuck it
      (symlink-file-home "/data/.elm" ".elm")      ; fuck it
      (symlink-file-home "/data/.xournalpp" ".xournalpp")      ; fuck it
      (symlink-file-home "/data/.jupyter" ".jupyter")      ; fuck it
      (symlink-file-home "/data/.xpra" ".xpra")      ; fuck it
      (symlink-file-home "/data/.tabula" ".tabula")      ; fuck it
      (symlink-file-home "/data/.amplify" ".amplify")      ; fuck it
      (symlink-file-home "/data/.nix-defexpr" ".nix-defexpr")      ; fuck it
      (symlink-file-home "/data/.nix-channels" ".nix-channels")      ; fuck it
      (symlink-file-home "/data/.nixpkgs" ".nixpkgs")      ; fuck it
      (symlink-file-home "/nix/var/nix/profiles/per-user/robin/profile" ".nix-profile")      ; fuck it
      (symlink-file-home "/data/.flutter_tool_state" ".flutter_tool_state")      ; fuck it
      (symlink-file-home "/data/.flutter" ".flutter")      ; fuck it
      (symlink-file-home "/data/.config/flutter" ".config/flutter")      ; fuck it
      (symlink-file-home "/data/.flutter_settings" ".flutter_settings")      ; fuck it
      (symlink-file-home "/data/.pub-cache" ".pub-cache")      ; fuck it
      (symlink-file-home "/data/.vpython-root" ".vpython-root")      ; fuck it
      (symlink-file-home "/data/.dartServer" ".dartServer")      ; fuck it
      (symlink-file-home "/data/.gnupg" ".gnupg")      ; fuck it
      (symlink-file-home "/data/.vpython_cipd_cache" ".vpython_cipd_cache")      ; fuck it
      (symlink-file-home "/data/texmf" "texmf") ; TODO(robin): rework this to static files in the store? (or build packages for the few missing things)
      (symlink-file-home "/data/robin/.config/chromium" ".config/chromium"))))

