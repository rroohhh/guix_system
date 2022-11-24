(define-module (home programs)
  #:use-module (rnrs lists)
  #:use-module (ice-9 match)
  #:use-module (home config)
  #:use-module (misc util)
  #:use-module (vup home i3)
  #:use-module (vup hwinfo)
  #:use-module (vup xkeylogger)
  #:use-module (vup ip_addr)
  #:use-module (vup tmp)
  #:use-module (vup rust-apps)
  #:use-module (vup misc)
  #:use-module (vup atuin)
  #:use-module (guix gexp)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages irc))

(define-public emacs-terminfo
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


(define-public alacritty-config
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

(define-public guix-config
  (scheme-file
   "channels.scm"
   #~(list (channel
             (name 'guix)
             (url "https://git.savannah.gnu.org/git/guix.git")
             (branch "master"))
           (channel
             (name 'vup)
             (url "https://github.com/rroohhh/guix_packages.git"))
           (channel
             (name 'guix-gaming-games)
             (url "https://gitlab.com/guix-gaming-channels/games.git")
             (introduction
              (make-channel-introduction
               "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
               (openpgp-fingerprint
                "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
           (channel
             (name 'nonguix)
             (url "https://gitlab.com/nonguix/nonguix")
             (introduction
              (make-channel-introduction
               "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
               (openpgp-fingerprint
                "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))))


(define-public ideavim-config
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


(define-public tmux-config
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

(define-public gitconfig
  (plain-file "gitconfig" "[user]
        email = robin.ole.heinemann@gmail.com
        name = Robin Ole Heinemann
[pull]
ff = only

[init]
defaultBranch = main"))

(define-public wofi-config
  (plain-file "wofi" "key_down=N
key_up=T"))

(define-public window-switcher
  (mixed-text-file "switch.sh" "#/usr/bin/env bash
alias swaymsg=" (file-append sway "/bin/swaymsg") "
" (read-file-as-string "home/window_switch.sh")))

(define-public sway-config
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
    }

seat * xcursor_theme Adwaita"))
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
            ((,mod "Shift" "w") ("exec sh " ,window-switcher))
            (("XF86MonBrightnessUp") "exec sh -c 'hwinfo backlight brightness $(($(hwinfo backlight brightness) + 20))'")
            (("XF86MonBrightnessDown") "exec sh -c 'hwinfo backlight brightness $(($(hwinfo backlight brightness) - 20))'")
            ,@(fold-right append '()
                          (map (lambda (x)
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

(define-public asoundrc
  (mixed-text-file "asoundrc" "pcm_type.pipewire {
lib \"" pipewire-0.3 "/lib/alsa-lib/libasound_module_pcm_pipewire.so\"
}
" (read-file-as-string "home/asoundrc")))

(define-public prompt-config
  (plain-file "prompt.sh" (read-file-as-string "home/prompt.sh")))

(define-public notifymuch-config
  (plain-file "notifymuch.cfg" "[notifymuch]
query = is:unread
recency_interval_hours = 0
hidden_tags = inbox unread attachment replied sent encrypted signed"))

(define-public bazel-settings
  (plain-file "bazelrc" "build --sandbox_block_path=/home/robin/.guix-home
"))

(define-public gtk3-settings
  (plain-file "settings.ini" "[Settings]
gtk-application-prefer-dark-theme = true"))

(define-public inputrc
  (plain-file "inputrc" (string-append "set editing-mode vi
set keymap vi-command
\"k\": nop
\"j\": nop
" (keys up) ": previous-history
" (keys down) ": next-history")))

(define-public battery-script
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

(define-public mako-config
  (plain-file  "config"
               "height=900
width=1920
margin=0
padding=0"))

(define-public i3blocks-config
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
command=echo \"$(date +'%a %d %b') <b>$(TZ='Europe/Berlin' date +'%T')</b> \"
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

(define-public rofi-config
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

(define-public shepherd-config
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
            (setenv "XDG_SESSION_TYPE" "wayland")
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

       (define (wireplumber-service display)
         (make-simple-forkexec-display-service display
           #:docstring "Wireplumber pipewire session manager"
           #:requires '(pipewire)
           #:provides '(wireplumber)
           #:command `(,#$(file-append wireplumber "/bin/wireplumber"))))

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
                                                                               "XDG_SESSION_TYPE=wayland"
                                                                               (default-environment-variables)))
           #:stop
           (make-kill-destructor)))

       (define ra-multiplex-server
         (make-service
           #:docstring "ra-multiplex-server"
           #:provides '(ra-multiplex)
           #:start (make-forkexec-constructor (list #$(file-append ra-multiplex "/bin/ra-multiplex-server")))
           #:stop (make-kill-destructor)))

       ;; (define (i3-service display)
       ;;   (make-simple-forkexec-display-service display
       ;;     #:docstring "i3"
       ;;     #:provides '(i3 wm)
       ;;     #:command `(,#$(file-append i3-gaps "/bin/i3"))))

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
            #:command `(,#$(file-append telegram-desktop-fixed "/bin/telegram-desktop"))))

       (define (pavucontrol-service display)
          (make-simple-forkexec-display-service display
            #:docstring "Pavucontrol"
            #:provides '(pavucontrol)
            #:command `(,#$(file-append pavucontrol "/bin/pavucontrol"))))

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
                    wireplumber-service
                    ;; i3-service
                    pulseaudio-service
                    dbus
                    emacsclient-service
                    tmux-service
                    quasselclient-service
                    telegram-service
                    pavucontrol-service
                    xkeylogger-service
                    redshift-service
                    natural-scrolling-service)))


       (apply register-services
              (append
               (list
                sway
                emacs-daemon
                ra-multiplex-server)
               (make-display-services ":0")))

       (action 'shepherd 'daemonize)
       (start 'dbus:0)
       (start 'sway)
       (start 'ra-multiplex)
       (start 'pipewire:0)
       (start 'pipewire-pulse:0)
       (start 'wireplumber:0)
;; (start 'dunst:0)
       (start 'pavucontrol:0)
;; (start 'tmux:0)
       (start 'emacsd)
;; (start 'emacsclient:0)
       (start 'quasselclient:0)
       (start 'telegram:0))))
;; (start 'xkeylogger:0)
;; (start 'redshift:0)
;; (start 'natural-scrolling:0)

(define-public user-shepherd-setup
  (mixed-text-file "user-shepherd-setup"
   "[[ -z $(pgrep -U $(id --user) '^shepherd$') ]] && " (file-append shepherd "/bin/shepherd") " -l ~/log/shepherd.log -c " shepherd-config " >> ~/log/shepherd.log 2>&1"))

;; (define-public cargo-sccache-setup
;;   (mixed-text-file "cargo-sccache-setup"
;;                    "[build]
;; # rustc-wrapper = \"" (file-append rust-sccache-0.2 "/bin/sccache")"\"
;; rustflags = ['-C', 'link-arg=-fuse-ld=lld']
;; "))

(define-public cargo-sccache-setup
  (mixed-text-file "cargo-sccache-setup"
                   "[build]
rustflags = ['-C', 'link-arg=-fuse-ld=lld']
"))

(define-public atuin-config
  (mixed-text-file "atuin-config"
                   "style = 'compact'
update_check = false
search_mode = 'fuzzy'
"))

(define-public ra-multiplex-config
  (mixed-text-file "ra-multiplex-config" "instance_timeout = false\n"))


(define-public basic-program-configs
  (list
   `(".config/rofi/config.rasi" ,rofi-config)
   `(".config/guix/channels.scm" ,guix-config)
   `(".config/mako/config" ,mako-config)
   `(".config/wofi/config" ,wofi-config)
   `(".config/i3blocks/config" ,i3blocks-config)
   `(".config/gtk-3.0/settings.ini" ,gtk3-settings)
   `(".config/ra-multiplex/config.toml" ,ra-multiplex-config)
   `(".config/alacritty/alacritty.yml" ,alacritty-config)
   `(".config/notifymuch/notifymuch.cfg" ,notifymuch-config)
   `(".config/atuin/config.toml" ,atuin-config)
   `(".config/sway" ,(i3-home sway-config))
   `(".cargo/config.toml" ,cargo-sccache-setup)
   `(".inputrc" ,inputrc)
   ;`(".bazelrc" ,bazel-settings) doesnt really work
   `(".asoundrc" ,asoundrc)
   `(".tmux.conf" ,tmux-config)
   `(".ideavimrc" ,ideavim-config)
   `(".gitconfig" ,gitconfig)
   `(".terminfo" ,emacs-terminfo)))

(define-public bashrc (mixed-text-file "bashrc" "export SHELL

if [[ $- != *i* ]]; then
      # Non interative shell. For SSH session, load /etc/profile to get
      # PATH and other variables set up.
        [[ -n \"$SSH_CLIENT\" ]] && source /etc/profile
      return
fi

source /etc/bashrc
export PROMPT_COMMAND=
source " prompt-config "
set -o vi

source " (file-append bash-preexec "/bash-preexec.sh") "
export ATUIN_NOBIND='true'
eval \"$(" (file-append atuin "/bin/atuin") " init bash)\"
bind -x '\"\\C-r\": __atuin_history'

source /run/current-system/profile/etc/profile.d/nix.sh

export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
 --color=fg:"     (theme* fg)     ",bg:"      (theme* bg)      ",hl:"      (theme* yellow) "
 --color=fg+:"    (theme* fg)     ",bg+:"     (theme* bg1)     ",hl+:"     (theme* red)    "
 --color=info:"   (theme* green)  ",prompt:"  (theme* magenta) ",pointer:" (theme* red)    "
 --color=marker:" (theme* blue)   ",spinner:" (theme* magenta) ",header:"  (theme* orange) "'

export PROMPT_COMMAND=\"$PROMPT_COMMAND;__prompt\"

eval \"$(" (file-append zoxide "/bin/zoxide") " init --cmd cd bash)\"

export XAUTHORITY=" xauthority-file "

export EDITOR='TERM=xterm-24bits " (file-append emacs "/bin/emacsclient") " -c -t'
export VISUAL=$EDITOR

export PATH=\"$PATH:/data/projects/dias/dart-sdk/flutter/bin\"
export LM_LICENSE_FILE=/data/projects/fpga/diamond/license.dat

export LIBCLANG_PATH=~/.guix-home/profile/lib
export SHADERC_LIB_DIR=~/.guix-home/profile/lib
export CC=gcc

alias pw-jack=" (file-append pipewire-0.3 "/bin/pw-jack") "
alias ls='" (file-append exa "/bin/exa") " --color=auto'
alias l='ls -la'

alias grep='grep --color=auto'

alias nvim=$EDITOR
alias vim=$EDITOR
alias vi=$EDITOR

[ -z \"$TMUX\" ] && (grep -e '/dev/tty[1-9]' <(tty) > /dev/null || " (file-append tmux "/bin/tmux") " new-session -t robin)"))
