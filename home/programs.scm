(define-module (home programs)
  #:use-module (rnrs lists)
  #:use-module (ice-9 match)
  #:use-module (home config)
  #:use-module (home hy3)
  #:use-module (config network)
  #:use-module (misc util)
  #:use-module (vup home i3)
  #:use-module (vup hwinfo)
  #:use-module (vup ip_addr)
  #:use-module (vup rust-apps)
  #:use-module (vup misc)
  #:use-module (vup atuin)
  ;; #:use-module (rosenthal packages wm)
  #:use-module (home lights)
  #:use-module (guix gexp)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages telegram)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages guile)
  #:use-module (guix packages)
  #:use-module (guix download))

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
  (plain-file "alacritty.toml" (string-append "general.live_config_reload = true

[env]
TERM = \"xterm-256color\"
WINIT_HIDPI_FACTOR = \"1.0\"
WINIT_X11_SCALE_FACTOR = \"1.0\"

[window]
decorations = \"full\"
dynamic_padding = false

[window.dimensions]
columns = 0
lines = 0

[window.padding]
x = 2
y = 2

[scrolling]
history = 10000
multiplier = 3

[font]
size = 10.0

[font.bold]
family = \"" monospace-font "\"
style = \"Bold\"

[font.glyph_offset]
x = 0
y = 0

[font.italic]
family = \"" monospace-font "\"
style = \"Italic\"

[font.normal]
family = \"" monospace-font "\"
style = \"Regular\"

[font.offset]
x = 0
y = 0

[bell]
animation = \"EaseOutExpo\"
color = \"0xffffff\"
duration = 0

[colors]
draw_bold_text_with_bright_colors = false

[colors.cursor]
cursor = " (theme red) "
text = " (theme fg) "

[colors.primary]
background = " (theme bg) "
foreground = " (theme fg) "

[colors.normal]
black = " (theme black) "
blue = " (theme blue) "
cyan = " (theme cyan) "
green = " (theme green) "
magenta = " (theme magenta) "
red = " (theme red) "
white = " (theme white) "
yellow = " (theme yellow) "

[colors.bright]
black = " (theme black) "
blue = " (theme blue) "
cyan = " (theme cyan) "
green = " (theme green) "
magenta = " (theme magenta) "
red = " (theme red) "
white = " (theme white) "
yellow = " (theme yellow) "

[cursor]
style = \"Block\"
unfocused_hollow = true

[debug]
persistent_logging = false
render_timer = false

[mouse]
hide_when_typing = true

[[mouse.bindings]]
action = \"PasteSelection\"
mouse = \"Middle\"

[selection]
save_to_clipboard = false
semantic_escape_chars = \",│`|:\\\"' ()[]{}<>\"")))


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

bind-key    -T copy-mode-vi Down                    send-keys -X scroll-down
bind-key    -T copy-mode-vi Up                    send-keys -X scroll-up

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

(define-public gitconfig-visions
  (plain-file "gitconfig-visions" "[user]
        email = robin.heinemann@kip.uni-heidelberg.de
        name = Robin Heinemann
"))

(define-public gitconfig
  (mixed-text-file "gitconfig" "[user]
        email = robin.ole.heinemann@gmail.com
        name = Robin Ole Heinemann
[pull]
ff = only

[init]
defaultBranch = main

[includeIf \"gitdir:/data/study/bachelor/\"]
    path = " gitconfig-visions "

[url \"ssh://git@git.froheiyd.de:2222/\"]
    insteadOf = https://git.froheiyd.de/

[sendemail]
    sendmailcmd = " (file-append msmtp "/bin/msmtp") "
    smtpserveroption = -a
    smtpserveroption = gmail
    confirm = always
"))

;; [url \"git@github.com:\"]
;; insteadOf = https://github.com/


(define-public wofi-config
  (plain-file "wofi" "key_down=N
key_up=T"))

(define-public window-switcher
  (mixed-text-file "switch.sh" "#/usr/bin/env bash
alias swaymsg=" (file-append sway "/bin/swaymsg") "
" (read-file-as-string "home/window_switch.sh")))


(define-public hyprland-config
  (apply mixed-text-file `("hyprland.conf" "
monitor=desc:AUO 0x573D,preferred,1920x1440,1.0
monitor=desc:FUS A17-3A YE2P263316,preferred,4480x0,1.0,transform,3
monitor=desc:LEN T27hv-20 V306V9KL,preferred,1920x0,1.0
monitor=desc:ENC EV2313W 42302061,preferred,0x0,1.0
monitor=desc:Dell Inc. DELL U2711 G606T11C08AL,modeline 241.5 2560 2608 2640 2720 1440 1443 1448 1481 +HSync -VSync,1920x0,1.0
monitor=desc:BNQ BenQ PD2700U ETH7N01993SL0,preferred,1920x0,1.5
monitor=,preferred,auto,1.0

$terminal = " ,(file-append alacritty "/bin/alacritty") "
$menu = " ,(file-append wofi "/bin/wofi") " --show run

env = XCURSOR_SIZE,32

plugin = " ,(file-append hyprland-hy3 "/lib/libhy3.so") "

plugin {
  hy3 {
    no_gaps_when_only = 2
    autotile {
      enable = true
    }
    tabs {
      padding = 3
      rounding = 3
      height = 20
      render_text = true
      text_height = 11
      col.active = rgb(" ,(string-trim (theme* blue) #\#) ")
      col.text.inactive = rgb(" ,(string-trim (theme* fg) #\#) ")
    }
  }
}

input {
    kb_layout = de
    kb_variant = vup
    kb_model =
    kb_options =
    kb_rules =

    follow_mouse = 1

    natural_scroll = yes

    touchpad {
        tap-to-click = no
        natural_scroll = yes
    }

    sensitivity = 0.2
}

device {
    name = tpps/2-elan-trackpoint
    sensitivity = 0.5
}

general {
    gaps_in = 3
    gaps_out = 5
    gaps_workspaces = 50
    border_size = 1
    col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
    col.inactive_border = rgba(595959aa)

    layout = hy3

    allow_tearing = false
}

decoration {
    rounding = 7

    blur {
        enabled = false
        size = 3
        passes = 1
    }
}

animations {
    enabled = yes

    bezier = linear, 0, 0, 1, 1
    bezier = md3_standard, 0.2, 0, 0, 1
    bezier = md3_decel, 0.05, 0.7, 0.1, 1
    bezier = md3_accel, 0.3, 0, 0.8, 0.15
    bezier = overshot, 0.05, 0.9, 0.1, 1.1
    bezier = crazyshot, 0.1, 1.5, 0.76, 0.92
    bezier = hyprnostretch, 0.05, 0.9, 0.1, 1.0
    bezier = fluent_decel, 0.1, 1, 0, 1
    bezier = easeInOutCirc, 0.85, 0, 0.15, 1
    bezier = easeOutCirc, 0, 0.55, 0.45, 1
    bezier = easeOutExpo, 0.16, 1, 0.3, 1

    # animation = borderangle, 1, 50, linear, loop
    animation = windows, 1, 3, md3_decel, popin 60%
    animation = border, 1, 10, default
    animation = fade, 1, 2.5, md3_decel
    animation = workspaces, 1, 7, fluent_decel, slide
    animation = specialWorkspace, 1, 3, md3_decel, slidevert
}

gestures {
    workspace_swipe = on
}

misc {
    force_default_wallpaper = -1
    vfr = true
    vrr = 2
    disable_hyprland_logo = true
    background_color = rgb(" ,(string-trim (theme* bg) #\#) ")
    focus_on_activate = true
    key_press_enables_dpms = true
}
debug {
disable_logs = false
}

$mainMod = SUPER

bind = $mainMod, Return, exec, $terminal
bind = $mainMod, Q, killactive,
bind = $mainMod, M, exit,
bind = $mainMod, Space, togglefloating,
bind = $mainMod, D, exec, $menu
bind = $mainMod, f, fullscreen,
bind = $mainMod, a, hy3:changefocus,raise


bind = $mainMod, w, hy3:changegroup,toggletab
bind = $mainMod, v, hy3:makegroup,opposite,
bind = $mainMod, e, hy3:changegroup,opposite,

bind = $mainMod, p, exec, pkill -USR1 waybar
bind = $mainMod, u, exec, sleep 1; " ,(file-append hyprland-0.46 "/bin/hyprctl") " dispatch dpms off
bind = $mainMod, k, exec, " ,(file-append swaynotificationcenter "/bin/swaync-client") " -t

bind=,XF86MonBrightnessDown,exec," ,(file-append brightnessctl "/bin/brightnessctl") " -e set 3%-
bind=,XF86MonBrightnessUp,exec," ,(file-append brightnessctl "/bin/brightnessctl") " -e set +3%
"
; this is fucking cursed, but it works
,@(apply append
 (map
  (lambda (key)
    (list
     (string-append "bind = $mainMod, " (primitive-eval `(keys* ,key)) ", hy3:movefocus, " (symbol->string key) "\n")
     (string-append "bind = $mainMod SHIFT, " (primitive-eval `(keys* ,key)) ", hy3:movewindow, " (symbol->string key) "\n")))
  '(left right up down)))

,@(apply append (map
  (lambda (num)
    (let*
        ((ws-num (+ num 1))
         (ws-key (modulo ws-num 10)))
        (list
          (format #f "bind = $mainMod, ~a, workspace, ~a\n" ws-key ws-num)
          (format #f "bind = $mainMod SHIFT, ~a, hy3:movetoworkspace, ~a\n" ws-key ws-num))))
  (iota 10)))

"
bindm = $mainMod, mouse:272, movewindow
bindm = $mainMod, mouse:273, resizewindow
")))

(define-public waybar-config
  (mixed-text-file
   "config.jsonc"
   (let*
       ((host-name (gethostname))
        (mel-address (address-of "mel" host-name)))
     (with-extensions (list guile-json-4)
       #~(begin
           (use-modules (json))
           (scm->json-string
            `(("wireplumber"
               ("format-icons" . #("" "" ""))
               ("on-click"
                .
                ,(string-append #$(file-append wireplumber "/bin/wpctl") " set-mute @DEFAULT_AUDIO_SINK@ toggle"))
               ("format-muted" . "")
               ("format" . "{volume}% {icon}"))
              ("network"
               ("format-alt" . "{ifname}: {ipaddr}/{cidr}")
               ("format-disconnected" . "Disconnected ⚠")
               ("format-linked" . "{ifname} (No IP) ")
               ("tooltip-format"
                .
                "{ifname} via {gwaddr} ")
               ("format-ethernet" . "{ipaddr}/{cidr} ")
               ("format-wifi" . " {essid}|{ipaddr}")
               ("interface" . "wlp*"))
              ("battery"
               ("format-icons"
                .
                #("", "", "", "", ""))
               ("format-alt" . "{time} {icon}")
               ("format-plugged" . "{capacity}% ")
               ("format-charging" . "{capacity}% ")
               ("format-full" . "{capacity}% {icon}")
               ("format" . "{capacity}% {icon}")
               ("states" ("critical" . 15) ("warning" . 30)))
              ("clock"
               ("interval" . 1)
               ("format" . "{:%a %d %b %T}")
               ("format-alt" . "{:%Y-%m-%d}")
               ("tooltip-format"
                .
                "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>"))
              ("tray" ("spacing" . 10) ("icon-size" . 21))
              ("sway/workspaces" ("format" . "{name}"))
              ;; TODO(robin): this chews a lot of traffic on wg0
              ;; maybe only buttons, no listening?
              ;; ("custom/lights"
              ;;  ("on-scroll-right"
              ;;   .
              ;;   ,(string-append #$(file-append mosquitto "/bin/mosquitto_pub") " -q 2 -h " #$mel-address " -t 'zigbee2mqtt/robin/set' -m '{\"color_temp_step\":20}'"))
              ;;  ("on-scroll-left"
              ;;   .
              ;;   ,(string-append #$(file-append mosquitto "/bin/mosquitto_pub") " -q 2 -h " #$mel-address " -t 'zigbee2mqtt/robin/set' -m '{\"color_temp_step\":-20}'"))
              ;;  ("on-scroll-up"
              ;;   .
              ;;   ,(string-append #$(file-append mosquitto "/bin/mosquitto_pub") " -q 2 -h " #$mel-address " -t 'zigbee2mqtt/robin/set' -m '{\"brightness_step\":20}'"))
              ;;  ("on-scroll-down"
              ;;   .
              ;;   ,(string-append #$(file-append mosquitto "/bin/mosquitto_pub") " -q 2 -h " #$mel-address " -t 'zigbee2mqtt/robin/set' -m '{\"brightness_step\":-20}'"))
              ;;  ("restart-interval" . 1)
              ;;  ("return-type" . "json")
              ;;  ("exec" . ,(string-append #$(file-append lights "/bin/lights") " " #$mel-address)))
              ("modules-right"
               .
               #("wireplumber"
                 ; "custom/lights"
                 "network"
                 "battery"
                 "clock"
                 "tray"))
              ("modules-center" . #("sway/window" "hyprland/window"))
              ("modules-left" . #("sway/workspaces" "hyprland/workspaces"))
              ("spacing" . 0)
              ("height" . 18)))
           )))))

(define-public waybar-style
  (plain-file "style.css" (read-file-as-string "home/waybar-style.css")))


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
            "focus_on_window_activation focus"
            "for_window [ title=\"^org.anbox.*\" ] border none floating enable"
            "default_border pixel 3"
            "input * {
  natural_scroll enabled
  xkb_layout de
  xkb_variant vup
    }

seat * xcursor_theme Adwaita"))
         (bar (i3-bar-configuration
               (bar-command (file-append waybar "/bin/waybar"))))
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
lib \"" pipewire "/lib/alsa-lib/libasound_module_pcm_pipewire.so\"
}
" (read-file-as-string "home/asoundrc")))


(define-public pipewire-conf
  (plain-file "pipewire.conf" "
context.properties = {
    default.clock.rate        = 44100
}

"))

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
search_mode = 'fulltext'
"))

(define-public ra-multiplex-config
  (mixed-text-file "ra-multiplex-config" "instance_timeout = false\n"))

(define-public mpv-config ; ~/.config/mpv/mpv.conf.
  (mixed-text-file "mpv.conf" "hwdec=vaapi
ytdl-format=bestvideo[height<=1080]+bestaudio/best"))


(define-public basic-program-configs
  (list
   `(".config/rofi/config.rasi" ,rofi-config)
   `(".config/mako/config" ,mako-config)
   `(".config/wofi/config" ,wofi-config)
   `(".config/gtk-3.0/settings.ini" ,gtk3-settings)
   `(".config/ra-multiplex/config.toml" ,ra-multiplex-config)
   `(".config/alacritty/alacritty.toml" ,alacritty-config)
   `(".config/notifymuch/notifymuch.cfg" ,notifymuch-config)
   `(".config/atuin/config.toml" ,atuin-config)
   `(".config/sway" ,(i3-home sway-config))
   `(".config/hypr/hyprland.conf" ,hyprland-config)
   `(".config/waybar/config.jsonc" ,waybar-config)
   `(".config/waybar/style.css" ,waybar-style)
   `(".config/mpv/mpv.conf" ,mpv-config)
   `(".config/pipewire/pipewire.conf.d/clockrate.conf" ,pipewire-conf)
   `(".cargo/config.toml" ,cargo-sccache-setup)
   ;; `(".bazelrc" ,bazel-settings) doesnt really work
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

export GOPRIVATE=gitlab.uni-kassel.de/*,git.roheiyd.de/*

export EDITOR='TERM=xterm-24bits " (file-append emacs "/bin/emacsclient") " -c -t'
export VISUAL=$EDITOR

export PATH=\"$PATH:/data/projects/flutter/flutter/bin:/home/robin/go/bin\"
export LM_LICENSE_FILE=/data/projects/fpga/diamond/license.dat

export LIBCLANG_PATH=~/.guix-home/profile/lib
export SHADERC_LIB_DIR=~/.guix-home/profile/lib
export CC=gcc

alias pw-jack=" (file-append pipewire "/bin/pw-jack") "
alias ls='" (file-append exa "/bin/eza") " --color=auto'
alias l='ls -la'

alias grep='grep --color=auto'

alias nvim=$EDITOR
alias vim=$EDITOR
alias vi=$EDITOR
printf \"\\e[?1042l\"

[ -z \"$TMUX\" ] && (grep -e '/dev/tty[1-9]' <(tty) > /dev/null || " (file-append tmux "/bin/tmux") " new-session -t robin)"))
