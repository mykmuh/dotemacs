(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#FAFAFA" "#FF1744" "#66BB6A" "#F57F17" "#42A5F5" "#7E57C2" "#0097A7" "#546E7A"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(beacon-color "#ff9da4")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("5057614f7e14de98bbc02200e2fe827ad897696bfd222d1bcab42ad8ff313e20" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(dired-filter-saved-filters
   (quote
    (("clean_view"
      (not
       (regexp . ".*~$"))
      (not
       (regexp . "^#.*"))
      (#("no underscore" 0 1
         (idx 0))
       (not
        (regexp . "^_.*"))))
     (#("no underscore" 0 1
        (idx 0))
      (not
       (regexp . "^_.*"))))))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil t)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(markdown-asymmetric-header t)
 '(markdown-command "/usr/local/bin/markdown" t)
 '(markdown-live-preview-delete-export (quote delete-on-export))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-show-context-detail
   (quote
    ((occur-tree . minimal)
     (agenda . local)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . minimal))))
 '(package-selected-packages
   (quote
    (hc-zenburn-theme labburn-theme yasnippet-snippets yasnippet solarized-theme github-theme terraform=mode company-terraform terraform-mode hcl-mode color-theme-modern color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-solarized paredit vimish-fold flatui-theme github-modern-theme minimal-theme undo-tree json-mode yaml-mode worf markdown-mode lua-mode avy zenburn-theme htmlize highlight-indent-guides origami makey discover indent-tools dired-ranger ranger el-get command-log-mode base16-twilight base16-twilight-theme twilight twilight-anti-bright-theme twilight-bright-theme twilight-theme tango-plus-theme tango-plus apropospriate-theme moe-theme base16-theme gruvbox-theme chruby seeing-is-believing ruby-electric dired-filter dired-narrow rainbow-delimiters robe company company-shell wgrep wgrep-ack wgrep-ag ztree ivy-hydra org counselq counsel-osx-app counsel-projectile highlight-indentation company-restclient restclient test-kitchen ag json-reformat smartscan which-key smooth-scrolling smooth-scroll peep-dired org-projectile projectile with-editor session magit-popup hydra helm git-gutter+ git-commit fringe-helper epl diminish dash bind-key avys async ace-link window-number whole-line-or-region use-package swiper simpleclip rebox2 powershell powerline pkg-info pdf-tools neotree magit git-gutter-fringe+ expand-region exec-path-from-shell dired-toggle-sudo dired+ deft cl-lib-highlight boxquote ace-window)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
