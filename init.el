;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; super vs shift key
;; - s = super
;; - S = shift

(setenv "PATH"
	(concat (getenv "PATH")
		":/usr/local/bin"))


(add-to-list 'exec-path "/usr/local/texlive/2016/bin/x86_64-darwin")

;; (add-to-list 'custom-theme-load-path "/Users/michael/source/emacs-leuven-theme")
;; (setq leuven-scale-outline-headlines nil)
;; (load-theme 'leuven t)

(require 'url-handlers)
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(add-to-list 'load-path "/Users/michael/.emacs.d/el-get/org-opml/")
;; let's edit opml files
(load-library "org-opml")
(bind-key* "C-c C-r" 'ivy-resume)

;; all in personal lisp and downloaded packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; devonthink
;; (load "org-devonthink")

;; mouse like kitchin
(require 'org-mouse)

;;preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; to copy from browser on mac
(require 'org-protocol)

;; mkm: moving to use-package for everything, so following will become useless
(setq my-package-list '(ace-link ace-window async avy bind-key boxquote cl-lib-highlight dash deft diminish dired+ dired-toggle-sudo epl exec-path-from-shell expand-region fringe-helper git-commit git-gutter+ git-gutter-fringe+ helm lua-mode magit magit-popup markdown-mode neotree pdf-tools pkg-info powerline powershell projectile rebox2 session simpleclip swiper use-package whole-line-or-region window-number with-editor with-editor worf ))

(mapc #'package-install my-package-list)

;; got to kill it
(bind-key "<C-f5>" 'save-buffers-kill-emacs)

;; theme shit
;; note: not using this at present
;; (defadvice load-theme 
;;   (before theme-dont-propagate activate)
;;   (mapcar #'disable-theme custom-enabled-themes))

(defun load-only-theme ()
  "Disable all themes and then load a single theme interactively."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (call-interactively 'load-theme))

(global-set-key (kbd "C-<f12>") 'switch-theme)

(defun switch-theme (theme)
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

;; trying out switch back to color themes
;; (use-package color-theme
;;   :ensure t)
;; (use-package base16-theme
;;   :ensure t)
;; (use-package moe-theme
;;   :ensure t
;;   :init
;;   (progn
;;     (global-hl-line-mode 1)
;;     (custom-set-faces
;;      '(diff-added ((t :foreground "green" :underline nil)))
;;      '(diff-removed ((t :foreground "red" :underline nil)))
;; ;;     '(hl-line ((t :background "white")))
;;      ))
;;   :config
;;   (load-theme 'moe-light t))
;; (moe-light)
;; (moe-theme-set-color 'yellow)

(use-package apropospriate-theme
  :ensure t
  :init
  (custom-set-faces
   '(org-level-1 ((t :height 1.0 )))
   '(org-level-2 ((t :height 1.0 )))
   '(org-level-3 ((t :height 1.0 )))
   )
  :config 
  (load-theme 'apropospriate-light t)
  )

;; (use-package base16-theme
;;   :ensure t
;;   :init
;;   (load-theme 'base16-twilight t))


;; (use-package tango-plus-theme
;;   :ensure t
;;   :init
;;   (custom-set-faces
;;    '(org-level-1 ((t :height 1.0 )))))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :init
;;   (progn
;;     (load-theme 'sanityinc-tomorrow-night :no-confirm)
;;     (setf frame-background-mode 'day)
;;     (global-hl-line-mode 1)
;;     (custom-set-faces
;;      '(cursor ((t :background "#eebb28")))
;;      '(diff-added ((t :foreground "green" :underline nil)))
;;      '(diff-removed ((t :foreground "red" :underline nil)))
;;      )))

;;color-theme-sanityinc-solarized
;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :init
;;   (progn
;;     (load-theme 'sanityinc-solarized-dark :no-confirm)
;;     (setf frame-background-mode 'dark)
;;     (global-hl-line-mode 1)
;;     (custom-set-faces
;;      '(cursor ((t :background "#eebb28")))
;;      '(diff-added ((t :foreground "green" :underline nil)))
;;      '(diff-removed ((t :foreground "red" :underline nil)))
;;      '(highlight ((t :background "black" :underline nil)))
;;      '(magit-item-highlight ((t :background "black")))
;;      '(hl-line  ((t :background "blace")))
;;      )))

(require 'deft)
;; (require 'session)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name)))))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-off))

(add-to-list 'projectile-globally-ignored-directories ".kitchen")
(setq neo-smart-open t)

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate))))

(use-package json-mode
  :ensure t
  :defer t
  :config
  (progn
    (setf json-reformat:pretty-string? t
          json-reformat:indent-width 2)
    (define-key json-mode-map (kbd "M-q")
      (lambda ()
        (interactive)
        (if (region-active-p)
            (call-interactively #'json-reformat-region)
          (json-reformat-region (point-min) (point-max)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mac settings                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shell issues
;; is this needed for flyspell to work?
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; keys
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
(setq sentence-end-double-space nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (shell . t)
   (python . t)
   (ruby . t)))

;; to fix python
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; super key
(define-key global-map [?\s-d] 'projectile-find-dir)
(define-key global-map [?\s-f] 'projectile-find-file)
(define-key global-map [?\s-g] 'projectile-grep)

;; note: for iterm2, set alt to esc+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsclient                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (server-start)
(setq ns-pop-up-frames nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove crappy header from grep return
(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; persistent-scratch 
;; (use-package persistent-scratch 
;;   :config 
;;   (persistent-scratch-setup-default))

(use-package dired-filter
  :ensure t)

(define-key dired-mode-map (kbd "/") dired-filter-map)


;; rename function from Steve Yegge
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(global-set-key (kbd "C-c r") 'rename-this-buffer-and-file)

(defun mkm/fix-title()
  (interactive)
  (insert (replace-regexp-in-string " " "" (upcase-initials (org-entry-get nil "ITEM"))))
  )

(global-set-key (kbd "C-c t") 'mkm/fix-title)
(global-set-key (kbd "C-s-f") 'counsel-ag)

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

;; no more lost files!

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
;; funky files name

(setq js-indent-level 2)

(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;; Save whatever’s in the current (system) clipboard before 
;; replacing it with the Emacs’ text. 
;; https://github.com/dakrone/eos/blob/master/eos.org 
;; (setq save-interprogram-paste-before-kill t) 

(global-set-key [(control x) (control c)]
                (function
                 (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? (save-buffers-kill-terminal) ")
                          (save-buffers-kill-terminal))))))

;; auto-revert mode
;; http://nhoffman.github.io/.emacs.d/#sec-3
(global-auto-revert-mode 1)
(setq auto-revert-verbose t)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; winner-mode for sanity
(winner-mode 1)

;; save my place
(save-place-mode 1)

;; no need for tabs
(setq-default indent-tabs-mode nil)

;; fix for 'ls does not support --dired' message
(setq dired-use-ls-dired nil)

(use-package dired+
  :ensure t
  :diminish dired+-mode)

;; mkm  mkm I think simpleclip handles the pasting now
;; simpleclip
;; (require 'simpleclip)
;; (simpleclip-mode 1)

;; some editing extras
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(setq whole-line-or-region t)
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev
;;                                          try-expand-dabbrev-all-buffers
;;                                          try-expand-dabbrev-from-kill
;;                                          try-complete-file-name-partially
;;                                          try-complete-file-name
;;                                          try-expand-all-abbrevs
;;                                          try-expand-list
;;                                          try-expand-line
;;                                          try-complete-lisp-symbol-partially
;;                                          try-complete-lisp-symbol))
;; == company-mode ==
;; (use-package company
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'after-init-hook 'global-company-mode)
;;   :diminish company-mode
;;   :config

;;   (setq company-idle-delay              0.3
;;         company-minimum-prefix-length   2
;;         company-begin-commands          '(self-insert-command)
;;         company-show-numbers            t
;;         company-tooltip-limit           20
;;         company-dabbrev-downcase        nil
;;         company-echo-delay              0
;;         company-backends                '((company-elisp
;;                                            company-shell
;;                                          ))))

(eval-after-load "fundamental-mode" '(diminish 'fundamental-mode))

(use-package avy
  :ensure t
  :diminish avy-mode
  :bind (("C-." . avy-goto-char-timer)))

(setq org-startup-with-inline-images t)

;; undo tree!
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (bind-keys*
   ("C-z" . undo-tree-undo)
   ("C-S-z" . undo-tree-redo))
  (global-undo-tree-mode 1)
  (setq undo-tree-mode t))

(defun my-diff-buffer-with-file ()
  "Compare the current modified buffer with the saved version."
  (interactive)
  (let ((diff-switches "-u")) ;; unified diff
    (diff-buffer-with-file (current-buffer))))

(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x =") 'my-diff-buffer-with-file)
(global-set-key (kbd "C-x C-=") 'ediff-current-file)

;; mkm since this doesn't work in magit, change ace-window
;; windows manipulation
;; (global-set-key (kbd "C-S-p") 'ace-window)

(require 'neotree)
(global-set-key (kbd "C-`") 'neotree-toggle)

;; make it ignore neotree window
(require 'ace-window)
;; (add-to-list 'aw-ignored-buffers " *NeoTree*")

;; mkm make sure savehistory is working as intended
;; disabling so I can try session mode
(desktop-save-mode 1)
(savehist-mode 1)

;; recent files?
(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'ivy-recentf)

;; let's add dates easier
(require 'calendar)
(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p)))

(global-set-key "\C-x\M-d" `insdate-insert-current-date)

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
  (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 2)
  :init (smooth-scrolling-mode 1))
(setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro 14"))
(set-face-attribute 'default t :font "Source Code Pro 14")

;; mkm turn back on for magit?
;; new stuff
(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :diminish (git-gutter+-mode)
  :config
  (global-set-key (kbd "C-S-s") 'swiper)
  :bind (("C-x G" . git-gutter+-mode)
         ("C-x ." . git-gutter+-show-hunk-inline-at-point)
         :map git-gutter+-mode-map
         ("C-x C-n" . git-gutter+-next-hunk)
         ("C-x C-p" . git-gutter+-previous-hunk)))

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro 14"))
(set-face-attribute 'default t :font "Source Code Pro 14")

;; mkm: turn back on for magit?
(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :diminish (git-gutter+-mode))

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(require 're-builder)
(setq reb-re-syntax 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; show full path in title bar
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(blink-cursor-mode 1)

(show-paren-mode 1)
;; (setq show-paren-style 'expression)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))
  :config
  (progn
    (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")
    (setf rainbow-delimiters-max-face-count 1)
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground 'unspecified
                        :inherit 'error)
    (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Howard Abrams settings                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-scratch-message "")
(setq visible-bell t)

(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))           ;; Scrollbars are waste screen estate

(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-splitter (global-map "<f9>")
  "splitter"
  ("<left>" hydra-move-splitter-left)
  ("<down>" hydra-move-splitter-down)
  ("<up>" hydra-move-splitter-up)
  ("<right>" hydra-move-splitter-right))

(use-package which-key
  :ensure t
  :defer 10
  :diminish which-key-mode
  :config

  ;; Replacements for how KEY is replaced when which-key displays
  ;;   KEY → FUNCTION
  ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
  (setq which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("left"                  . "◀")
          ("right"                 . "▶")
          ("up"                    . "▲")
          ("down"                  . "▼")
          ("delete"                . "DEL") ; delete key
          ("\\`DEL\\'"             . "BS") ; backspace key
          ("next"                  . "PgDn")
          ("prior"                 . "PgUp"))

        ;; List of "special" keys for which a KEY is displayed as just
        ;; K but with "inverted video" face... not sure I like this.
        which-key-special-keys '("RET" "DEL" ; delete key

                                 "ESC" "BS" ; backspace key
                                 "SPC" "TAB")

        ;; Replacements for how part or whole of FUNCTION is replaced:
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ("\\`calc-"       . "") ; Hide "calc-" prefixes when listing M-x calc keys
          ("/body\\'"       . "") ; Remove display the "/body" portion of hydra fn names
          ("\\`projectile-" . "𝓟/")
          ("\\`hydra-"      . "+𝐇/")
          ("\\`org-babel-"  . "ob/"))

        ;; Underlines commands to emphasize some functions:
        which-key-highlighted-command-list
        '(("\\`hydra-" . which-key-group-description-face)
          "\\(rectangle-\\)\\|\\(-rectangle\\)"
          "\\`org-"))

  ;; Change what string to display for a given *complete* key binding
  ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
  (which-key-add-key-based-replacements
    "C-x 8"   "unicode"
    "C-c T"   "toggles-"
    "C-c p s" "projectile-search"
    "C-c p 4" "projectile-other-buffer-"
    "C-x a"   "abbrev/expand"
    "C-x r"   "rect/reg"
    "C-c /"   "engine-mode-map"
    "C-c C-v" "org-babel"))

(which-key-mode 1)

(require 'bind-key)

;; (use-package smartscan
;;   :ensure t
;;   :config
;;   (unbind-key "M-n" smartscan-map)
;;   (unbind-key "M-p" smartscan-map)
;;   (unbind-key "s-n")
;;   (unbind-key "s-p")
;;   (bind-keys :map smartscan-map
;;              ("s-n" . smartscan-symbol-go-forward)
;;              ("s-p" . smartscan-symbol-go-backward))
;;   (global-smartscan-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("tramp" (or
                   (name . "^\\*tramp.*")
                   (filename . "^/sudo.*")
                   (filename . "^/ssh.*")))
         ("markdown" (name . ".*\\.md"))
         ("dired" (mode . dired-mode))
         ("yaml" (or
                  (name . ".*\\.yml")))
         ("org" (name . "^.*org$"))
         ("web" (or (mode . web-mode) (mode . js2-mode)))
         ("shell" (or
                   (mode . eshell-mode)
                   (mode . shell-mode)
                   (name . ".*\\.sh")))
         ("programming" (or
                         (mode . python-mode)
                         (mode . c++-mode)
                         (mode . perl-mode)
                         (mode . ruby-mode)
                         (name . ".*\\.ps1")))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Help\\*$")
                   (name . ".*\\.el$")))
         ("magit" (or
                   (name . "^*magit.*:.*")))
         ("chef" (or
                  (name . ".*\\.erb")
                  (name . ".*\\.yml")))
         )))
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
					;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)

;; nearly all of this is the default layout
(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 45 45 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-mode                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-S-s") 'swiper)
  (global-set-key (kbd "C-S-r") 'swiper)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "s-i") 'counsel-imenu )
  )

(global-set-key [f6] 'ivy-resume)
(setq magit-completing-read-function 'ivy-completing-read)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'indent-tools)
;;(require 'yafolding)
;;(load "makey")
;;(require 'discover)
;;(global-discover-mode 1)
(global-set-key (kbd "C-c >") 'mkm-indent-tools-hydra/body)
;; (require 'origami)

(require 'yafolding)
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

;;;;; General hydra
(defhydra mkm-indent-tools-hydra (:color red :hint nil)
  "
 ^Indent^         | ^Navigation^        | ^Actions^
------------------+---------------------+-----------
 _>_ indent       | _j_ v               | _i_ imenu
 _<_ de-indent    | _k_ ʌ               | _C_ Copy…
 _l_ end of level | _n_ next sibling    | _c_ comment
 _E_ end of fn    | _p_ previous sibling| _U_ uncomment (paragraph)
 _P_ paragraph    | _u_ up parent       | _f_ fold
 _SPC_ space      | _d_ down child      | _F_ fold all level
 ___ undo         | _e_ end of tree     | _q_ quit
"

  (">" indent-tools-indent)
  ("<" indent-tools-demote)
  ("E" indent-tools-indent-end-of-defun)
  ("c" indent-tools-comment)
  ("U" indent-tools-uncomment)
  ("P" indent-tools-indent-paragraph)
  ("l" indent-tools-indent-end-of-level)
  ("K" indent-tools-kill-tree)
  ("C" indent-tools-copy-hydra/body :color blue)
  ("s" indent-tools-select)
  ("e" indent-tools-goto-end-of-tree)
  ("u" indent-tools-goto-parent)
  ("d" indent-tools-goto-child)
  ("S" indent-tools-select-end-of-tree)
  ("n" indent-tools-goto-next-sibling)
  ("p" indent-tools-goto-previous-sibling)
  ("i" helm-imenu)
  ("j" forward-line)
  ("k" previous-line)
  ("SPC" indent-tools-indent-space)
  ("_" undo-tree-undo)
  ("L" recenter-top-bottom)
  ("f" yafolding-toggle-element)
  ("F" yafolding-toggle-all)
  ("q" nil)
  )

(require 'highlight-indentation)
;; f3
(defun mkm/show-lines ()
  "Toggle `highlight-indentation-mode and `highlight-indentation-current-column-mode."
  (interactive)
  (highlight-indentation-mode)
  (highlight-indentation-current-column-mode))

(global-set-key (kbd "<f4>") 'highlight-indentation-current-column-mode)
(global-set-key (kbd "<f3>") 'highlight-indentation-mode)


(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#D3D3E3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

;; (setq org-special-ctrl-a/e (cons 'reversed t))
;; (setq org-special-ctrl-k t)
;; (setq org-cycle-include-plain-lists 'integrate)
;; (setq org-cycle-separator-lines 0)
;; (setq org-blank-before-new-entry (quote ((heading)
;;                                         (plain-list-item . auto))))
;; (setq org-reverse-note-order nil)

;; get rid of pesky subscript exporting
;; (setq org-export-with-sub-superscripts nil)

(setq org-replace-disputed-keys t)
(setq org-agenda-log-mode-items '(clock closed))

(require 'ox-confluence)

;;bind to key
(define-key org-mode-map (kbd "C-<") 'org-begin-template)
(global-set-key (kbd "C-<") 'org-begin-template)

(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-log-done 'time)

;; clocking stuff
;;(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-report-include-clocking-task t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-adapt-indentation nil)

;; archive cancelled tasks, too
(setq org-todo-state-tags-triggers '(("CANCELLED" ("ARCHIVE" . t ))))

;; mkm: disabled clock for now -- too much!
;; clock stuff
(defun org-agenda-timeline-all (&optional arg)
  (interactive "P")
  (with-temp-buffer
    (dolist (org-agenda-file org-agenda-files)
      (insert-file-contents org-agenda-file nil)
      (end-of-buffer)
      (newline))
    (write-file "/tmp/timeline.org")
    (org-agenda arg "L")))

(setq org-use-fast-todo-selection t)
(setq org-fast-selection-include-todo nil)
(setq org-log-into-drawer t)

(setq org-todo-keywords
           '((sequence "NEXT(n)" "TODO(t)" "PROJ(p)" "WAITING(w!)" "|" "DONE(d!)")
             (sequence "SOMEDAY(s)" "|" "CANCELED(c@!)")
             (type "AOR(a)" "|" "DONE")))
(setq org-tag-alist '((:startgroup . nil)
                      ("@monitoring" . ?m) ("@general" . ?g) ("@chef" . ?c) ("@sysops" . ?s)
                      (:endgroup . nil)
                      (:newline . nil)
                      ("tools" . ?T) ("cloudConnector" . ?C) ("deviceDB" . ?D) 
                      ))

(setq-default org-src-fontify-natively t)

(setq org-M-RET-may-split-line t)

(setq org-use-speed-commands t)

(setq org-directory "~/Documents/org")
(setq org-agenda-files (list "~/Documents/org/work"))

(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))


(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)


;; mobile-org settings -- cross your fingers!
(setq org-mobile-inbox-for-pull "~/Documents/org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; latex export settings
(add-to-list 'org-latex-packages-alist '("" "listings"))
(setq org-latex-listings t)

(setq org-latex-listings-options '(("breaklines" "true")))

;; (setq org-agenda-use-tag-inheritance '(search timeline agenda))

(setq
 org-outline-path-complete-in-steps nil
 org-refile-use-outline-path 'file
 org-refile-targets  '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
 )

(global-set-key (kbd "C-c a") 'org-agenda)

;; mkm:Tuesday, November 29, 2016 -- disable because breaking proj statistics update
;; make todo hierarchy switch to DONE when subs done
;; (defun org-summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;;(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-enforce-todo-dependencies t)

(setq org-agenda-dim-blocked-tasks t)
(setq org-enforce-todo-checkbox-dependencies t)

;; ;; some org-mode wonder
(setq org-default-notes-file (concat org-directory "/work/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(setq org-startup-folded t)
(setq org-startup-indented nil)

;; fix for leuven and ugly hidden stars
;; (let ((class '((class color))))    
;;      (custom-theme-set-faces
;;      'leuven
;;      `(org-hide ((,class (:foreground "#FFFFFF"))))))

;; ;; just archive DONE entries
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

(setq org-speed-commands-user
          '(("S" . (widen))))

;; publish zettelkasten
(setq org-publish-project-alist
      '(("zk"
         :base-directory "~/Documents/org/zk/"
         :base-extension "org"
         :publishing-directory "~/Documents/org/my_pub/"
         :makeindex non-nil
         :auto-index t
         :section-numbers nil
         :with-author nil
         :with-date nil
         :auto-sitemap t
         :with-toc nil
         :with-properties t
         :with-title t
         :with-tags t
         :with-date nil
         :with-creator nil
         :with-email nil
         :with-timestamps t
         :html-validation-link nil
         :publishing-function org-html-publish-to-html)))
;;         :html-head "<link rel=\"stylesheet\"
;; href=\"~/Documents/org/zk_pub/mystyle.css\"
;;type=\"text/css\"/>")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(beacon-color "#F8BBD0")
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
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
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#37474f")
 '(highlight-indent-guides-auto-enabled nil t)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(markdown-asymmetric-header t)
 '(markdown-command "/usr/local/bin/markdown")
 '(markdown-live-preview-delete-export (quote delete-on-export))
 '(org-agenda-files
   (quote
    ("~/Documents/org/work/work.org" "/Users/michael/Documents/org/work/gtd.org" "/Users/michael/Documents/org/work/inbox.org" "/Users/michael/Documents/org/work/journal.org" "/Users/michael/Documents/org/work/log.org" "/Users/michael/Documents/org/work/diary.org")))
 '(org-show-context-detail
   (quote
    ((occur-tree . minimal)
     (agenda . local)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . minimal))))
 '(package-selected-packages
   (quote
    (htmlize highlight-indent-guides origami makey discover indent-tools dired-ranger ranger el-get command-log-mode base16-twilight base16-twilight-theme twilight twilight-anti-bright-theme twilight-bright-theme twilight-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow-day color-theme-sanityinc-day tango-plus-theme tango-plus apropospriate-theme moe-theme base16-theme gruvbox-theme color-theme-gruvbox color-theme-sanityince-tomorrow color-theme-sanityinc-tomorrow chruby seeing-is-believing ruby-electric dired-filter dired-narrow rainbow-delimiters robe company company-shell wgrep wgrep-ack wgrep-ag ztree ivy-hydra org counselq counsel-osx-app counsel-projectile highlight-indentation company-restclient restclient test-kitchen ag json-reformat smartscan which-key smooth-scrolling color-theme smooth-scroll peep-dired org-projectile projectile with-editor session magit-popup hydra helm git-gutter+ git-commit fringe-helper epl diminish dash bind-key avys async ace-link window-number whole-line-or-region use-package swiper simpleclip rebox2 powershell powerline pkg-info pdf-tools neotree magit git-gutter-fringe+ expand-region exec-path-from-shell dired-toggle-sudo dired+ deft cl-lib-highlight boxquote ace-window)))
 '(pos-tip-background-color "#ffffff")
 '(pos-tip-foreground-color "#78909C")
 '(show-paren-mode t)
 '(tabbar-background-color "#ffffff")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))


(defun mkm/fix-title()
  (interactive)
  (insert (replace-regexp-in-string " " "" (upcase-initials (org-entry-get nil "ITEM"))))
  )

(defun mkm/zettel-file-new (x)
  "Create zettel file with name from heading."
  (interactive "sZettel Heading: ")
  (let* (
         (first-char (downcase (substring x nil 1)))
         (rest-str (substring (replace-regexp-in-string " " "" (upcase-initials x )) 1))
         (z (concat (downcase first-char) rest-str)))

    (find-file (concat "~/Documents/org/zk/" z ".md"))
    (insert (concat "# " x "\n\n"))))

(global-set-key (kbd "s-n") 'mkm/zettel-file-new)

(defun mkm/zix-file-new (x)
  "Create zix file with name from heading and zix prefix"
  (interactive "szix Heading: ")
  (let* (
         (first-char (downcase (substring x nil 1)))
         (rest-str (substring (replace-regexp-in-string " " "" (upcase-initials x )) 1))
         (z (concat (downcase first-char) rest-str)))

    (find-file (concat "~/Documents/org/zk/zix_" z ".md"))
    (insert (concat "# " x "\n\n"))))

(global-set-key (kbd "s-N") 'mkm/zix-file-new)

(setq markdown-enable-wiki-links nil)
(setq markdown-hide-urls t)
;; use visual-line mode in markdown mode
(defun my-markdown-mode-hook ()
  (visual-line-mode 1)
  (local-set-key (kbd "s-l") 'mkm/link-zk))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(defun mkm/link-zk ()
  (interactive)
  (ivy-read "ZK File: "
          (directory-files "~/Documents/org/zk" nil "^.*\.md$")
          :action (lambda (file)
                    (save-excursion
                      (with-temp-buffer
                        (insert-file-contents (concat "/Users/michael/Documents/org/zk/" file))
                        (goto-char 1)
                        (setq z (buffer-substring-no-properties 3 (line-end-position))))
                      (insert "[" z "](" file ")")
                      )
                    (end-of-line))))
    
;; (define-key markdown-mode-map (kbd "s-l") 'mkm/link-zk)

;; devonthink
;; (defun org-dtp-open (record-location) "Visit the dtp message with the given Message-ID." (shell-command (concat "open x-devonthink-item:" record-location)))

;; (org-link-set-parameters "x-devonthink-item" :follow 'org-dtp-open :export (lambda (path desc backend) (cond ((eq 'html backend) (format "<font color="red"> <a href="x-devonthink-item:%s">%s </a> </font>" path desc)))) :face '(:foreground "red") :help-echo "Click me for devonthink link.")

;; (provide 'org-devonthink)

;; ;; my own templates -- screw automation!
(setq org-capture-templates
      '(
	("j" "Journal Entry"
	 entry (file+datetree "~/Documents/org/work/journal.org")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1
	 )
	("h" "Home Entry"
	 entry (file+datetree "~/Documents/org/personal/home.org")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1
	 )
	("i" "inbox - Home" entry
	 (file+headline "~/Documents/org/personal/todo.org" "INBOX")
	 "* TODO %?")
        ("l" "A link, for reading later."
         entry (file+headline "~/Documents/org/work/inbox.org" "Reading List")
         "** %:description\n%u : %:link\n\n%i"
         :empty-lines 1)
	("f" "Fiction Entry"
	 entry (file+datetree "~/Documents/org/fiction/fiction.org")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1
	 )
	("e" "Emacs"
	 entry (file "~/Documents/org/work/notes/emacs.org")
	 "* %?\n%i\n\n")
	("x" "Linux Entry"
	 entry (file+olp "~/Documents/org/work/notes/linux.org" "General")
	 "* %?\n%i\n\n")
	("c" "Chef"
	 entry (file+headline "~/Documents/org/work/notes/chef.org" "Notes")
	 "** %?")
	("E" "E" entry
	 (file+headline"~/Documents/org/personal/eros.org" "Notes")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1)
	("t" "Todo" entry
	 (file+headline "~/Documents/org/work/work.org" "AOR INBOX")
	 "* TODO %?")
        ("d" "Diary" entry (file+datetree "~/Documents/org/work/diary.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
	))

(add-hook 'org-capture-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-todo-state-tags-triggers '(("CANCELLED" ("ARCHIVE" . t))))

;; (setq org-agenda-custom-commands
;;       '(
;;         ("z" "Available Tasks" tags-todo "-research&-home&-tools/!TODO|NEXT")
;;         ("n" "Next Tasks" tags-todo "-research&-home&-tools/!NEXT|WAITING")
;;         ("p" "Show Projects" tags-todo "-research&-home&-tools/PROJ")
;;           ))

(setq org-agenda-custom-commands
      '(("z" "Available Tasks" tags-todo "-research&-home&-tools/!NEXT|TODO"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down))))
        ("n" "Next Tasks" tags-todo "-research&-home&-tools/!NEXT|WAITING"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down))))
        ("p" "Show Projects" tags-todo "-research&-home&-tools/PROJ")
        ("c" "Simple agenda view"
         (
          ;; (tags-todo "-research&-home&-tools&/PRIORITY=\"A\""
          ;;            ((org-agenda-sorting-strategy '(todo-state-up priority-down))
          ;;             (org-agenda-overriding-header "High-Priority Tasks:")))
          (tags-todo "-research&-home&-tools/!NEXT"
                     ((org-agenda-sorting-strategy '(todo-state-up priority-down))
                      (org-agenda-overriding-header "Today's Tasks:")))
          (agenda "")
          (tags-todo "-research&-home&-tools/PROJ"
                     ((org-agenda-overriding-header "Projects:")))
          (tags-todo "-research&-home&-tools/!WAITING"
                     ((org-agenda-sorting-strategy '(todo-state-up priority-down))
                      (org-agenda-overriding-header "Waiting Tasks:")))
          (tags-todo "-research&-home&-tools/!TODO|WAITING"
                     ((org-agenda-sorting-strategy '(todo-state-up priority-down))
                      (org-agenda-overriding-header "Task Pool:")))
          (tags-todo "-research&-home&-tools&+{^p_.*}&+LEVEL=2/SOMEDAY"
                     ((org-agenda-overriding-header "Projects (Someday):")))
          ))))

;; drag and drop!
;; from http://kitchingroup.cheme.cmu.edu/blog/2015/07/10/Drag-images-and-files-onto-org-mode-and-insert-a-link-to-them/#disqus_thread
(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))


(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoclose paired syntax elements like parens, quotes, etc

;; (use-package ruby-electric
;;   :ensure t
;;   :init
;;   (add-hook 'ruby-mode-hook 'ruby-electric-mode))

;; C-. s - Run Seeing is Believing for the entire file
;; C-. c - Clear the Seeing is Believing output
;; C-. t - Tag a line to be “targeted” for evaluation by SiB
;; C-. x - Run only the “tagged” lines (those with trailing “# => ” markers)

;; (use-package seeing-is-believing
;;   :ensure t
;;   :init
;;   (add-hook 'ruby-mode-hook 'seeing-is-believing)
;;   :config
;;   (setq seeing-is-believing-prefix "C-,"))

;; (use-package chruby
;;   :ensure t)

;; (use-package inf-ruby
;;   :ensure t)

;; ;; fix the pesky send to ruby
;; (define-key inf-ruby-minor-mode-map (kbd "C-c M-r") 'ruby-send-region)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; helm setup   CURRENT

;; (require 'helm)
;; (require 'helm-config)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (setq helm-M-x-fuzzy-match t) ;; fuzzy matching
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (setq helm-buffers-fuzzy-matching t
;;        helm-recentf-fuzzy-match    t)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)


;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z




;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))



;; (when (executable-find "curl")
;;       (setq helm-google-suggest-use-curl-p t))

; (global-set-key (kbd "C-c h o") 'helm-occur) 

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (setq helm-grep-default-command
;;       "ggrep -a -d skip %e -n%cH -e %p %f")
;; (setq helm-grep-default-recurse-command
;;       "ggrep -a -d recurse %e -n%cH -e %p %f")

;; (helm-mode 1)

;; END OLD SHIT helm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; helm setup   CURRENT

;; (require 'helm)
;; (require 'helm-config)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (setq helm-M-x-fuzzy-match t) ;; fuzzy matching
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (setq helm-buffers-fuzzy-matching t
;;        helm-recentf-fuzzy-match    t)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)


;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;; OLD SHIT

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))



;; (when (executable-find "curl")
;;       (setq helm-google-suggest-use-curl-p t))

; (global-set-key (kbd "C-c h o") 'helm-occur) 

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (setq helm-grep-default-command
;;       "ggrep -a -d skip %e -n%cH -e %p %f")
;; (setq helm-grep-default-recurse-command
;;       "ggrep -a -d recurse %e -n%cH -e %p %f")

;; (helm-mode 1)

;; END OLD SHIT helm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; helm setup   CURRENT

;; (require 'helm)
;; (require 'helm-config)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (setq helm-M-x-fuzzy-match t) ;; fuzzy matching
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (setq helm-buffers-fuzzy-matching t
;;        helm-recentf-fuzzy-match    t)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)


;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;; OLD SHIT

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))



;; (when (executable-find "curl")
;;       (setq helm-google-suggest-use-curl-p t))

; (global-set-key (kbd "C-c h o") 'helm-occur) 

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (setq helm-grep-default-command
;;       "ggrep -a -d skip %e -n%cH -e %p %f")
;; (setq helm-grep-default-recurse-command
;;       "ggrep -a -d recurse %e -n%cH -e %p %f")

;; (helm-mode 1)

;; END OLD SHIT helm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; helm setup   CURRENT

;; (require 'helm)
;; (require 'helm-config)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (setq helm-M-x-fuzzy-match t) ;; fuzzy matching
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (setq helm-buffers-fuzzy-matching t
;;        helm-recentf-fuzzy-match    t)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)


;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;; OLD SHIT

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))



;; (when (executable-find "curl")
;;       (setq helm-google-suggest-use-curl-p t))

; (global-set-key (kbd "C-c h o") 'helm-occur) 

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (setq helm-grep-default-command
;;       "ggrep -a -d skip %e -n%cH -e %p %f")
;; (setq helm-grep-default-recurse-command
;;       "ggrep -a -d recurse %e -n%cH -e %p %f")

;; (helm-mode 1)

;; END OLD SHIT helm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs auto stuff                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t :height 1.0)))
 '(org-level-2 ((t :height 1.0)))
 '(org-level-3 ((t :height 1.0))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; '(org-block ((t (:background "#000000"))))
 ;; '(org-block-background ((t (:background "#000000"))))
 ;; '(org-block-begin-line ((t (:foreground "#008ED1" :background "#002E41"))))
 ;; '(org-block-end-line ((t (:foreground "#008ED1" :background "#002E41"))))
 ;; '(which-func ((t (:foreground "#008000")))))

(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OSX plist workaround
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Allow editing of binary .plist files.
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;; ;;It is necessary to perform an update!
(jka-compr-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell setup                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ispell
  :defer 15
  :config
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=ultra"
                                  "--lang=en_US")))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args   '("-d en_US"))))

    ;; Save a new word to personal dictionary without asking
    (setq ispell-silently-savep t)

    (use-package flyspell
      :diminish flyspell-mode
      :init
      (progn
        (setq flyspell-use-meta-tab nil)
        ;; Binding for `flyspell-auto-correct-previous-word'
        (setq flyspell-auto-correct-binding (kbd "C-S-j")))
      :config
      (progn
        ;; (bind-key "<C-f12>" #'flyspell-goto-next-error flyspell-mode-map)
        ;; Stop flyspell overriding other key bindings
        (define-key flyspell-mode-map (kbd "C-,") nil)
        (define-key flyspell-mode-map (kbd "C-.") nil)
        (global-set-key (kbd "<f12>") 'flyspell-mode))

        (add-hook 'prog-mode-hook #'flyspell-prog-mode)
        (with-eval-after-load 'auto-complete
          (ac-flyspell-workaround))
        ;; https://github.com/larstvei/dot-emacs#flyspell
        (add-hook 'text-mode-hook #'turn-on-flyspell)
        (add-hook 'org-mode-hook  #'turn-on-flyspell)

        ;; Flyspell signals an error if there is no spell-checking tool is
        ;; installed. We can advice `turn-on-flyspell' and `flyspell-prog-mode'
        ;; to try to enable flyspell only if a spell-checking tool is available.
        (defun modi/ispell-not-avail-p (&rest args)
          "Return `nil' if `ispell-program-name' is available; `t' otherwise."
          (not (executable-find ispell-program-name)))
        (advice-add 'turn-on-flyspell   :before-until #'modi/ispell-not-avail-p)
        (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p))))

(provide 'setup-spell)
(put 'narrow-to-region 'disabled nil)
