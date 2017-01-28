;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH"
	(concat (getenv "PATH")
		":/usr/local/bin"))

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

;; mkm: moving to use-package for everything, so following will become useless
(setq my-package-list '(ace-link ace-window async avy bind-key boxquote cl-lib-highlight dash deft diminish dired+ dired-toggle-sudo epl exec-path-from-shell expand-region fringe-helper git-commit git-gutter+ git-gutter-fringe+ helm leuven-theme lua-mode magit magit-popup markdown-mode neotree pdf-tools pkg-info powerline powershell projectile rebox2 color-theme-sanityinc-tomorrow session simpleclip swiper use-package whole-line-or-region window-number with-editor with-editor worf ))

(mapc #'package-install my-package-list)

;; mkm: not used
(defun org-src-color-blocks-light ()
  "Colors the block headers and footers to make them stand out more for lighter themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
    ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(org-block-background
     ((t (:background "#ffe1ff"))))
   '(org-block
     ((t (:background "#ffe1ff"))))
   '(org-block-end-line
     ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))))

; mkm: not used
(defun org-src-color-blocks-dark ()
  "Colors the block headers and footers to make them stand out more for dark themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:foreground "#008ED1" :background "#002E41"))))
   '(org-block-background
     ((t (:background "#000000"))))
   '(org-block
     ((t (:background "#000000"))))
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background "#002E41"))))))


;; theme shift
(defun load-only-theme ()
  "Disable all themes and then load a single theme interactively."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (call-interactively 'load-theme))

(defun switch-theme (theme)
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

;;(load-theme 'minimal-light t)
;; (load-theme 'leuven t)

;; (defcustom default-light-color-theme 'sanityinc-tomorrow-day
;;   "default light theme")

;; (defcustom default-dark-color-theme 'sanityinc-tomorrow-night
;;   "default dark theme")

;; (defcustom default-light-color-theme 'minimal-light
;;   "default light theme")

(defcustom default-light-color-theme 'material-light
  "default light theme")

;; (defcustom default-dark-color-theme 'minimal
;;   "default dark theme")

(defcustom default-dark-color-theme 'material
  "default dark theme")

(defun toggle-dark-light-theme ()
  (interactive)

  (let ((is-light (find default-light-color-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if is-light
        (progn
          (load-theme default-dark-color-theme t))
;;          (org-src-color-blocks-dark))
      (progn
        (load-theme default-light-color-theme t)))))
  ;;      (org-src-color-blocks-light)))))

(add-hook 'after-make-frame-functions 'my-theme)

;; now to handle that icky larger sized org mode heading!
;; (custom-theme-set-faces
;;  'material
;;  '(org-level-1 ((t (:height 1.0))))
;;  '(org-level-2 ((t (:height 1.0))))
;;  '(org-level-3 ((t (:height 1.0))))
;;  '(org-level-4 ((t (:height 1.0))))
;;  '(org-level-5 ((t (:height 1.0))))
;;  '(org-level-6 ((t (:height 1.0)))))
 

;; (defun my-theme (frame)
;;   (if (display-graphic-p frame)
;;       (progn
;;         (tool-bar-mode -1)
;;         (scroll-bar-mode -1)
;;         (global-set-key (kbd "C-+") 'toggle-dark-light-theme)
;;         (load-theme 'minimal-light t)
;;         (custom-theme-set-faces
;;          'minimal-light
;;          '(org-level-1 ((t (:height 1.0))))
;;          '(org-level-2 ((t (:height 1.0))))
;;          '(org-level-3 ((t (:height 1.0))))
;;          '(org-level-4 ((t (:height 1.0))))
;;          '(org-level-5 ((t (:height 1.0))))
;;          '(org-level-6 ((t (:height 1.0))))
;;          ))))
        
(defun my-theme (frame)
  (if (display-graphic-p frame)
      (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (global-set-key (kbd "C-+") 'toggle-dark-light-theme)
        (load-theme 'material-light t)
        (custom-theme-set-faces
         'material-light
         '(org-level-1 ((t (:height 1.0))))
         '(org-level-2 ((t (:height 1.0))))
         '(org-level-3 ((t (:height 1.0))))
         '(org-level-4 ((t (:height 1.0))))
         '(org-level-5 ((t (:height 1.0))))
         '(org-level-6 ((t (:height 1.0))))
         ))))



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

(add-to-list 'projectile-globally-ignored-directories ".kitchen")
(setq neo-smart-open t)

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

(setq js-indent-level 2)

(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;; Save whatever’s in the current (system) clipboard before 
;; replacing it with the Emacs’ text. 
;; https://github.com/dakrone/eos/blob/master/eos.org 
(setq save-interprogram-paste-before-kill t) 

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

;; love me some ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :diminish company-mode
  :config

  (setq company-idle-delay              0.3
        company-minimum-prefix-length   2
        company-begin-commands          '(self-insert-command)
        company-show-numbers            t
        company-tooltip-limit           20
        company-dabbrev-downcase        nil
        company-echo-delay              0
        company-backends                '((company-elisp
                                           company-shell
                                         ))))

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
(global-set-key (kbd "C-S-p") 'ace-window)

(require 'neotree)
(global-set-key (kbd "C-~") 'neotree-toggle)

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
(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :diminish (git-gutter+-mode)
  :bind (("C-x G" . git-gutter+-mode)
         (:map git-gutter+-mode-map
              ("C-x N" . git-gutter+-next-hunk)
              ("C-x P" . git-gutter+-previous-hunk))))

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

(set-cursor-color "red")
(blink-cursor-mode 1)

(show-paren-mode 1)

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

(use-package smartscan
  :ensure t
  :config
  (unbind-key "M-n" smartscan-map)
  (unbind-key "M-p" smartscan-map)
  (unbind-key "s-n")
  (unbind-key "s-p")
  (bind-keys :map smartscan-map
             ("s-n" . smartscan-symbol-go-forward)
             ("s-p" . smartscan-symbol-go-backward))
  (global-smartscan-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tramp-default-method "ssh")

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
  )


(global-set-key [f6] 'ivy-resume)
(setq magit-completing-read-function 'ivy-completing-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

(setq org-special-ctrl-a/e (cons 'reversed t))
(setq org-special-ctrl-k t)


;; get rid of pesky subscript exporting
(setq org-export-with-sub-superscripts nil)

;;bind to key
(define-key org-mode-map (kbd "C-<") 'org-begin-template)
(global-set-key (kbd "C-<") 'org-begin-template)

(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-log-done 'time)

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
           '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAITING(w!)" "|" "DONE(d!)")
             (sequence "SOMEDAY(s)" "|" "CANCELED(c@!)")
             (type "AOR(a)" "|" "DONE")))
(setq org-tag-alist '((:startgroup . nil)
                      ("@monitoring" . nil) ("@home" . nil) ("@general" . nil) ("@chef" . nil) ("@sysops" . nil)
                      (:endgroup . nil)
                      (:newline . nil)
                      ("tools" . ?T) ("cloudConnector" . ?C) ("deviceDB" . ?D) 
                      ))

(setq-default org-src-fontify-natively t)
(setq org-fontify-whole-heading-line t)

(setq org-M-RET-may-split-line t)

(setq org-use-speed-commands t)

(setq org-directory "~/Documents/org")
(setq org-agenda-files (list "~/Documents/org"))

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

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 't)
(setq org-enforce-todo-checkbox-dependencies t)

;; ;; some org-mode wonder
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(setq org-startup-folded t)

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

;; ;; my own templates -- screw automation!
(setq org-capture-templates
      '(
	("j" "Journal Entry"
	 entry (file+datetree "~/Documents/org/journal.org")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1
	 )
	("f" "Fiction Entry"
	 entry (file+datetree "~/Documents/org/fiction.org")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1
	 )
	("p" "Private Journal"
	 entry (file+datetree "~/Documents/org/personal.org" "Journal")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1)
	("i" "Inbox"
	 entry (file "~/Documents/org/inbox.org")
	 "** %? \n%U\n")
	("e" "Emacs"
	 entry (file "~/Documents/org/emacs.org")
	 "* %?\n%i\n\n")
	("l" "Linux Entry"
	 entry (file+olp "~/Documents/org/linux.org" "General")
	 "* %?\n%i\n\n")
	("c" "Chef"
	 entry (file+headline "~/Documents/org/chef.org" "Notes")
	 "** %?")
	("E" "E" entry
	 (file+headline"~/Documents/org/eros.org" "Notes")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1)
	("G" "Todo E" entry
	 (file+headline "~/Documents/org/eros.org" "Todo GTD")
	 "* TODO %?")
        ("d" "Diary" entry (file+datetree "~/Documents/org/diary.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
	))

(add-hook 'org-capture-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-todo-state-tags-triggers '(("CANCELLED" ("ARCHIVE" . t))))

(setq org-agenda-custom-commands
      '(
        ("z" "Available Tasks" tags-todo "-research&-home&-tools/!TODO|NEXT")
        ("n" "Next Tasks" tags-todo "-research&-home&-tools/!NEXT")
        ("p" "Show Projects" tags-todo "-research&-home&-tools/PROJ")
          ))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "4156d0da4d9b715c6f7244be34f2622716fb563d185b6facedca2c0985751334" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f01e589752ca7edbda53ff23f28f58ce313d3716edb39cbc1e9093a67d41a5b7" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/Documents/org/eros.org" "~/Documents/org/linux.org" "~/Documents/org/work.org" "~/Documents/org/personal.org" "~/Documents/org/prj_security.org" "~/Documents/org/prj_tim.org" "~/Documents/org/prj_device.org" "~/Documents/org/prj_sensu.org" "/Users/michael/Documents/org/brent.org" "/Users/michael/Documents/org/cbt.org" "/Users/michael/Documents/org/chef.org" "/Users/michael/Documents/org/emacs.org" "/Users/michael/Documents/org/fiction.org" "/Users/michael/Documents/org/gtd.org" "/Users/michael/Documents/org/inbox.org" "/Users/michael/Documents/org/journal.org" "/Users/michael/Documents/org/log.org" "/Users/michael/Documents/org/rhsca.org" "/Users/michael/Documents/org/skillet.org" "/Users/michael/Documents/org/diary.org")))
 '(org-show-context-detail
   (quote
    ((occur-tree . minimal)
     (agenda . local)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . ancestors))))
 '(package-selected-packages
   (quote
    (rainbow-delimiters solarized-theme danneskjold-theme robe company company-shell wgrep wgrep-ack wgrep-ag ztree minimal-theme material-theme ivy-hydra org counselq counsel-osx-app counsel-projectile highlight-indentation company-restclient restclient test-kitchen ag json-reformat color-theme-sanityinc-tomorrow smartscan which-key smooth-scrolling color-theme-sanityinc-tomorrow-day color-theme color-theme-modern darkokai-theme monokai-theme gruvbox-theme soft-stone-theme gotham-theme tao-theme twilight-bright-theme warm-night-theme smooth-scroll peep-dired org-projectile projectile with-editor session magit-popup hydra helm git-gutter+ git-commit fringe-helper epl diminish dash bind-key avys async ace-link window-number whole-line-or-region use-package swiper simpleclip rebox2 powershell powerline pkg-info pdf-tools neotree markdown-mode magit lua-mode leuven-theme git-gutter-fringe+ expand-region exec-path-from-shell dired-toggle-sudo dired+ deft cl-lib-highlight boxquote ace-window)))
 '(show-paren-mode t)
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
      :init
      (progn
        (setq flyspell-use-meta-tab nil)
        ;; Binding for `flyspell-auto-correct-previous-word'
        (setq flyspell-auto-correct-binding (kbd "C-S-j")))
      :config
      (progn
        (bind-key "<C-f12>" #'flyspell-goto-next-error flyspell-mode-map)
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
