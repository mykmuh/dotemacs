;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq package-enable-at-startup nil)


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

(defcustom default-light-color-theme 'sanityinc-tomorrow-day
  "default light theme")

(defcustom default-dark-color-theme 'sanityinc-tomorrow-night
  "default dark theme")

(defun toggle-dark-light-theme ()
  (interactive)

  (let ((is-light (find default-light-color-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if is-light
        (progn
          (load-theme default-dark-color-theme t)
          (org-src-color-blocks-dark))
      (progn
        (load-theme default-light-color-theme t)
        (org-src-color-blocks-light)))))

(add-hook 'after-make-frame-functions 'my-theme)

(defun my-theme (frame)
  (if (display-graphic-p frame)
      (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (global-set-key (kbd "C-+") 'toggle-dark-light-theme)
        (progn
          (load-theme 'sanityinc-tomorrow-night t)
          (org-src-color-blocks-dark)))))

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

;; (setq projectile-use-native-indexing t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mac settings                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mkm does this copy and paste still need to be used?
;; for copy on oxs and paste
;; (defun copy-from-osx ()
;;    (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)


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
;; mkm 2016-06-27  DISABLED
;; make modified buffers auto-reload
;; (defadvice server-visit-files (around save-buffers last activate)
;;   "Try to emulate gnuclient behavior with emacsclient.
;;     Works only for visiting one buffer at a time."
;;   (let* ((filen (car (car (ad-get-arg 0))))
;;          (buf (get-file-buffer filen))
;;          (this-buf-modified-p nil))
;;     ;;; the following is copied from server-visit-files, with
;;     ;;; a modification for the `verify-visited-file-modtime' test
;;     (if (and buf (set-buffer buf))
;;         (if (file-exists-p filen)
;;             ;;; if the file has changed on disk, reload it
;;             ;;; using `find-file-noselect'
;;             (if (not (verify-visited-file-modtime buf))
;;                 (progn
;;                   (find-file-noselect filen)
;;                    ;;; if user answered `no', reset modtime anyway
;;                    ;;; so that server-visit-files doesn't realize the
;;                    ;;; difference:
;;                   (set-visited-file-modtime)))
;;           ;;; if file exists no longer, we let server-visit-files
;;           ;;; deal with that
;;           t)
;;       (setq buf (find-file-noselect filen)))
;;     (setq this-buf-modified-p (buffer-modified-p buf))
;;     (set-buffer buf)
;;     (set-buffer-modified-p nil)
;;     ad-do-it
;;     (set-buffer-modified-p this-buf-modified-p)))

;; (defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
;;   "Hide Emacs instead of closing the last frame"
;;   (let ((frame   (posn-window (event-start event)))
;;         (numfrs  (length (frame-list))))
;;     (if (> numfrs 1)
;;       ad-do-it
;;       (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))


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


;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; persistent-scratch 
(use-package persistent-scratch 
  :config 
  (persistent-scratch-setup-default))


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

;; winner-mode for sanity
(winner-mode 1)

;; save my place
(save-place-mode 1)

;; love me some ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; no need for tabs
(setq-default indent-tabs-mode nil)

;; peep-dired
;; (setq peep-dired-cleanup-on-disable t)
;; (setq peep-dired-ignored-extensions '("iso" "png"))

;; fix for 'ls does not support --dired' message
(setq dired-use-ls-dired nil)

(use-package dired+
  :ensure t
  :diminish dired+-mode)

;; mkm  mkm I think simpleclip handles the pasting now
;; simpleclip
(require 'simpleclip)
(simpleclip-mode 1)


;; mkm shouldn't pdf-tools be back on?
;; trying pdf-tools
;; (pdf-tools-install)

;; some editing extras
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(setq whole-line-or-region t)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package avy
  :ensure t
  :diminish avy-mode
  :bind (("C-." . avy-goto-word-1)))

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

;; mkm Wednesday, April 6, 2016: never used this!
;; mkm Do I want to keep ctrl-z for undo?
;; make ctrl-z undo
;; (global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
;; (defalias 'redo 'undo-tree-redo)
;; (global-set-key (kbd "C-S-z") 'redo)
;; remember, C-x u to get the tree!!!

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
;; (require 'recentf)
;; (recentf-mode 1)
;; (global-set-key (kbd "C-x C-r") 'ivy-recentf)


;; mkm do I really need the insert data feature?
;; mkm should I consider hydra for these sorts of easily forgotten commands?
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

;; pesky current line
;; (global-hl-line-mode +1)
;; (setq global-hl-line-sticky-flag t)
;; keep my sanity
(show-paren-mode 1)
;; mkm Tuesday, March 8, 2016: turned off because bugging me
;; (electric-pair-mode)

;; mkm Wednesday, April 20, 2016
;; current main -- highlight line too subtle!
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "black")
;; (set-face-foreground 'highlight nil)

;; light
;; (load-theme 'soft-stone t)
;; (load-theme 'twilight-bright t)

;; not enough contrast
;; (load-theme 'tao-yang t)

;; (load-theme 'leuven t)

;; too Xmas
;; (load-theme 'sanityinc-tomorrow-day t)

;; gruvbox has too garish brights
;; (load-theme 'gruvbox t)
;; (load-theme 'monokai t)


;; Howard Abrams settings
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


;; (ha/change-theme 'color-theme-sanityinc-tomorrow-day
;;                  'org-src-color-blocks-light)
;; (ha/change-theme 'color-theme-sanityinc-tomorrow-night
;;                    'org-src-color-blocks-dark)

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

;; (load-theme 'darkokai t)
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro 14"))
(set-face-attribute 'default t :font "Source Code Pro 14")

;; smooth scrolling on mac when not using railwaycat
;; (use-package smooth-scroll
;;   :config
;;   (smooth-scroll-mode 1)
;;   (setq smooth-scroll/vscroll-step-size 5)
;;  )

;; mkm turn back on for magit?
(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :diminish (git-gutter+-mode))

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; (error "Done")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monday, June 27, 2016 Too complicated of tramp setup for now, really need it?

;; if this works, I need to kiss somebody
;; (require 'tramp)
;; (require 'dired)

(setq tramp-default-method "ssh")

;; (defun sudo-edit-current-file ()
;;   (interactive)
;;   (let ((my-file-name) ; fill this with the file to open
;;         (position))    ; if the file is already open save position
;;     (if (equal major-mode 'dired-mode) ; test if we are in dired-mode 
;;         (progn
;;           (setq my-file-name (dired-get-file-for-visit))
;;           (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
;;       (setq my-file-name (buffer-file-name); hopefully anything else is an already opened file
;;             position (point))
;;       (find-alternate-file (prepare-tramp-sudo-string my-file-name))
;;       (goto-char position))))

;; (defun prepare-tramp-sudo-string (tempfile)
;;   (if (file-remote-p tempfile)
;;       (let ((vec (tramp-dissect-file-name tempfile)))

;;         (tramp-make-tramp-file-name
;;          "sudo"
;;          (tramp-file-name-user nil)
;;          (tramp-file-name-host vec)
;;          (tramp-file-name-localname vec)
;;          (format "ssh:%s@%s|"
;;                  (tramp-file-name-user vec)
;;                  (tramp-file-name-host vec))))
;;     (concat "/sudo:root@localhost:" tempfile)))

;; (define-key dired-mode-map [s-return] 'sudo-edit-current-file)

;; mkm Wednesday, April 6, 2016: cleaning up mode line, don't tramp much
;; show me where I'm at!
;; (defconst my-mode-line-buffer-identification
;;   (list
;;    '(:eval
;;      (let ((host-name
;;             (if (file-remote-p default-directory)
;;                 (tramp-file-name-host
;;                  (tramp-dissect-file-name default-directory))
;;               (system-name))))
;;        (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
;;            (substring host-name 0 (match-beginning 1))
;;          host-name)))
;;    ": %12b"))

;; (setq-tedefault
;;  mode-line-buffer-identification
;;  my-mode-line-buffer-identification)

;; (add-hook
;;  'dired-mode-hook
;;  (lambda ()
;;    (setq
;;     mode-line-buffer-identification
;;     my-mode-line-buffer-identification)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-mode                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disabled ivy mode to re-try helm on November 12, 2015

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(global-set-key (kbd "C-S-s") 'swiper)
(global-set-key (kbd "C-S-r") 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; (global-set-key (kbd "M-s") 'isearch-forward)
;; (define-key isearch-mode-map "\M-s" 'isearch-repeat-forward)


(global-set-key [f6] 'ivy-resume)
(setq magit-completing-read-function 'ivy-completing-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

;;bind to key
(define-key org-mode-map (kbd "C-<") 'org-begin-template)
(global-set-key (kbd "C-<") 'org-begin-template)

(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-log-done 'time)

(setq org-adapt-indentation nil)

;; clock stuff
;; (defun org-agenda-timeline-all (&optional arg)
;;   (interactive "P")
;;   (with-temp-buffer
;;     (dolist (org-agenda-file org-agenda-files)
;;       (insert-file-contents org-agenda-file nil)
;;       (end-of-buffer)
;;       (newline))
;;     (write-file "/tmp/timeline.org")
;;     (org-agenda arg "L")))


(setq org-use-fast-todo-selection t)
(setq org-fast-selection-include-todo nil)
(setq org-log-into-drawer t)

(setq org-todo-keywords
           '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAITING(w!)" "|" "DONE(d!)")
             (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
	     (sequence "RAND(r)" "Expand" "|" "Done(d)")
             (sequence "SOMEDAY(s)" "|" "CANCELED(c@!)")))
(setq org-tag-alist '(("ops" . ?O) ("tools" . ?T) ("research" . ?R) ("mastery" . ?M) ("home" . ?H) ("inbox" . "I")("hot" . "h")))

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

;; make todo hierarchy switch to DONE when subs done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 't)
(setq org-enforce-todo-checkbox-dependencies t)

;; ;; some org-mode wonder
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)

;; (setq org-todo-keyword-faces
;;       '(("WAIT" . "dark gray")))

(setq org-goto-interface 'outline
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
rd	 entry (file+datetree "~/Documents/org/fiction.org")
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
	("s" "Security"
	 entry (file "~/Documents/org/prj_security.org")
	 "* %?\n%i\n\n")
	("g" "GTD" entry
	 (file+headline "~/Documents/org/gtd.org" "Misc")
	 "* TODO %?")
	("r" "RAND" entry
	 (file "~/Documents/org/research.org")
	 "* RAND %?")
	("c" "Chef"
	 entry (file+headline "~/Documents/org/chef.org" "Notes")
	 "** %?")
	("h" "Home"
	 entry (file+headline "~/Documents/org/gtd.org" "Home")
	 "* TODO %?")
	("l" "Log" entry
	 (file+headline "~/Documents/org/log.org" "Tasks")
	 "* %U %i %?\n"
         :clock-in 1
         :clock-keep 1)
	("E" "E" entry
	 (file+datetree "~/Documents/org/eros.org" "Journal")
	 "* %?\n\n\n%i\n"
	 :empty-lines 1)
	("G" "Todo E" entry
	 (file+headline "~/Documents/org/eros.org" "Todo GTD")
	 "* TODO %?")
	("a" "Anxiety Template" entry
	 (file+headline "~/Documents/org/cbt.org" "Triple")
	 (file "~/Documents/org/cbt.template"))
	))

(add-hook 'org-capture-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-todo-state-tags-triggers '(("CANCELLED" ("ARCHIVE" . t))))

(setq org-agenda-custom-commands
      '(("z" tags-todo "-research&-home&-tools/!TODO|NEXT|PROJ")
        ("n" tags-todo "-research&-home&-tools/!NEXT")))

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

;; (error "Done")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/Documents/org/work.org" "~/Documents/org/personal.org" "~/Documents/org/prj_security.org" "~/Documents/org/prj_tim.org" "~/Documents/org/prj_device.org" "~/Documents/org/prj_sensu.org" "~/Documents/org/lita_bot.org" "~/Documents/org/career.org" "/Users/michael/Documents/org/brent.org" "/Users/michael/Documents/org/cbt.org" "/Users/michael/Documents/org/chef.org" "/Users/michael/Documents/org/craft.org" "/Users/michael/Documents/org/emacs.org" "/Users/michael/Documents/org/fiction.org" "/Users/michael/Documents/org/gtd.org" "/Users/michael/Documents/org/inbox.org" "/Users/michael/Documents/org/journal.org" "/Users/michael/Documents/org/language.org" "/Users/michael/Documents/org/log.org" "/Users/michael/Documents/org/notes.org" "/Users/michael/Documents/org/research.org" "/Users/michael/Documents/org/rhsca.org" "/Users/michael/Documents/org/skillet.org" "/Users/michael/Documents/org/time.org")))
 '(package-selected-packages
   (quote
    (ivy-hydra org counselq counsel-osx-app counsel-projectile highlight-indentation company-restclient restclient test-kitchen ag json-reformat color-theme-sanityinc-tomorrow smartscan which-key smooth-scrolling color-theme-sanityinc-tomorrow-day color-theme color-theme-modern darkokai-theme monokai-theme gruvbox-theme soft-stone-theme gotham-theme tao-theme twilight-bright-theme warm-night-theme smooth-scroll peep-dired org-projectile projectile with-editor session magit-popup hydra helm git-gutter+ git-commit fringe-helper epl diminish dash bind-key avys async ace-link window-number whole-line-or-region use-package swiper simpleclip rebox2 powershell powerline pkg-info pdf-tools neotree markdown-mode magit lua-mode leuven-theme git-gutter-fringe+ expand-region exec-path-from-shell dired-toggle-sudo dired+ deft cl-lib-highlight boxquote ace-window)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#000000"))))
 '(org-block-background ((t (:background "#000000"))))
 '(org-block-begin-line ((t (:foreground "#008ED1" :background "#002E41"))))
 '(org-block-end-line ((t (:foreground "#008ED1" :background "#002E41"))))
 '(which-func ((t (:foreground "#008000")))))

(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OSX plist bullshit
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

;; Time-stamp: <2015-08-26 08:48:04 kmodi>
;;
;; Spell check
;; ispell, flyspell
;; aspell, hunspell
;;
;; NOTE: You need to have `aspell' or `hunspell' installed first
;;
;; Aspell Setup:
;; 1. Install aspell from http://aspell.net/
;;    - Install using ./configure --prefix=~/usr_local/bin, make, make install
;; 2. Download the latest dictionary from ftp://ftp.gnu.org/gnu/aspell/dict/0index.html
;;    and extract it.
;;    - Install the dictionary using ./configure, make, make install
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;; 2. Download openoffice dictionary extension from
;;    http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;;    the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;;    save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;;     setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;;    variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'
;;
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; (error "Done")


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

;; How to add a new word to the dictionary?
;; 1. Run ispell-word when the cursor is over the word ( `M-$' )
;; 2. Press `i' to add the word to the dictionary
;; 3. Done!
;;
;; For `aspell', the new words are auto added to `~/.aspell.en.pws'.
;; For `hunspell', the new words are auto added to `~/.hunspell_en_US'.
;;
;; If the word does not auto-correct properly, call the function
;; `flyspell-auto-correct-previous-word' repeatedly till you find the
;; right match. It is easy if a key is bound to call that function.
;; (put 'narrow-to-region 'disabled nil)


(require 're-builder)
(setq reb-re-syntax 'string)
