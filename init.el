;; switch to org mode init
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; keep custom crap out!
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(put 'upcase-region 'disabled nil)
