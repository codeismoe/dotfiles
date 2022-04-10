;;; init --- Summary
;;; Commentary:
;;; Init file for Emacs
;;; Code:

;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)

;; backup stuff
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; TODO Fix this per computer
(let ((paths (list "/etc/profiles/per-user/pks/bin"
		   "/opt/homebrew/bin"
		   (concat (getenv "HOME") "/.ghcup/bin")
		   (concat (getenv "HOME") "/.cargo/bin"))))
  (setq exec-path (append exec-path paths))
  (setenv "PATH" (string-join exec-path ":")))

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" 0 custom-file))
(load-file custom-file)

;; aesthetics
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Iosevka 16")
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p #'y-or-n-p)

;; color theme
(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox t))

;; company mode
(use-package company
  :straight t
  :hook ((after-init . global-company-mode)))

;; git
(use-package magit
  :straight t)

(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window))

;; projectile stuff
(use-package projectile
  :straight t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

;; which key
(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

;; flycheck
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode 1))

(use-package nix-mode
  :straight t)

;; org mode
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;; init.el ends here
