;;; init.el --- Its my init
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Custom file initialization
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)

;; Aesthetics
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-resize-pixelwise t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "stroustrup")

;; Packages
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-light t))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper-isearch))

(use-package counsel
  :ensure t
  :bind ("M-x" . counsel-M-x))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-;" . avy-goto-char-timer)))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package magit
  :ensure t)

(use-package vue-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :hook (lisp-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package qml-mode
  :ensure t)
(use-package company-qml
  :ensure t)

(use-package julia-mode
  :ensure t)

(use-package rtags
  :ensure t)

(use-package irony
  :ensure t
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)))

(use-package flycheck-irony
  :ensure t
  :config (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; LSP Stuff
(use-package lsp-mode
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp))
(use-package lsp-ivy
  :ensure t)

;; Evil Mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

;; org mode
(use-package org-roam
  :ensure t
  :custom (org-roam-directory "~/org/"))
;; projectile

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))
;; Hooks
(add-hook 'before-save-hook 'whitespace-cleanup)

;; slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; backups
(defvar pks/backup-directory (concat user-emacs-directory "backups/"))
(defvar pks/autosave-directory (concat user-emacs-directory "autosaves/"))

(unless (file-exists-p pks/backup-directory)
  (make-directory pks/backup-directory))

(unless (file-exists-p pks/autosave-directory)
  (make-directory pks/autosave-directory))

(setq backup-directory-alist `(("." . ,pks/backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 9
      auto-save-default 20
      auto-save-interval 200)

(setq auto-save-file-name-transforms `((".*" ,pks/autosave-directory t)))
(setq create-lockfiles nil)

;; y-or-n alias
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init)
;;; init.el ends here
