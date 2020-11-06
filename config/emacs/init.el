;;; init.el --- Its my init  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Custom file initialization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)

;; Aesthetics
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-resize-pixelwise t)
(set-frame-font "Inconsolata 12")

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "stroustrup")

;; usepackage conifg
(setq-default use-package-always-ensure t)

(use-package diminish)

;; Packages
(use-package solarized-theme
  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'solarized-dark t)))
    (load-theme 'solarized-dark t)))

(use-package avy
  :bind (("C-;" . avy-goto-char-timer)))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package magit)

(use-package smartparens
  :hook (lisp-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package company
  :diminish company-mode
  :config
  (global-company-mode 1))

(use-package julia-mode)

(use-package flycheck
  :diminish flycheck-mode
  :config (global-flycheck-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; LSP Stuff
(use-package lsp-mode)
(use-package lsp-ui)

;; org mode
(use-package org-roam
  :custom (org-roam-directory "~/org/"))
;; projectile

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1))

;; Hooks
(add-hook 'before-save-hook 'whitespace-cleanup)

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
