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
(set-frame-font "Inconsolata 14")

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "stroustrup")

;; usepackage conifg
(require 'use-package-ensure)
(setq use-package-always-ensure t)

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
  :bind ("M-o" . ace-window))

(use-package magit)

(use-package smartparens
  :hook (lisp-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package company
  :diminish company-mode
  :config
  (global-company-mode 1))

(use-package julia-mode
  :config
  (setenv "JULIA_NUM_THREADS"
          ;; get # of threads
          (let ((string (car (cl-remove-if-not
                              (lambda (x)
                                (let ((match (string-match-p "^CPU" x)))
                                  (and match (= match 0))))
                              (process-lines "lscpu")))))
            (string-match "\\([0-9]+\\)" string)
            (match-string 0 string))))

(use-package yaml-mode)

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

;; helm
(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x f" . helm-find-files)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list))
  :config
  (helm-mode 1)
  (add-to-list 'display-buffer-alist
               `("*.*Helm.*"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . bottom)
                 (window-height . 0.3))))

(use-package helm-swoop
  :bind ("C-s" . helm-swoop))
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


;; elixir
(use-package elixir-mode
  :hook (elixir-mode-hook . lsp))

(use-package alchemist)

;; erlang
(use-package erlang)
;; javascript/typescript/etc
(defun setup-tide-mode ()
  "Setup tide-mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'init)
;;; init.el ends here
