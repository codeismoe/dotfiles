;;; init --- Summary
;;; Commentary:
;;; Init file for Emacs
;;; Code:

;; straight.el backup
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
(set-frame-font "Inconsolata Nerd Font 16")
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p #'y-or-n-p)

;; color theme
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox t)

;; eyebrowse (project things)
(straight-use-package 'eyebrowse)
(eyebrowse-mode 1)

;; meow editing
(straight-use-package 'meow)
(load-file "~/.emacs.d/meow.el")
(require 'meow)
(meow-setup)
(meow-global-mode 1)

;; company mode
(straight-use-package 'company)
(add-hook 'after-init-hook #'global-company-mode)

;; counsel
(straight-use-package 'counsel)
(straight-use-package 'swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper)


;; git
(straight-use-package 'magit)

(straight-use-package 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; projectile stuff
(straight-use-package 'projectile)
(require 'projectile)
(projectile-mode 1)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;; which key

(straight-use-package 'which-key)
(which-key-mode 1)

;; flycheck
(straight-use-package 'flycheck)
(require 'flycheck)
(global-flycheck-mode 1)

;; direnv
(straight-use-package 'direnv)
(direnv-mode 1)

;;; lsp
(straight-use-package 'lsp-pyright)
(straight-use-package 'lsp-haskell)
(straight-use-package 'lsp-mode)
(require 'lsp)
(add-hook 'python-mode-hook (lambda ()
			      (require 'lsp-pyright)
			      (lsp)))
(add-hook 'haskell-mode 'lsp)
(add-hook 'lsp-mode 'lsp-enable-which-key-integration)
(setq lsp-keymap-prefix "s-l"
      lsp-rust-server 'rust-analyzer)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-ivy)
(straight-use-package 'dap-mode)

;; tide
(straight-use-package 'tide)
(defun setup-tide-mode ()
  "Hook to setup tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'typescript-mode 'setup-tide-mode)

(straight-use-package 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist
	     (mapcar (lambda (x)
		       (cons x 'web-mode))
		     '("\\.ejs\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'")))
(setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'"))
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-script-padding 2
      web-mode-block-padding 2
      web-mode-style-padding 2
      web-mode-enable-auto-pairing t
      web-mode-enable-auto-closing t
      web-mode-enable-current-element-highlight t)

;; org mode
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(provide 'init)
;;; init.el ends here
