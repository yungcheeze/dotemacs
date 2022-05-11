;;; Early init support for older Emacs

(unless (featurep 'early-init)
  (load (expand-file-name "early-init" user-emacs-directory)))

(require 'straight)
(straight-use-package 'use-package)
(require 'use-package)

;; Packages
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package xclip
  :straight (xclip :type git :host github :repo "emacsmirror/xclip")
  :config
  (xclip-mode 1))

;;; Visuals
(use-package mood-line
  :hook (after-init . mood-line-mode))

(use-package nord-theme
  :config
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))

  (add-hook 'window-setup-hook 'on-after-init)
  (load-theme 'nord t))

(use-package all-the-icons
  :defer t)

(use-package general
  :ensure t
  :preface
  (global-unset-key (kbd "M-m"))
  (general-create-definer cheesemacs
    :prefix "M-m")
  (general-create-definer cheesemacs/buffers
    :prefix "M-m b")
  (general-create-definer cheesemacs/files
    :prefix "M-m f")
  (general-create-definer cheesemacs/git
    :prefix "M-m g")
  (general-create-definer cheesemacs/jump
    :prefix "M-m j")
  (general-create-definer cheesemacs/project
    :prefix "M-m p")
  (general-create-definer cheesemacs/search
    :prefix "M-m s")
  (general-create-definer cheesemacs/text
    :prefix "M-m x")
  (general-create-definer cheesemacs/windows
    :prefix "M-m w")

  (cheesemacs/buffers "w" 'save-buffer)
  (cheesemacs/windows "c" 'delete-window)
  (cheesemacs/windows "O" 'delete-other-windows)
  (cheesemacs/windows "o" 'other-window)
  (cheesemacs/windows "s" 'split-window-below)
  (cheesemacs/windows "/" 'split-window-right)
  (cheesemacs/windows "1" 'delete-other-windows)
  (cheesemacs/windows "2" 'split-window-below)
  (cheesemacs/windows "3" 'split-window-right)
  (cheesemacs/windows "0" 'delete-window)
  
  :config
  (general-auto-unbind-keys))

(use-package xterm-color
  :defer t
  :init
  (cheesemacs/buffers "x c" 'xterm-color-colorize-buffer))


;;; Helm 2.0
(use-package selectrum
  :init
  (cheesemacs ";" 'selectrum-repeat)
  :config
  (selectrum-mode +1)
  (setq completion-styles '(orderless))

  ;; Persist history over Emacs restarts
  (savehist-mode)

  ;; Optional performance optimization
  ;; by highlighting only the visible candidates.
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package orderless)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("M-;" . embark-act)         ;; pick some comfortable binding
   ("M-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :init
  (cheesemacs/buffers "b" 'consult-buffer)
  (cheesemacs/jump "m" 'consult-mark)
  (cheesemacs/jump "i" 'consult-imenu)
  (cheesemacs/jump "I" 'consult-imenu-multi)
  (cheesemacs/files "r" 'consult-recent-file)
  (cheesemacs/files "s" 'consult-find)
  (cheesemacs/files "f" 'find-file)
  (cheesemacs/search "s" 'consult-ripgrep))

(use-package rg
  :defer t
  :config
  (rg-enable-menu))

(use-package which-key
  :config
  (which-key-mode))

;;; Git stuff
(use-package magit
  :defer t
  :init
  (cheesemacs/git "g" 'magit)
  (cheesemacs/git "f" 'magit-find-file)
  :custom
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
(use-package magit-delta
  :defer t
  :custom
  (magit-delta-default-dark-theme "Nord")
  :hook (magit-mode . magit-delta-mode))
(use-package magit-todos
  :defer t
  :after magit
  :custom
  (magit-todos-ignored-keywords '("DONE"))
  (magit-todos-nice (when (executable-find "nice") t)
                    "avoid breaking Magit on systems that don't have `nice'.")
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (global-blamer-mode 1))

(use-package git-gutter
  :config
  (setq git-gutter:update-interval 1)
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :defer t)

;; good for files
(use-package git-link
  :defer t
  :custom
  (git-link-use-commit t))
;; good for commit objects
(use-package browse-at-remote
  :defer t)

(use-package forge
  :after magit
  :defer t)

(use-package code-review
  :defer t
  :init
  (cheesemacs/git "r" 'code-review-start))

;;;LSP
(use-package lsp-mode
  :defer t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix "M-m l")
  :commands lsp)
(use-package consult-lsp
  :defer t)
(use-package flycheck
  :defer t)
(use-package lsp-ui
  :defer t)

(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package lsp-haskell
  :hook ((haskell-mode . lsp)
	 (haskell-literate-mode . lsp)))

;;;Completion
(use-package company
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "TAB") nil)
    (define-key company-active-map (kbd "C-f") #'company-complete-selection))
  (global-company-mode 1))
(use-package company-shell
  :defer t)
(use-package company-cabal
  :defer t)
(use-package company-nixos-options
  :defer t
  )

(use-package yasnippet
  :config
  (yas-global-mode t))
(use-package yasnippet-snippets
  :after yasnippet
  :defer t)
(use-package consult-yasnippet
  :after yasnippet
  :defer t)


;;; Direnv
(use-package envrc
  :defer t
  :config
  (envrc-global-mode))
;; (use-package direnv
;;  :defer t
;;  :config
;;  (direnv-mode))

;;; Langs
(use-package nix-mode
  :defer t)
(use-package haskell-mode
  :defer t)
(use-package json-mode
  :defer t)
(use-package json-snatcher
  :defer t)
(use-package json-navigator
  :defer t)
(use-package tree-mode
  :defer t)
(use-package dockerfile-mode
  :defer t)
(use-package yaml-mode
  :defer t)

(use-package python-black
  :defer t
  :after python)
(use-package pyimport
  :defer t)
(use-package py-isort
  :defer t)
(use-package pytest
  :defer t)

(use-package format-all
  :defer t)

(use-package separedit
  :defer t)

(use-package org-projectile
  :defer t
  :init
  (cheesemacs/project "c" 'org-projectile-capture-for-current-project)
  (cheesemacs/project "n" 'org-projectile-goto-location-for-project)
  :config
  (progn
    (setq org-projectile-projects-file
          "~/work/notes/PROJECTS.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

(use-package org-autolist
  :defer t
  :hook (org-mode . org-autolist-mode))

;;; QOL
(use-package phi-search
  :defer t)
(use-package multiple-cursors
  :defer t
  :bind
  (:map mc/keymap
	("<return>" . nil)
	("C-s" . phi-search)
	("C-r" . phi-search-backward)
	("C-&" . mc/vertical-align-with-space)))

(use-package expand-region
  :defer t
  :bind ("M-." . er/expand-region))

(use-package auto-mark
  :straight
  (auto-mark :type git :host github :repo "yungcheeze/auto-mark.el")
  :config
  (setq auto-mark-command-class-alist
        '((anything . anything)
          (goto-line . jump)
          (indent-for-tab-command . ignore)
          (undo . ignore)))
  (global-auto-mark-mode 1))

(use-package recentf
  :straight nil
  :custom
  (recentf-max-menu-items 100)
  :config
  (recentf-mode))

(use-package gcmh
  :delight gcmh-mode
  :init
  (gcmh-mode t))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l ?g ?h))
  :defer t)

(use-package evil-nerd-commenter
  :defer t)

(use-package golden-ratio-scroll-screen
  :ensure t
  :custom
  (golden-ratio-scroll-highlight-flag 'before)
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package perfect-margin
  :defer t)

(use-package projectile
  :ensure t
  :init
  (cheesemacs/project "f" 'projectile-find-file)
  (cheesemacs/project "p" 'projectile-switch-project)
  (projectile-mode +1))
(use-package consult-projectile
  :after projectile
  :defer t)
(use-package flycheck-projectile
  :after projectile
  :defer t)
(use-package hl-todo
  :init
  (global-hl-todo-mode 1))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-suppress-no-prefix-key-warning t)
  :init
  (cheesemacs/windows "p" 'persp-switch)
  (cheesemacs/windows "TAB" 'persp-switch)
  (cheesemacs/project "TAB" 'persp-switch)
  
  (persp-mode))

(use-package treemacs
  :init
  (cheesemacs "t" 'treemacs-display-current-project-exclusively)
  (cheesemacs "TAB" 'treemacs-display-current-project-exclusively)
  :config
  (treemacs-project-follow-mode +1)
  )
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)
(use-package lsp-treemacs
  :defer t)

(use-package neotree
  :defer t
  :custom
  (neo-theme 'nerd))

(use-package scratch
  :defer t
  :init
  (defun scratch-with-mode ()
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'scratch)
    (setq ))
  (cheesemacs/buffers "s" 'scratch)
  (cheesemacs/buffers "S" 'scratch-with-mode))

(use-package helpful
  :defer t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

;;; Custom Binds

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-M-o") 'ace-swap-window)
(global-set-key (kbd "M-s") 'consult-line)
(global-set-key (kbd "ESC M-DEL") 'backward-kill-sexp)

;;; Custom Functions
;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun my-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
	   ;; process-list is not defined on MSDOS.
	   (let ((processes (process-list))
		 active)
             (while processes
               (and (memq (process-status (car processes)) '(run stop open listen))
		    (process-query-on-exit-flag (car processes))
		    (setq active t))
               (setq processes (cdr processes)))
             (or (not active)
		 (progn (list-processes t)
			(yes-or-no-p "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
	   (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))
(fset 'save-buffers-kill-emacs 'my-save-buffers-kill-emacs)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; Setup Shit
;; keep backups and auto-saves out of the way https://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `doom-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat user-emacs-directory "autosave/")
      tramp-auto-save-directory  (concat user-emacs-directory "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))


;; Custom options
;; Automatically reload a file if it changes on disk
(global-auto-revert-mode t)
(setq confirm-kill-emacs nil)

(column-number-mode 1)
(global-hl-line-mode +1)
(setq inhibit-splash-screen t)
(electric-pair-mode 1)

(use-package auto-fill
  :straight nil
  :custom
  (fill-column 80)
  :hook
  (org-mode . auto-fill-mode))
