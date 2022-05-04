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

;;; Helm 2.0 
(use-package selectrum
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
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("M-;" . embark-dwim)        ;; good alternative: M-.
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

(use-package which-key
  :config
  (which-key-mode))

;;; Git stuff
(use-package magit)

(use-package magit-delta
  :after magit
  :config
  (setq
    magit-delta-default-dark-theme "Nord"
    magit-delta-hide-plus-minus-markers nil)
  (magit-delta-mode))

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

(use-package git-timemachine)

(use-package forge
  :after magit)

(use-package code-review)

;;;LSP
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)
(use-package consult-lsp)
(use-package flycheck)
(use-package lsp-ui)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


;;;Completion
(use-package company
  :config
  (global-company-mode 1))
(use-package company-shell)
(use-package company-cabal)
(use-package company-nixos-options)


;;; Direnv
;; (use-package envrc
;;   :config
;;   (envrc-global-mode))
(use-package direnv
 :config
 (direnv-mode))

;;; Langs
(use-package nix-mode)

;;; QOL
(use-package multiple-cursors)

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

(use-package ace-window)

(use-package evil-nerd-commenter)

(use-package esup)
;;; Setup Shit
;; keep backups and auto-saves out of the way https://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "saves") t)))

;; Automatically reload a file if it changes on disk
(global-auto-revert-mode t)

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
