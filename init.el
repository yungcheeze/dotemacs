;;; Early init support for older Emacs

(unless (featurep 'early-init)
  (load (expand-file-name "early-init" user-emacs-directory)))

(require 'straight)
(straight-use-package 'use-package)
(require 'use-package)

;; Packages
(use-package straight
  )

(use-package benchmark-init
  :straight t
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package xclip
  :straight (xclip :type git :host github :repo "emacsmirror/xclip")
  :config
  (xclip-mode 1))

;;; Visuals
(use-package doom-modeline
  :straight (:type git :host github :repo "seagle0128/doom-modeline")
  :ensure t
  :custom
  (doom-modeline-env-version nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-encoding t)
  (mode-line-percent-position nil)
  :init
  (doom-modeline-mode 1))

(use-package nord-theme
  :straight t
  :custom-face
  (hl-line ((t (:background "#313845"))))
  (highlight ((t (:background "#353C4A"))))
  (mode-line ((t (:background "#3B4252"))))
  :config
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))

  (add-hook 'window-setup-hook 'on-after-init)
  (load-theme 'nord t))

(use-package vline
  :straight t
  :custom
  (vline-use-timer nil)
  :custom-face
  (vline ((t (:background "#313845")))))

(use-package rainbow-delimiters
  :straight t
  :defer t)
(use-package highlight-parentheses
  :straight t
  :defer t
  :hook
  (prog-mode . highlight-parentheses-mode))
(use-package highlight-indent-guides
  :straight t
  :hook
  (yaml-mode . highlight-indent-guides-mode))
(use-package rainbow-mode
  :straight t
  :defer t)
(use-package all-the-icons
  :straight t
  :defer t)

(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

(use-package combobulate
  :straight t
  :bind
  (:map combobulate-key-map
	("M-." . combobulate-mark-node-dwim)
	("M-N" . nil)
	("M-P" . nil)
	("M-<up>" . combobulate-drag-up)
	("M-<down>" . combobulate-drag-down))
  ;; TODO: add pretty hydra
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

(use-package general
  :straight t
  :ensure t
  :preface
  (global-unset-key (kbd "M-SPC"))
  (define-prefix-command 'cheesemacs-map)
  (global-set-key (kbd "M-SPC") 'cheesemacs-map)
  (general-create-definer cheesemacs
    :prefix-command 'cheesemacs-map)
  (define-prefix-command 'cheesemacs/buffers-map)
  (global-set-key (kbd "M-B") 'cheesemacs/buffers-map)
  (general-create-definer cheesemacs/buffers
    :prefix-command 'cheesemacs/buffers-map)
  (cheesemacs "b" cheesemacs/buffers-map)
  (define-prefix-command 'cheesemacs/files-map)
  (global-set-key (kbd "M-F") 'cheesemacs/files-map)
  (general-create-definer cheesemacs/files
    :prefix-command 'cheesemacs/files-map)
  (cheesemacs "f" cheesemacs/files-map)
  (define-prefix-command 'cheesemacs/git-map)
  (global-set-key (kbd "M-G") 'cheesemacs/git-map)
  (general-create-definer cheesemacs/git
    :prefix-command 'cheesemacs/git-map)
  (cheesemacs "g" 'cheesemacs/git-map)
  (general-create-definer cheesemacs/jump
    :wrapping cheesemacs
    :infix "j")
  (define-prefix-command 'cheesemacs/lsp-map)
  (global-set-key (kbd "M-L") 'cheesemacs/lsp-map)
  (general-create-definer cheesemacs/lsp
    :prefix-command 'cheesemacs/lsp-map)
  (cheesemacs "l" cheesemacs/lsp-map)
  (general-create-definer cheesemacs/narrow
    :wrapping cheesemacs
    :infix "n")
  (general-create-definer cheesemacs/org
    :wrapping cheesemacs
    :infix "o")
  (define-prefix-command 'cheesemacs/project-map)
  (global-set-key (kbd "M-P") 'cheesemacs/project-map)
  (general-create-definer cheesemacs/project
    :prefix-command 'cheesemacs/project-map)
  (cheesemacs "p" cheesemacs/project-map)
  (general-create-definer cheesemacs/quit
    :wrapping cheesemacs
    :infix "q")
  (general-create-definer cheesemacs/search
    :wrapping cheesemacs
    :infix "s")
  (define-prefix-command 'cheesemacs/toggle-map)
  (global-set-key (kbd "M-T") 'cheesemacs/toggle-map)
  (general-create-definer cheesemacs/toggle
    :prefix-command 'cheesemacs/toggle-map)
  (general-create-definer cheesemacs/text
    :wrapping cheesemacs
    :infix "x")
  (define-prefix-command 'cheesemacs/windows-map)
  (global-set-key (kbd "M-W") 'cheesemacs/windows-map)
  (general-create-definer cheesemacs/windows
    :prefix-command 'cheesemacs/windows-map)
  (cheesemacs "w" 'cheesemacs/windows-map)

  (cheesemacs/buffers "k" 'kill-buffer)
  (cheesemacs/narrow "w" 'widen)
  (cheesemacs/narrow "r" 'narrow-to-region)
  (cheesemacs/narrow "d" 'narrow-to-defun)
  (cheesemacs/toggle "l" 'toggle-truncate-lines)
  (cheesemacs/windows "c" 'delete-window)
  (cheesemacs/windows "x" 'delete-window)
  (cheesemacs/windows "O" 'ace-window)
  (cheesemacs/windows "o" 'delete-other-windows)
  (cheesemacs/windows "s" 'split-window-below)
  (cheesemacs/windows "/" 'split-window-right)
  (cheesemacs/windows "v" 'split-window-right)
  (cheesemacs/quit "q" 'save-buffers-kill-emacs)
  (cheesemacs/text "c" 'capitalize-word)
  :config
  (general-auto-unbind-keys))

(use-package hydra
  :straight t)
(use-package pretty-hydra
  :straight t)
(use-package major-mode-hydra
  :straight t)

(use-package breadcrumb
  :straight (breadcrumb :type git :host github :repo "joaotavora/breadcrumb")
  :defer t
  :init
  (cheesemacs/toggle "b" 'breadcrumb-local-mode))

(use-package xterm-color
  :straight t
  :defer t
  :init
  (cheesemacs/buffers "x c" 'xterm-color-colorize-buffer)
  (cheesemacs/toggle "a" 'xterm-color-colorize-buffer))

(use-package ansi
  :straight t)

;;; Helm 2.0
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  :config
  (vertico-mode +1)
  (savehist-mode +1))

(use-package vertico-repeat
  :straight nil
  :after vertico
  :ensure nil
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (cheesemacs "SPC" 'vertico-repeat))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :straight t
  :ensure t

  :bind
  (("M-'" . embark-act)         ;; pick some comfortable binding
   ("M-@" . embark-dwim)        ;; good alternative: M-.
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
  :straight t
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :straight t
  :ensure t
  :init
  (cheesemacs/buffers "b" 'consult-buffer)
  (cheesemacs/jump "m" 'consult-mark)
  (cheesemacs/jump "i" 'consult-imenu)
  (cheesemacs/jump "I" 'consult-imenu-multi)
  (cheesemacs/files "r" 'consult-recent-file)
  (cheesemacs/files "s" 'consult-find)
  (cheesemacs/files "f" 'find-file)
  (cheesemacs/project "b" 'consult-project-buffer)
  (cheesemacs "s" 'consult-ripgrep)
  :bind
  ("M-g m" . consult-mark)
  ("M-s" . consult-line)
  ("M-S" . consult-ripgrep))

(use-package imenu-list
  :straight t
  :bind
  ("M-g l" . imenu-list-smart-toggle)
  :init
  (cheesemacs/jump "l" 'imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t
	imenu-list-size 30))

(use-package origami
  :straight t
  :config
  (global-origami-mode))

(use-package rg
  :straight t
  :defer t
  :config
  (rg-enable-menu))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

;;; Git stuff
(use-package magit
  :straight t
  :defer t
  :init
  (cheesemacs/git "g" 'magit)
  (cheesemacs/git "d" 'magit-dispatch)
  (cheesemacs/git "f" 'magit-find-file)
  (cheesemacs/git "l" (lambda () (interactive) (call-interactively 'magit) (call-interactively 'magit-log)))
  (cheesemacs/git "o l" 'magit-list-submodules)
  (cheesemacs/git "B" 'magit-switch-to-repository-buffer)
  (cheesemacs/git "b" 'magit-blame)
  :custom
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :config
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  ;; magit buffer that respects current window configuration
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  (define-key magit-mode-map (kbd "M-q") 'magit-mode-bury-buffer))
(use-package magit-delta
  :straight t
  :custom
  (magit-delta-default-dark-theme "Nord")
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; https://shivjm.blog/better-magit-diffs/
  (defun cheeze/toggle-magit-delta ()
    (interactive)
    (magit-delta-mode
     (if magit-delta-mode
         -1
       1))
    (magit-refresh))

  (transient-append-suffix 'magit-diff '(-1 -1 -1)
    '("l" "Toggle magit-delta" cheeze/toggle-magit-delta)))
(use-package difftastic
  :straight (difftastic :type git :host github :repo "pkryger/difftastic.el")
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))
(use-package magit-todos
  :straight t
  :defer t
  :after magit
  :custom
  (magit-todos-ignored-keywords '("DONE"))
  (magit-todos-nice (when (executable-find "nice") t)
                    "avoid breaking Magit on systems that don't have `nice'.")
  (magit-todos-exclude-globs '("./git/" "node_modules/"))
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(use-package blamer
  :straight t
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
                   :italic t))))

(use-package git-gutter
  :straight t
  :config
  (setq git-gutter:update-interval 1)
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :straight t
  :defer t)

;; good for files
(use-package git-link
  :straight t
  :defer t
  :init
  (cheesemacs/git "k l" 'git-link)
  (cheesemacs/git "k k" 'git-link)
  :custom
  (git-link-use-commit t))
;; good for commit objects
(use-package browse-at-remote
  :straight t
  :init
  (cheesemacs/git "k b" 'browse-at-remote-kill)
  :defer t)

(use-package forge
  :straight t
  :after magit
  :defer t)

(use-package code-review
  :straight t
  :after forge
  :defer t
  :preface
  (cheesemacs/git "r" 'code-review-start)
  :init
  (transient-append-suffix 'forge-dispatch "v p" '("v r" "code-review" code-review-forge-pr-at-point))
  (define-key forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point))

;;;LSP

(use-package eglot
  :straight nil
  :defer t
  :commands (eglot eglot-ensure)
  :init
  (setq completion-category-overrides '((eglot (styles orderless))))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '(json-mode . ("vscode-json-languageserver" "--stdio")))
    (add-to-list 'eglot-server-programs
		 '(jsonc-mode . ("vscode-json-languageserver" "--stdio"))))
  :hook
  (python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (json-mode . eglot-ensure)
  (json-ts-mode . eglot-ensure)
  (jsonc-mode . eglot-ensure)
  (yaml-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  (shell-script-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (cc-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (haskell-mode . eglot-ensure)
  (haskel-literate-mode . eglot-ensure)
  (nix-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  :init
  (cheesemacs/lsp "e" 'consult-flymake)
  (cheesemacs/lsp "r" 'eglot-rename)
  :bind (:map eglot-mode-map
	      ("M-g g" . xref-find-definitions)
	      ("M-g d" . xref-find-definitions)
	      ("M-g r". xref-find-references)
	      ("M-g R". eglot-rename)
	      ("M-g M-e" . consult-flymake)
	      ("M-g M-s" . consult-imenu)
	      ("M-g i" . consult-imenu)
	      ("M-g s" . consult-eglot-symbols)
	      ("M-g ." . eldoc-print-current-symbol-info))
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(use-package consult-eglot
  :straight t
  :after eglot
  :defer t)

(use-package flycheck
  :straight t
  :defer t)

(use-package flymake-ruff
  :straight t)

(use-package eldoc
  :custom
  (eldoc-idle-delay 1.0))


(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el"))

  )

(use-package chatgpt-shell
  :requires shell-maker
  :custom
  (chatgpt-shell-openai-key (lambda ()
			      (auth-source-pick-first-password :host "api.openai.com")))
  (chatgpt-shell-model-version "gpt-4")
  (chatgpt-shell-model-temperature 0)
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))

;;;Completion
;; copilot dependencies
(use-package dash
  :straight t)
(use-package s
  :straight t)
(use-package editorconfig
  :straight t)
;; define copilot before corfu so keymap gets precedence
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :custom
  (copilot-idle-delay 0)
  :bind
  (:map prog-mode-map ("M-RET" . (lambda() (interactive) (copilot-complete) (cheese/hydra-copilot/body))))
  (:map copilot-completion-map
	("<right>" .  copilot-accept-completion))
  :pretty-hydra
  (cheese/hydra-copilot
   (:title "Copilot"
	   :quit-key "C-g")
   ("Complete"
    (("M-RET" copilot-accept-or-complete "accept-or-complete")
     ("<right>" copilot-accept-completion "accept" :exit t :color blue)
     ("<up>" copilot-accept-completion-by-word "accept by word")
     ("<down>" copilot-accept-completion-by-line "accept by line"))
    "Cycle"
    (("n" copilot-next-completion "next")
     ("M-n" copilot-next-completion "next")
     ("p" copilot-previous-completion "previous")
     ("M-p" copilot-previous-completion "previous"))
    "Special"
    (("RET" newline "newline")
     ("u" (lambda() (interactive) (undo-only) (copilot-complete)) "undo")
     ("M-u" (lambda() (interactive) (undo-only) (copilot-complete)) "undo")
     ("q" copilot-clear-overlay "quit" :color blue))))
  :config
  (defun copilot-accept-or-complete ()
    (interactive)
    (or (copilot-accept-completion)
	(copilot-complete)))
  :hook (prog-mode . copilot-mode))

(use-package corfu
  :straight ( :host github
              :repo "minad/corfu"
              :branch "main"
	      :files (:defaults "extensions/*")
	      :includes (corfu-history))
  :defer nil
  :bind ( :map corfu-map
          ("C-n" . corfu-next)
          ("C-p" . corfu-previous)
          ([remap completion-at-point] . corfu-complete)
          ("RET" . corfu-insert)
          ("<return>" . corfu-insert)
	  ("TAB" . nil)
	  ("<tab>" . nil)
	  ("M-TAB" . nil))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode 1))

(use-package popon
  :straight ( :type git
	      :host nil
              :repo "https://codeberg.org/akib/emacs-popon.git"
              :branch "master")
  :defer t
  :unless (display-graphic-p))

(use-package corfu-terminal
  :straight ( :type git
	      :host nil
              :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"
              :branch "master")
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))


(use-package company
  :straight t
  :custom
  (company-dabbrev-downcase nil))
(use-package company-shell
  :straight t
  )
(use-package company-cabal
  :straight t
  )
(use-package company-emoji
  :straight t
  )

(use-package cape
  :straight t
  :after corfu
  :hook ((nix-mode . (lambda ()
		       (setq-local completion-at-point-functions (list
								  (cape-capf-buster
								   (cape-super-capf
								    ;;(cape-company-to-capf #'company-nixos-options)
								    #'cape-file
								    #'cape-dabbrev))))
		       ))
	 (shell-script-mode . (lambda ()
				(setq-local completion-at-point-functions (list
									   (cape-capf-buster
									    (cape-super-capf
									     (cape-company-to-capf #'company-shell)
									     (cape-company-to-capf #'company-shell-env)
									     #'cape-file
									     #'cape-dabbrev))))
				))
	 (haskell-cabal-mode . (lambda ()
				 (setq-local completion-at-point-functions (list
									    (cape-capf-buster
									     (cape-super-capf
									      (cape-company-to-capf #'company-cabal)
									      #'cape-file
									      #'cape-dabbrev))))
				 ))
	 (text-mode . (lambda ()
			(setq-local completion-at-point-functions (list
								   (cape-capf-buster
								    (cape-super-capf
								     (cape-company-to-capf #'company-emoji)
								     #'cape-file
								     #'cape-dabbrev
								     #'cape-dict))))
			))
	 (forge-post-mode . (lambda ()
			      (setq-local completion-at-point-functions (list
									 (cape-capf-buster
									  (cape-super-capf
									   (cape-company-to-capf #'company-emoji)
									   #'cape-file
									   #'cape-dabbrev
									   #'cape-dict))))
			      ))
	 (emacs-lisp-mode . (lambda ()
			      (setq-local completion-at-point-functions (list
									 (cape-capf-buster
									  (cape-super-capf
									   #'cape-symbol
									   ;; (cape-company-to-capf #'company-elisp)
									   #'cape-file
									   #'cape-dabbrev))))
			      ))
	 )
  :config
  (setq completion-at-point-functions (list
                                       (cape-capf-buster
                                        (cape-super-capf
                                         #'cape-file
                                         #'cape-dabbrev))))
  )

(use-package yasnippet
  :straight t
  :bind
  (:map yas-minor-mode-map
	("M-i" . consult-yasnippet))
  :init
  (yas-global-mode t))
(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :defer t)
(use-package consult-yasnippet
  :straight t
  :after yasnippet
  :defer t)
(use-package auto-yasnippet
  :straight t
  :after yasnippet
  :custom
  (aya-case-fold t)
  :defer t)

(use-package autoinsert
  :straight t
  :init
  (setq auto-insert-directory (concat user-emacs-directory "templates"))
  (setq auto-insert-query nil)
  (add-hook 'find-file-hook 'auto-insert) (auto-insert-mode 1)
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  :config
  (define-auto-insert "\.hs$" ["default-haskell.hs" autoinsert-yas-expand]))


;;; Direnv
(use-package envrc
  :straight t
  :config
  (envrc-global-mode))
;; (use-package direnv
;;  :defer t
;;  :config
;;  (direnv-mode))

;;; Langs
(use-package nix-mode
  :straight t
  :defer t)
(use-package haskell-mode
  :straight t
  :defer t)
(use-package hlint-refactor
  :straight t
  :hook (haskell-mode . hlint-refactor-mode))
(use-package json-mode
  :straight t
  :custom
  (js-indent-level 2)
  :bind
  (:map js-mode-map
	("M-." . nil)) ;; M-. used for expand-region
  :defer t)
(use-package json-snatcher
  :straight t
  :defer t)
(use-package json-navigator
  :straight t
  :defer t)
(use-package tree-mode
  :straight t
  :defer t)
(use-package dockerfile-mode
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '(".*Dockerfile\\'" . dockerfile-mode)))
(use-package yaml-mode
  :straight t
  :defer t)
(use-package jenkinsfile-mode
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("jenkins/build\\'" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '(".*Jenkinsfile\\'" . jenkinsfile-mode)))
(use-package groovy-mode
  :straight t
  :defer t)
(use-package dotenv-mode
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))
(use-package typescript-mode
  :straight t
  :defer t)
(use-package csv-mode
  :straight t
  :defer t)
(use-package csharp-mode
  :straight t
  :defer t
  :custom
  (csharp-indent-offset 4)
  :config)
(use-package cc-mode
  :straight t
  :defer t)
(use-package cmake-mode
  :straight t
  :defer t)

(use-package python-black
  :straight t
  :defer t
  :after python)
(use-package pyimport
  :straight t
  :defer t)
(use-package py-isort
  :straight t
  :defer t
  :bind (:map python-mode-map
	      ("C-c i" . py-isort-buffer)))
(use-package pytest
  :straight t
  :defer t
  :init
  (defcustom pytest-remove-path ""
    "Path to remove from pytest test-names variable")
  :config
  (with-eval-after-load 'pytest
    (defun pytest-cmd-format (format-string working-directory test-runner command-flags test-names)
      "Override default function to remove local path."
      (format format-string working-directory test-runner command-flags (replace-regexp-in-string pytest-remove-path "" test-names)))
    ))
(use-package pip-requirements
  :straight t
  :defer t)
(use-package text-mode
  :defer t
  :straight nil
  :bind
  (:map text-mode-map
	;; unbind key used for copilot
	("M-TAB" . nil)))

(use-package format-all
  :straight t
  :defer t
  :custom
  (format-all-formatters '(("Shell" shfmt)))
  :init
  (cheesemacs/buffers "f" 'format-all-buffer)
  :config
  (define-format-all-formatter fourmolu
    (:executable "fourmolu")
    (:install "stack install fourmolu")
    (:languages "Haskell" "Literate Haskell")
    (:features)
    (:format
     (format-all--buffer-easy executable)))
  (define-format-all-formatter ormolu
    (:executable "ormolu")
    (:install "stack install ormolu")
    (:languages "Haskell" "Literate Haskell")
    (:features)
    (:format
     (format-all--buffer-easy executable)))
  (define-format-all-formatter shfmt
    (:executable "shfmt")
    (:install
     (macos "brew install shfmt")
     (windows "scoop install shfmt"))
    (:languages "Shell")
    (:features)
    (:format
     (format-all--buffer-easy
      executable
      "-i" (format "%d" sh-indentation)
      (if (buffer-file-name)
          (list "-filename" (buffer-file-name))
	(list "-ln" (cl-case (and (eql major-mode 'sh-mode)
                                  (boundp 'sh-shell)
                                  (symbol-value 'sh-shell))
                      (bash "bash")
                      (mksh "mksh")
                      (t "posix"))))))))

(use-package editorconfig
  :straight t
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package ethan-wspace
  :straight t
  :custom
  (mode-require-final-newline nil)
  :init
  (cheesemacs/buffers "w" 'ethan-wspace-clean-all)
  :config
  (global-ethan-wspace-mode 1))

(use-package separedit
  :straight t
  :defer t)

(use-package org
  :straight t
  :commands (org-store-link)
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\n  %i\n  %a")
     ))
  (org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "|" "DONE")))
  (org-export-backends '(ascii html icalendar latex odt md))
  (org-clock-persist 'history)
  :config
  (org-clock-persistence-insinuate)
  (defun cheese/markdown-to-org-region (start end)
    "Convert region from markdown to org."
    (interactive "r")
    (shell-command-on-region start end "pandoc -f markdown -t org" nil t)
    (fill-region start end))
  :bind
  (:map org-mode-map
	("C-c RET" . org-insert-heading-respect-content)
	("C-t" . org-todo)
	("M-g M-s" . consult-imenu)
	("M-g i" . consult-imenu)
	("M-g f" . org-fold-hide-sublevels))
  :init
  ;; setting these here so my org-capture- functions work when org hasn't loaded
  (setq-default org-directory "~/work/notes")
  (setq-default org-default-notes-file (concat org-directory "/NOTES.org"))
  (setq-default org-agenda-files `(,(concat org-directory "/NOTES.org")))

  (defun cheese/org-store-link-raw ()
    (interactive)
    (setq current-prefix-arg '(16))
    (call-interactively 'org-store-link))
  (cheesemacs/project "L" 'org-store-link)
  (cheesemacs/project "l" 'cheese/org-store-link-raw)

  (defun org-capture-jump ()
    "Jump to last captured entry"
    (interactive)
    (setq current-prefix-arg '(16))
    (call-interactively 'org-capture))
  (defun org-capture-open ()
    "Jump to last captured entry"
    (interactive)
    (find-file (expand-file-name org-default-notes-file)))

  (cheesemacs/org "j" 'org-capture-jump)
  (cheesemacs/org "o" 'org-capture-open)
  (cheesemacs/org "c" 'org-capture)
  (cheesemacs/org "l" 'cheese/org-store-link-raw))

(use-package ox-jira
  :straight t)

(use-package org-super-agenda
  :straight t
  :hook
  (org-mode . org-super-agenda-mode)
  :config
  (setq org-agenda-custom-commands
	'(("a" "Main Agenda" todo ""
           ((org-super-agenda-groups
             `((:auto-outline-path t)
	       ))))))
  )
(use-package org-contrib
  :straight t)
(use-package ol-git-link
  :straight t
  :after org)
(use-package orgit
  :straight t
  :after org
  :commands (orgit-store-link))
(use-package org-projectile
  :straight t
  :defer t
  :init
  (cheesemacs/project "c" 'org-projectile-capture-for-current-project)
  (cheesemacs/project "C" 'org-projectile-goto-location-for-project)
  :config
  (progn
    (setq org-projectile-projects-file
          "~/work/notes/PROJECTS.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

(use-package org-autolist
  :straight (:type git :host github :repo "ucizi-turintech/org-autolist")
  :defer t
  :hook (org-mode . org-autolist-mode))

(use-package org-modern
  :straight t
  :defer t
  :custom
  (org-modern-table nil)
  (org-modern-list '((?+ . "◦")
		     (?- . "-")
		     (?* . "•")))
  (org-modern-horizontal-rule nil)
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "✳✳" "✳✳✳" "✳✳✳✳" "✳✳✳✳✳" "✳✳✳✳✳✳"))

  :hook (org-mode . org-modern-mode))

;;; QOL
(use-package phi-search
  :straight t
  :defer t)
(use-package multiple-cursors
  :straight (multiple-cursors :type git :host github :repo "magnars/multiple-cursors.el")
  :defer t
  :commands (mc/split-region)
  :bind
  (("M-c" . cheese/hydra-mc/body)
   (:map mc/keymap
	 ("<return>" . nil)
	 ("C-s" . phi-search)
	 ("C-r" . phi-search-backward)
	 ("C-&" . mc/vertical-align-with-space)))
  :config
  (defun mc/split-region (beg end search)
    "Split region each time SEARCH occurs between BEG and END.

This can be thought of as an inverse to `mc/mark-all-in-region'."
    (interactive "r\nsSplit on: ")
    (let ((case-fold-search nil))
      (if (string= search "")
          (user-error "Empty search term")
	(progn
          (mc/remove-fake-cursors)
          (goto-char beg)
          (push-mark beg)
          (while (search-forward search end t)
            (save-excursion
              (goto-char (match-beginning 0))
              (mc/create-fake-cursor-at-point))
            (push-mark (match-end 0)))
          (unless (= (point) end)
            (goto-char end))
          (mc/maybe-multiple-cursors-mode)))))
  :pretty-hydra
  (cheese/hydra-mc
   (:quit-key "C-g" :title "Multiple Cursors")
   ("Prev/Next"
    (("n" mc/mark-next-like-this "next-like-this")
     ("N" mc/skip-to-next-like-this "skip-next-like-this")
     ("p" mc/mark-previous-like-this "previous-like-this")
     ("P" mc/skip-to-previous-like-this "skip-previous-like-this")
     ("M-n" mc/mark-next-symbol-like-this "next-symbol-this")
     ("M-p" mc/mark-previous-symbol-like-this "previous-symbol-this"))
    "Regions"
    (("A" mc/edit-beginnings-of-lines "edit-lines")
     ("E" mc/edit-ends-of-lines "edit-lines (ends)")
     ("s" mc/mark-all-in-region "filter")
     ("r" mc/mark-all-in-region-regexp "filter")
     ("S" mc/split-region "split"))
    "Buffer"
    (("bw" mc/mark-all-symbols-like-this "symbols-like-this")
     ("bb" mc/mark-all-like-this "like-this"))
    "Defun"
    (("dw" mc/mark-all-symbols-like-this-in-defun "symbols-like-this")
     ("dd" mc/mark-all-like-this-in-defun "like-this"))
    "Special"
    (("&" mc/vertical-align-with-space "align")
     ("q" nil "quit" :color blue)
     ("RET" nil "quit" :color blue)))))

(use-package iedit
  :straight t
  :defer t)

(use-package expand-region
  :straight t
  :defer t
  :bind ("M-." . er/expand-region))

(use-package swap-regions
  :straight t
  :init
  (defun swap-regions--exit ()
    (interactive)
    (exit-recursive-edit))
  (defun swap-regions--abort ()
    (interactive)
    (abort-recursive-edit))
  :config
  (defun cheese/swap-regions ()
    (interactive)
    (if (eq (recursion-depth) 0)
	(call-interactively 'swap-regions)
      (call-interactively 'swap-regions--exit)))
  (cheesemacs "x" 'cheese/swap-regions)
  :bind
  ("M-g x" . cheese/swap-regions))

(use-package ialign
  :straight t
  :ensure t
  :defer t)

(use-package string-inflection
  :straight t)

(use-package goto-chg
  :straight t)

(use-package recentf
  :straight t
  :straight nil
  :custom
  (recentf-max-menu-items 100)
  :config
  (recentf-mode))


(use-package undo
  :straight nil
  :bind
  ("M-u" . undo-only)
  ("M-r" . undo-redo))

(use-package vundo
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :bind
  ("C-x u" . vundo))

(use-package gcmh
  :straight t
  :delight gcmh-mode
  :init
  (gcmh-mode t))

(use-package isearch
  :straight nil
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
	  ("C-j" . avy-isearch)
	  ("C-w" . (lambda ()
		     "Reset current isearch to a word-mode search of the word under point."
		     (interactive)
		     (setq isearch-word t
			   isearch-string ""
			   isearch-message "")
		     (isearch-yank-string (word-at-point))))
	  ("M-w" . (lambda ()
		     "Reset current isearch to a word-mode search of the word under point."
		     (interactive)
		     (setq isearch-word t
			   isearch-string ""
			   isearch-message "")
		     (isearch-yank-string (buffer-substring (region-beginning) (region-end)))
		     (deactivate-mark)))
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char)))
(use-package avy
  :straight t
  :defer t
  :custom
  (avy-timeout-seconds 1)
  :bind
  (("M-j" . avy-goto-char-timer)
   ("M-l" . avy-goto-line)))
(use-package ace-window
  :straight t
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l ?g ?h))
  :defer t)

(use-package evil-nerd-commenter
  :straight t
  :defer t
  :bind ("M-g c" . evilnc-comment-or-uncomment-lines))

(use-package golden-ratio-scroll-screen
  :straight t
  :ensure t
  :custom
  (golden-ratio-scroll-highlight-flag 'before)
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package perfect-margin
  :straight t
  :defer t)

(use-package projectile
  :straight t
  :ensure t
  :init
  (cheesemacs/project "f" 'project-find-file)
  (cheesemacs/project "P" 'projectile-switch-project)
  (cheesemacs/project "k" 'projectile-compile-project)
  (cheesemacs/project "t" 'projectile-test-project)
  (cheesemacs/buffers "o" 'projectile-toggle-between-implementation-and-test)
  (cheesemacs/buffers "a" 'ff-find-other-fileg)
  (projectile-mode +1)
  :config
  (projectile-register-project-type 'haskell-hpack '("package.yaml")
                                    :project-file "package.yaml"
                                    :compile "hpack . && cabal build"
                                    :test "hpack . && cabal test --test-show-details=direct"
                                    :test-suffix "Spec"))
(use-package consult-projectile
  :straight t
  :after projectile
  :defer t)
(use-package flycheck-projectile
  :straight t
  :after projectile
  :defer t)
(use-package hl-todo
  :straight t
  :init
  (global-hl-todo-mode 1))

(use-package perspective
  :straight t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-suppress-no-prefix-key-warning t)
  :pretty-hydra
  (cheese/hydra-persp
   (:quit-key "C-g" :title "Perspective")
   ("Prev/Next"
    (("TAB" persp-next "next")
     ("n" persp-next "next")
     ("p" persp-prev "previous"))
    "Switch"
    (("o" persp-switch "switch" :exit t))
    "Special"
    (("q" nil "quit" :color blue)
     ("RET" nil "quit" :color blue))))
  :init
  (cheesemacs/project "TAB" 'cheese/hydra-persp/persp-next)
  (cheesemacs/project "n" 'cheese/hydra-persp/persp-next)
  (cheesemacs/project "o" 'cheese/hydra-persp/persp-switch-and-exit)
  (persp-mode))

(use-package persp-projectile
  :straight t
  :after perspective
  :init
  (cheesemacs/project "p" 'projectile-persp-switch-project))

(use-package dired-subtree
  :straight t
  :bind (:map dired-mode-map
	      ("i" . dired-subtree-cycle)
	      ("I" . dired-subtree-remove)
	      ("M-n" . dired-subtree-next-sibling)
	      ("M-p" . dired-subtree-previous-sibling)
	      ("M-u" . dired-subtree-up)))

(use-package dired-sidebar
  :straight t
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'nerd)
  :init
  (cheesemacs "TAB" #'dired-sidebar-toggle-sidebar))

(use-package ranger
  :straight t
  :defer t)

(use-package scratch
  :straight t
  :defer t
  :init
  (defun scratch-with-mode ()
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'scratch)
    (setq ))
  (cheesemacs/buffers "s" 'scratch)
  (cheesemacs/buffers "S" 'scratch-with-mode))

(use-package popper
  :straight t
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (cheesemacs/windows "p p" 'popper-toggle-latest)
  (cheesemacs/windows "p n" 'popper-cycle)
  (cheesemacs/windows "p t" 'popper-toggle-type)

  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "\\*xref\\*"
	  "\\*Embark Collect.*\\*"
	  "\\*Embark Export.*\\*"
	  "\\*Flymake diagnostics.*\\*"
	  "\\*eldoc\\*"
	  "\\*eshell\\*"
	  "\\*format-all-errors\\*"
          help-mode
	  helpful-mode
          compilation-mode))

  (popper-mode +1)
  (popper-echo-mode +1))

(use-package winner
  :straight t
  :straight nil
  :init
  (cheesemacs/windows "u" 'cheese/hydra-winner/winner-undo)
  (cheesemacs/windows "r" 'cheese/hydra-winner/winner-redo)
  :pretty-hydra
  (cheese/hydra-winner
   (:quit-key "C-g" :title "Winner")
   ("Undo/Redo"
    (
     ("u" winner-undo "undo")
     ("r" winner-redo "redo"))
    "Special"
    (("q" nil "quit" :color blue)
     ("RET" nil "quit" :color blue))
    ))
  :config
  (winner-mode))

(use-package helpful
  :straight t
  :defer t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(use-package browse-url
  :straight t
  :custom
  (browse-url-browser-function #'browse-url-chrome)
  (browse-url-chrome-program  "brave"))

;;; Custom Binds

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-M-o") 'ace-delete-window)
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

(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))

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

;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

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

(xterm-mouse-mode 1)
(setq mouse-wheel-scroll-amount '(3
				  ((shift)
				   . hscroll)
				  ((meta))
				  ((control)
				   . text-scale)))
(column-number-mode 1)
(global-hl-line-mode +1)
(setq show-trailing-whitespace 1)
(setq inhibit-splash-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")
(delete-selection-mode 1)
(setq read-minibuffer-restore-windows nil)

(use-package ediff
  :straight t
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

(use-package auto-fill
  :straight nil
  :custom
  (fill-column 80)
  :init
  (defun xah-fill-or-unfill ()
    "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or “unfill”.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
URL `http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
    (interactive)
    ;; This command symbol has a property “'compact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
    (let ( ($compact-p
            (if (eq last-command this-command)
		(get this-command 'compact-p)
              (> (- (line-end-position) (line-beginning-position)) fill-column)))
           (deactivate-mark nil)
           ($blanks-regex "\n[ \t]*\n")
           $p1 $p2
           )
      (if (use-region-p)
          (progn (setq $p1 (region-beginning))
		 (setq $p2 (region-end)))
	(save-excursion
          (if (re-search-backward $blanks-regex nil "NOERROR")
              (progn (re-search-forward $blanks-regex)
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward $blanks-regex nil "NOERROR")
              (progn (re-search-backward $blanks-regex)
                     (setq $p2 (point)))
            (setq $p2 (point)))))
      (if $compact-p
          (fill-region $p1 $p2)
	(let ((fill-column most-positive-fixnum ))
          (fill-region $p1 $p2)))
      (put this-command 'compact-p (not $compact-p))))
  :hook
  (org-mode . auto-fill-mode))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package electric-pairs
  :straight nil
  :init
  (defvar org-electric-pairs '() "Electric pairs for org-mode.")
  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  :defer nil ;; needed because of :hook other :config won't run on startup
  :config
  (electric-pair-mode 1)
  (push '(?\` . ?\`) electric-pair-pairs)
  :hook
  (org-mode . org-add-electric-pairs))
