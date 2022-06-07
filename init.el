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
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-env-version nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-encoding t)
  (mode-line-percent-position nil)
  :init
  (doom-modeline-mode 1))

(use-package nord-theme
  :custom-face
  (hl-line ((t (:background "#313845"))))
  (tree-sitter-hl-face:method.call          ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.call        ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:operator             ((t (:inherit default))))
  (tree-sitter-hl-face:type.builtin         ((t (:inherit font-lock-keyword-face))))
  (tree-sitter-hl-face:number               ((t (:inherit highlight-numbers-number))))
  (tree-sitter-hl-face:variable.special     ((t (:inherit font-lock-keyword-face))))
  :config
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))

  (add-hook 'window-setup-hook 'on-after-init)
  (load-theme 'nord t))

(use-package rainbow-delimiters
  :defer t)
(use-package highlight-parentheses
  :defer t
  :hook
  (prog-mode . highlight-parentheses-mode))
(use-package all-the-icons
  :defer t)

(use-package tree-sitter
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package general
  :ensure t
  :preface
  (global-unset-key (kbd "M-SPC"))
  (define-prefix-command 'cheesemacs-map)
  (global-set-key (kbd "M-SPC") 'cheesemacs-map)
  (general-create-definer cheesemacs
    :prefix-command 'cheesemacs-map)
  (general-create-definer cheesemacs/buffers
    :wrapping cheesemacs
    :infix "b")
  (general-create-definer cheesemacs/files
    :wrapping cheesemacs
    :infix "f")
  (general-create-definer cheesemacs/git
    :wrapping cheesemacs
    :infix "g")
  (general-create-definer cheesemacs/jump
    :wrapping cheesemacs
    :infix "j")
  (general-create-definer cheesemacs/narrow
    :wrapping cheesemacs
    :infix "n")
  (general-create-definer cheesemacs/org
    :wrapping cheesemacs
    :infix "o")
  (general-create-definer cheesemacs/project
    :wrapping cheesemacs
    :infix "p")
  (general-create-definer cheesemacs/quit
    :wrapping cheesemacs
    :infix "q")
  (general-create-definer cheesemacs/search
    :wrapping cheesemacs
    :infix "s")
  (general-create-definer cheesemacs/toggle
    :wrapping cheesemacs
    :infix "t")
  (general-create-definer cheesemacs/text
    :wrapping cheesemacs
    :infix "x")
  (general-create-definer cheesemacs/windows
    :wrapping cheesemacs
    :infix "w")

  (cheesemacs/buffers "k" 'kill-buffer)
  (cheesemacs/buffers "w" 'save-buffer)
  (cheesemacs/narrow "w" 'widen)
  (cheesemacs/narrow "r" 'narrow-to-region)
  (cheesemacs/narrow "d" 'narrow-to-defun)
  (cheesemacs/windows "c" 'delete-window)
  (cheesemacs/windows "O" 'delete-other-windows)
  (cheesemacs/windows "o" 'other-window)
  (cheesemacs/windows "s" 'split-window-below)
  (cheesemacs/windows "/" 'split-window-right)
  (cheesemacs/windows "1" 'delete-other-windows)
  (cheesemacs/windows "2" 'split-window-below)
  (cheesemacs/windows "3" 'split-window-right)
  (cheesemacs/windows "0" 'delete-window)
  (cheesemacs/windows "h" 'windmove-left)
  (cheesemacs/windows "j" 'windmove-down)
  (cheesemacs/windows "k" 'windmove-up)
  (cheesemacs/windows "l" 'windmove-right)
  (cheesemacs/quit "q" 'save-buffers-kill-emacs)

  :config
  (general-auto-unbind-keys))

(use-package hydra)
(use-package pretty-hydra)
(use-package major-mode-hydra)

(use-package xterm-color
  :defer t
  :init
  (cheesemacs/buffers "x c" 'xterm-color-colorize-buffer)
  (cheesemacs/toggle "a" 'xterm-color-colorize-buffer))

(use-package ansi)

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
  (cheesemacs/project "b" 'consult-project-buffer)
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
  (cheesemacs/git "d" 'magit-dispatch)
  (cheesemacs/git "f" 'magit-find-file)
  (cheesemacs/git "o l" 'magit-list-submodules)
  (cheesemacs/git "B" 'magit-switch-to-repository-buffer)
  (cheesemacs/git "b" 'magit-blame)
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
                   :italic t))))

(use-package git-gutter
  :config
  (setq git-gutter:update-interval 1)
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :defer t)

;; good for files
(use-package git-link
  :defer t
  :init
  (cheesemacs/git "k l" 'git-link)
  (cheesemacs/git "k k" 'git-link)
  :custom
  (git-link-use-commit t))
;; good for commit objects
(use-package browse-at-remote
  :init
  (cheesemacs/git "k b" 'browse-at-remote-kill)
  :defer t)

(use-package forge
  :after magit
  :defer t)

(use-package code-review
  :after forge
  :defer t
  :preface
  (cheesemacs/git "r" 'code-review-start)
  :init
  (transient-append-suffix 'forge-dispatch "v p" '("v r" "code-review" code-review-forge-pr-at-point))
  (define-key forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point))

;;;LSP
(use-package lsp-mode
  :defer t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (json-mode . lsp)
	 (jsonc-mode . lsp)
	 (yaml-mode . lsp))
  :custom
  (lsp-keymap-prefix "M-SPC l")
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

;; Need my own version vscode-languageserver.
;; lsp-mode's json-ls doesn't allow me to set a custom executable path and
;; tries to install it using npm (which I don't always have)
(use-package lsp-json-nix
  :straight nil
  :defer t
  :requires (lsp-mode)
  :preface
  (require 'lsp-mode) ;; defines lsp- symbols
  (require 'lsp-json) ;; defines lsp-json-- symbols
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection
     ;; this is the main thing I changed
     (lambda () (list "vscode-json-languageserver" "--stdio")))
    :activation-fn (lsp-activate-on "json" "jsonc")
    :server-id 'json-ls-nix
    :priority 1 ;; set higher priority than json-ls (currently 0)
    :multi-root t
    :completion-in-comments? t
    :initialization-options lsp-json--extra-init-params
    :async-request-handlers (ht ("vscode/content" #'lsp-json--get-content))
    :initialized-fn
    (lambda (w)
      (with-lsp-workspace w
	(lsp--set-configuration
	 (ht-merge (lsp-configuration-section "json")
		   (lsp-configuration-section "http")))
	(lsp-notify "json/schemaAssociations" lsp-json--schema-associations)))))
  (provide 'lsp-json-nix))

;;; Debugging
(use-package dap-mode
  :defer t
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))
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
  :bind
  ("M-i" . consult-yasnippet)
  :init
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
  :defer t
  :init
  (add-to-list 'auto-mode-alist '(".*Dockerfile\\'" . dockerfile-mode)))
(use-package yaml-mode
  :defer t)
(use-package jenkinsfile-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("jenkins/build\\'" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '(".*Jenkinsfile\\'" . jenkinsfile-mode)))
(use-package groovy-mode
  :defer t)
(use-package dotenv-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package python-black
  :defer t
  :after python)
(use-package pyimport
  :defer t)
(use-package py-isort
  :defer t)
(use-package pytest
  :defer t)
(use-package pip-requirements
  :defer t)

(use-package format-all
  :defer t
  :init
  (cheesemacs/buffers "f" 'format-all-buffer))

(use-package separedit
  :defer t)

(use-package org
  :commands (org-store-link)
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\n  %i\n  %a")
     ))
  (org-todo-keywords '((sequence "TODO" "BLOCKED" "|" "DONE")))
  :bind
  (:map org-mode-map
	("C-c RET" . org-insert-heading-respect-content))
  :init
  ;; setting these here so my org-capture- functions work when org hasn't loaded
  (setq-default org-directory "~/work/notes")
  (setq-default org-default-notes-file (concat org-directory "/NOTES.org"))

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

(use-package org-contrib
  :defer t)
(use-package ol-git-link
  :after org)
(use-package orgit
  :after org
  :commands (orgit-store-link))
(use-package org-projectile
  :defer t
  :init
  (cheesemacs/project "c" 'org-projectile-capture-for-current-project)
  (cheesemacs/project "n" 'org-projectile-goto-location-for-project)
  (cheesemacs/project "o" 'org-projectile-goto-location-for-project)
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

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

;;; QOL
(use-package phi-search
  :defer t)
(use-package multiple-cursors
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
     ("q" nil "quit" :color blue)))))

(use-package iedit
  :defer t)

(use-package expand-region
  :defer t
  :bind ("M-." . er/expand-region))

(use-package ialign
  :ensure t
  :defer t)

(use-package auto-mark
  :straight
  (auto-mark :type git :host github :repo "yungcheeze/auto-mark.el")
  :custom
  (auto-mark-command-class-alist
   '((goto-line . jump)
     (indent-for-tab-command . ignore)
     (undo . ignore)))
  :config
  (defun auto-mark-set-mark ()
    (interactive)
    (push-mark (point) t nil))

  (defadvice consult-line (after set-mark activate) (auto-mark-set-mark))

  (global-auto-mark-mode 1))

(use-package recentf
  :straight nil
  :custom
  (recentf-max-menu-items 100)
  :config
  (recentf-mode))

(use-package undo-tree
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist
   `(("." . ,(let ((dir (expand-file-name "undo-tree-history" user-emacs-directory)))
               (if (file-exists-p dir)
                   (unless (file-accessible-directory-p dir)
                     (warn "Cannot access directory `%s'.
 Perhaps you don't have required permissions, or it's not a directory.
 See variable `undo-tree-history-directory-alist'." dir))
                 (make-directory dir))
               dir))))
  :config
  (global-undo-tree-mode))

(use-package gcmh
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
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l ?g ?h))
  :defer t)

(use-package evil-nerd-commenter
  :defer t
  :bind ("M-g c" . evilnc-comment-or-uncomment-lines))

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
  (cheesemacs/project "f" 'project-find-file)
  (cheesemacs/project "P" 'projectile-switch-project)
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

(use-package persp-projectile
  :after perspective
  :init
  (cheesemacs/project "p" 'projectile-persp-switch-project))

(use-package treemacs
  :defer t
  :init
  (cheesemacs "TAB" 'treemacs-display-current-project-exclusively)
  (cheesemacs/files "t" 'treemacs-display-current-project-exclusively)
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

(use-package popper
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
	  "\\*eshell\\*"
          help-mode
	  helpful-mode
          compilation-mode))

  (popper-mode +1)
  (popper-echo-mode +1))

(use-package helpful
  :defer t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(use-package browse-url
  :custom
  (browse-url-browser-function #'browse-url-chrome)
  (browse-url-chrome-program  "brave"))

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

(column-number-mode 1)
(global-hl-line-mode +1)
(setq inhibit-splash-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")
(electric-pair-mode 1)
(delete-selection-mode 1)
(setq read-minibuffer-restore-windows nil)

(use-package ediff
  :straight nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally))

(use-package auto-fill
  :straight nil
  :custom
  (fill-column 80)
  :hook
  (org-mode . auto-fill-mode))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
