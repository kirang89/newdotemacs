(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(defconst font "Basier Square Mono")
(defconst org-code-block-font "JetBrains Mono")
(defconst org-variable-pitch-font "SF Pro")

(set-face-attribute 'default nil
                    :family "Codelia"
                    :height 170
                    :weight 'normal
                    :width 'normal)

(use-package font-lock+)

(setq-default help-window-select t
              truncate-lines t
              fill-column 100)

(setq-default line-spacing 1)

;; Prevents from accidentally quitting Emacs
;; (global-unset-key (kbd "C-x C-c"))

;; workaround for alt not working as meta key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'super
      mac-option-modifier 'meta
      inhibit-splash-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      select-enable-clipboard t
      save-interprogram-paste-before-kill t
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      vc-handled-backends '(Git)
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8
      org-ellipsis " ▶")

;;(set-default 'cursor-type '(bar . 3))
(set-default 'cursor-type 'box)
(blink-cursor-mode nil)

(global-hl-line-mode -1)
(set-face-attribute hl-line-face nil :underline t)

(global-set-key (kbd "s-x") 'execute-extended-command)

;; Rewrite selected text
(delete-selection-mode 1)

;;Treat hyphens as a word character when transposing words
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'transpose-words-with-hyphens)

(use-package spacious-padding
  :hook (after-init . spacious-padding-mode)
  :config
  ;; (setq spacious-padding-widths
  ;;     '( :internal-border-width 15
  ;;        ;; :header-line-width 4
  ;;        ;; :mode-line-width 6
  ;;        ;; :tab-width 4
  ;;        ;; :right-divider-width 30
  ;;        ;; :scroll-bar-width 8
  ;;        :fringe-width 8))
  )

;; =========================================================
;;                          THEMES
;; =========================================================

(use-package monokai-pro-theme)

(use-package challenger-deep-theme)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-spacegrey t)
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

(use-package solarized-theme
  :disabled t
  :init
  (setq solarized-distinct-fringe-background nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-variable-pitch nil)
  (setq x-underline-at-descent-line t))

(use-package jetbrains-darcula-theme
  :straight (:host github :repo "ianpan870102/jetbrains-darcula-emacs-theme")
  :config
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  ;; (load-theme 'jetbrains-darcula t)
  )

(use-package tokyo-theme
  :straight (:host github :repo "rawleyfowler/tokyo-theme.el"))

(use-package ef-themes)

(use-package github-modern-theme)

;; Run hooks after loading a new theme
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(advice-add #'load-theme :after #'run-after-load-theme-hook)
(add-hook 'after-load-theme-hook #'kg/reset-ui)

;; =========================================================
;;                OTHER THIRD PARTY PACKAGES
;; =========================================================

(use-package expand-region
  :config (global-set-key (kbd "C-;") 'er/expand-region))

(use-package ivy
  :bind ("s-b". 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-count-format "%d/%d ")

  ;; Use [Enter] to navigate into the directory, not dired-open it.
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done))

(use-package rg
  :config
  (setq rg-command-line-flags '("-w"))
  (setq rg-ignore-case 'smart)

  (rg-define-search kg/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git")))

(use-package counsel
  :after rg
  :config
  (global-set-key (kbd "s-r") 'counsel-recentf)
  (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-cleanup (* 24 60 60))
  (use-package flx
    :init
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper))

(use-package org
  ;; TODO Add face configuration, maybe using the below as basis
  ;; https://lepisma.xyz/2017/10/28/ricing-org-mode/
  :config
  (setq org-directory "~/Box Sync/org-notes"
        org-default-notes-file (concat org-directory "/notes.txt")
        org-export-coding-system 'utf-8
        org-ellipsis " ▼ "
        org-startup-indented t
        org-pretty-entities t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-hide-emphasis-markers t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (add-hook 'org-mode-hook
            (lambda ()
              (auto-fill-mode 1)
              (org-indent-mode t)
              (custom-theme-set-faces
                 'user
                 '(variable-pitch ((t (:family org-variable-pitch-font :height 180 :weight thin))))
                 '(fixed-pitch ((t ( :family org-code-block-font :height 150))))
                 '(org-block ((t (:inherit fixed-pitch))))
                 '(org-code ((t (:inherit (shadow fixed-pitch)))))))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package olivetti
  :config
  (add-hook 'org-mode-hook (lambda () (olivetti-mode t))))

(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 1.1))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (set-face-attribute 'mode-line nil :family font :height 140)
  (set-face-attribute 'mode-line-inactive nil :family font :height 140)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 40
        doom-modeline-bar-width 2
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 50
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil
        doom-modeline-window-width-limit fill-column))

(use-package undo-tree
  :bind ("s-Z" . 'undo-tree-redo)
  :init
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history")))
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'gfm-mode-hook 'display-line-numbers-mode)
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

(use-package markdown-preview-mode
  :after markdown-mode)

(use-package smartparens
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  (add-hook 'elixir-mode-hook 'smartparens-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package yaml-mode)
(use-package json-mode)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package rust-mode
  :disabled t
  :config
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c SPC") #'rust-format-buffer)
              (local-set-key (kbd "C-c c") #'rust-compile)
              (local-set-key (kbd "C-c C-t") #'rust-test)
              (local-set-key (kbd "C-c C-r") #'rust-run)
              (setq company-minimum-prefix-length 1)))

  (use-package racer
    :init
    (setq racer-rust-src-path
          "/Users/kiran/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (define-key rust-mode-map (kbd "C-c C-d") #'racer-describe)
    (setq company-tooltip-align-annotations t
          racer-eldoc-timeout 0.5))

  (use-package cargo
    :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

  (use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

;; (use-package flycheck-clj-kondo
;;   :config
;;   (add-hook 'clojure-mode-hook #'flycheck-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4))

;; (use-package ivy-posframe
;;   :after ivy
;;   :diminish
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
;;         ;; ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
;;         ivy-posframe-height-alist '((t . 20))
;;         ivy-posframe-parameters '((internal-border-width . 10)))
;;   (setq ivy-posframe-width 70)
;;   (ivy-posframe-mode -1))

(use-package protobuf-mode
  :hook (protobuf-mode . flycheck-mode)
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))

  ;; Consider integrating buf using the snippet below
  ;; https://github.com/flycheck/flycheck/issues/1453#issuecomment-506598272
  )

(use-package dockerfile-mode
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package golden-ratio
  :disabled t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode -1))

(use-package which-key
  :config
  (which-key-mode))

(use-package elixir-mode
  :hook ((elixir-mode . (lambda ()
                          (local-set-key (kbd "s-l l") 'goto-line)
                          (local-set-key (kbd "C-c C-d") 'elixir-mode-open-docs-stable))))
  :custom
  (lsp-elixir-server-command '("/Users/kiran/3rdPartyDev/lexical-v0.2.2/start_lexical.sh")))

(use-package exunit
  :after elixir-mode
  :hook ((elixir-mode . exunit-mode))
  :bind (("C-c t t" . 'exunit-verify-single)
         ("C-c t f" . 'exunit-verify)
         ("C-c t a" . 'exunit-verify-all)))

;; IEx REPL
(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)))

(use-package flycheck
  :after elixir-mode
  :hook ((elixir-mode . flycheck-mode)))

(use-package flycheck-credo
  :after flycheck
  :config
  (eval-after-load 'flycheck '(flycheck-credo-setup))
  :custom
  (flycheck-elixir-credo-strict t))

(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :hook ((elixir-mode . lsp)
         ;; (ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (("C-c h" . 'lsp-describe-thing-at-point))
  :init
  (add-to-list 'exec-path "/Users/kiran/3rdPartyDev/elixir-ls-v0.15.0")
  :config
  (use-package lsp-ui
    :hook ((lsp-mode . lsp-ui-mode)
           (lsp-mode . (lambda ()
                         (add-hook 'before-save-hook #'lsp-format-buffer))))
    :custom
    (lsp-ui-doc-position 'top)
    (lsp-ui-doc-enable t)
    (lsp-ui-peek-enable t)
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-hover nil)
    (lsp-ui-imenu-enable nil)
    (lsp-ui-flycheck-enable t)
    (lsp-headerline-breadcrumb-mode nil)
    (lsp-modeline-diagnostics-enable t)
    (lsp-signature-render-documentation t)
    (lsp-elixir-suggest-specs t)))

(setq lsp-elixir-suggest-specs t
      lsp-ui-peek-enable t)

;; ++++++++++++
;; Experimental
;; ++++++++++++

(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; Omit magit from native compilation
(setq native-comp-deferred-compilation-deny-list '("magit"))

;; Use existing frame when opening files
(setq ns-pop-up-frames nil)

;; No need to keep duplicates in prompt history.
(setq history-delete-duplicates t)

;; Allow auto revert mode to update vc information
(setq auto-revert-check-vc-info t)

;; Set ctags binary
;; Run ctags -e -R . for indexing a project
(setq ctags-path "/opt/homebrew/bin/ctags")

(use-package smart-comment
  :bind ("M-;" . smart-comment))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :interpreter "ruby"
  :hook ((ruby-mode . smartparens-strict-mode))
  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)
  :config
  ;; (use-package robe
  ;;   :hook ((ruby-mode . robe-mode)
  ;;          (ruby-ts-mode-hook . robe-mode))
  ;;   :config
  ;;   (eval-after-load 'company
  ;;     '(push 'company-robe company-backends)))
  )

(use-package properties-mode
  :straight (:host github :repo "iquiw/properties-mode")
  :mode "\\.properties\\'")

(use-package kotlin-mode)

(use-package kanagawa-theme
  :disabled true
  :straight (:host github :repo "meritamen/emacs-kanagawa-theme"))

(use-package hungry-delete
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

;; Magit requires ‘transient’ >= 0.5.0,
;; but due to bad defaults, Emacs’ package manager, refuses to
;; upgrade this and other built-in packages to higher releases
;; from GNU Elpa.

;; The configuration below must be added to fix this:
;; (setq package-install-upgrade-built-in nil)

;;;; +++++++++++++++++++
;;;; LSP Experimentation
;;;; +++++++++++++++++++

;; (use-package go-mode
;;   :after exec-path-from-shell

;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;;   :config
;;   (exec-path-from-shell-copy-env "GOROOT")
;;   (exec-path-from-shell-copy-env "GOPATH")
;;   (add-hook 'go-mode-hook 'flycheck-mode)
;;   (add-hook 'before-save-hook 'gofmt-before-save))

;; (use-package yasnippet)

;; (use-package company-go
;;   :commands company-go
;;   :config
;;   (add-hook 'go-mode-hook
;;             (lambda ()
;;               (setq company-backends '(company-go))
;;               (company-mode-on))))

;; (use-package go-eldoc)
(use-package gotest)

(defun kg/go-mode-hook ()
  (if (executable-find "goimports")
      (setq gofmt-command "goimports"))

  ;; (go-eldoc-setup)
  (subword-mode 1)
  (smartparens-mode 1)
  (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-c") 'go-run))
  (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-t") 'go-test-current-project)))

(add-hook 'go-mode-hook 'kg/go-mode-hook)

;; --------- go-mode with LSP ------------

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-go-analyses '((unusedparams . t)
                          (unusedvariable . t))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable t
        ;; lsp-ui-imenu-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-doc-delay 1
        lsp-enable-snippet nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package company-lsp :commands company-lsp)

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :after exec-path-from-shell

  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  :config
  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "GOPATH"))


;; (defun go-mode-setup ()
;;   (go-eldoc-setup)
;;   (setq gofmt-command "goimports")
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   (local-set-key (kbd "M-.") 'godef-jump)
;;   (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
;;   (setq compilation-read-command nil)
;;   (subword-mode 1)
;;   (smartparens-mode 1)
;;   (add-hook 'go-mode-hook 'flycheck-mode)
;;   (local-set-key (kbd "C-c C-c") 'compile))

;; (use-package eldoc-overlay
;;   :init (eldoc-overlay-mode -1))

;; (use-package avy
;;   :config
;;   ;; (global-set-key (kbd "s-f") 'avy-goto-char-timer)
;;   )

;; (use-package git-gutter
;;   :diminish git-gutter-mode
;;   :config
;;   (global-git-gutter-mode 't)
;;   (setq git-gutter:added-sign "+")
;;   (setq git-gutter:deleted-sign "-")
;;   (setq git-gutter:modified-sign "  "))

;;;; +++++++++++++++++++
;;;; MISC
;;;; +++++++++++++++++++

(use-package prescient
  :config
  (use-package ivy-prescient :config (ivy-prescient-mode))
  (use-package company-prescient :config (company-prescient-mode)))

(setq load-path (cons "/Users/kiran/.asdf/installs/erlang/26.0.1/lib/tools-3.6/emacs/" load-path))
(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(setq initial-major-mode 'org-mode
      initial-scratch-message nil)

(use-package git-timemachine)

(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t)
  (global-set-key (kbd "s-t") 'find-file-in-project))

;; (use-package jenkinsfile-mode)

(use-package org-present
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))
;; =========================================================
;;                           EFUNS
;; =========================================================

(defun kg/iex ()
  (interactive)
  (term "iex"))

;; (defun load-local (file)
;;   (load (f-expand file user-emacs-directory)))

;; (load-local "init-efuns")

(load "/Users/kiran/.emacs.d/init-efuns.el")

;; =========================================================
;;                      GLOBAL KEYBINDINGS
;; =========================================================

(global-set-key (kbd "s-g") 'kg/search-marked-region-if-available)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 3") 'kg/split-right-and-move)
(global-set-key (kbd "C-x 2") 'kg/split-below-and-move)
(global-set-key (kbd "C-a") 'kg/beginning-of-line-dwim)
(global-set-key [(meta shift down)] 'kg/duplicate-start-of-line-or-region)
;; (global-set-key (kbd "C-c C-l") 'org-capture)
(global-set-key (kbd "<f6>") 'kg/show-user-config)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "<s-S-return>") 'kg/toggle-maximize-buffer)


;; =============== ADD THEME FOLDER =========================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")




;; (use-package go-mode
;;   :after exec-path-from-shell

;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;;   :config
;;   (exec-path-from-shell-copy-env "GOROOT")
;;   (exec-path-from-shell-copy-env "GOPATH")
;;   (add-hook 'go-mode-hook 'flycheck-mode)
;;   (add-hook 'before-save-hook 'gofmt-before-save))

;; (use-package company-go
;;   :commands company-go
;;   :config
;;   (add-hook 'go-mode-hook
;;             (lambda ()
;;               (setq company-backends '(company-go))
;;               (company-mode-on))))

;; (use-package go-eldoc)
;; (use-package gotest)

;; (defun kg/go-mode-hook ()
;;   (if (executable-find "goimports")
;;       (setq gofmt-command "goimports"))

;;   (go-eldoc-setup)
;;   (subword-mode 1)
;;   (smartparens-mode 1)
;;   (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-c") 'go-run))
;;   (add-hook 'go-mode-hook (local-set-key (kbd "C-c C-t") 'go-test-current-project)))

;; (add-hook 'go-mode-hook 'kg/go-mode-hook)
