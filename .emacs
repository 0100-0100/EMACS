(setq user-full-name "Diego Lopez")

(setq inhibit-startup-message t)
;; (tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)

;; Set org evaluate to nil by default. ───────────────────────────────────── ;;
(setq org-confirm-babel-evaluate nil)

;; Enable downcase region command. ───────────────────────────────────────── ;;
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "M-<f2>") 'comment-or-uncomment-region)

;; Prevents creation of back-up ~files. ──────────────────────────────────── ;;
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Y/N instead of Yes/No. ────────────────────────────────────────────────── ;;
(defalias 'yes-or-no-p 'y-or-n-p)

;; Toggles truncation of long lines instead of wrapping. ─────────────────── ;;
(setq-default truncate-lines t)

;; Enables use of C-c to copy, C-x to cut... ─────────────────────────────── ;;
(cua-mode 1)

;; Remembers cursor's last position. ─────────────────────────────────────── ;;
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Highlights whitespaces and lines over 80 characters. ──────────────────── ;;
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))

;; UTF-8 as default encoding. ────────────────────────────────────────────── ;;
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(column-number-mode 1)
(electric-pair-mode 1)
(show-paren-mode t)
(setq-default indent-tabs-mode nil)

(xterm-mouse-mode)
(setq mouse-wheel-scroll-amount '(2))
(setq mouse-wheel-progressive-speed 5)

;; IMPORTANT Confirm how to show fringes
(setq-default left-fringe-width 10)

(setq-default css-indent-offset 2)

;; Package configuration. ─────────────────────────────────────────────────────
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package comment-tags
  :ensure t
  :init
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  :config
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#FF0000"))
          ("FIX" . ,(list :weight 'bold :foreground "#FF00FF"))
          ("BUG" . ,(list :weight 'bold :foreground "#FFFF00"))
          ("DONE" . ,(list :weight 'bold :foreground "#00FF00"))
          ("???" . ,(list :weight 'bold :foreground "#0000FF"))
          ))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon nil
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil)
  :hook (python-mode . comment-tags-mode))

(use-package crontab-mode
  :ensure t
  )

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :hook (
         (css-mode . company-mode)
         (css-mode . (lambda () (display-line-numbers-mode 0)))
         )
)

(use-package display-line-numbers
  :hook (after-init . global-display-line-numbers-mode)
  )

(use-package docker-compose-mode
  :ensure t
 )

(use-package dockerfile-mode
  :ensure t
  )

(use-package exec-path-from-shell
  :ensure t
  )

(use-package hideshowvis
  :ensure t
  :bind
  ("M-<f3>" . hs-toggle-hiding)
  ("C-<f3>" . hs-hide-all)
  :hook ((prog-mode . hideshowvis-enable)
         (org-mode . (lambda () (hideshowvis-enable -1))))
  )

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)  ;; Disable auto-setting faces
  (set-face-background 'highlight-indent-guides-odd-face "#1C1C1C")
  (set-face-background 'highlight-indent-guides-even-face "#242424")
  (set-face-foreground 'highlight-indent-guides-character-face "#333333"))

(use-package impatient-showdown
  :ensure t
  :hook (markdown-mode . impatient-showdown-mode)
  ;; :config
  ;; (setq impatient-showdown-flavor "github")
  )

(use-package ivy
  :ensure t
  :hook (prog-mode . ivy-mode)
  )

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2)
  )

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

(use-package kivy-mode
  :ensure t
  )

(use-package markdown-preview-mode
  :ensure t
  :mode "\\.md\\'"
  )

(use-package multiple-cursors
  :ensure t
  :bind
  ("<f9>" . 'mc/edit-lines)
  ("<C-f9>" . 'mc/edit-ends-of-lines)
  ("<M-f9>" . 'mc/mark-next-like-this-symbol)
  ;; ("C-x C-<mouse-1>" . 'mc/add-cursor-on-click)
  )

(use-package nginx-mode
  :ensure t
  )

(use-package org
  :no-require
  :config
  (setq org-babel-python-command "python3")
  (setq org-startup-with-inline-images t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))
(use-package pip-requirements
  :ensure t
  )

(use-package powershell
  :ensure t
  )

(use-package puppet-mode
  :ensure t
  )

(use-package typescript-mode
  :ensure t
  )

(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode))
  )

(use-package company
  :ensure t
  :hook (
         (emacs-lisp-mode . company-mode)
         (latex-mode . company-mode))
  :config (setq company-minimum-prefix-length 1) (setq company-idle-delay 0.3)
  )

(use-package company-auctex
  :ensure t
  )

(use-package lsp-mode
  :ensure t
  :commands lsp-deferred
  :config
  (setq lsp-idle-delay 0.0
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil
        lsp-auto-configure t
        lsp-log-io nil
        lsp-keymap-prefix "C-c l"
        lsp-restart 'auto-restart
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t)
  :hook (
         (python-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  )

;; Set up garbage collection and performance-related settings
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      ;; treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

;; Configure `which-key` for better keybinding discovery
(use-package which-key
  :config
  (which-key-mode))

;; Configure `yasnippet` for code snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Configure `flycheck` for on-the-fly syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

;; Configure `php-mode` for editing PHP files
(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

;; ;; Optionally, configure `lsp-treemacs` for LSP tree visualizations
;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   :config
;;   (lsp-treemacs-sync-mode 1))

;; Python Config. ──  ──  ──  ──  ──  ──  ──  ──  ──  ──  ──  ──  ──  ──  ──  ─
(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  )

(use-package python-mode
  :ensure t
  :hook
  (python-mode . lsp-deferred)
  :custom (python-shell-interpreter "python3")
  :config (setq python-indent-offset 4)
  )

(use-package auctex
  :ensure t
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '(("django" . "\\.html\\'"))
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-block-padding 0)
  :commands web-mode
  )

(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(face empty trailing))
  (global-whitespace-mode 1))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#FFFFFF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(ansi-color-fast-blink ((t nil)))
 '(button ((t (:foreground "#D7AF00" :overline t :underline t))))
 '(company-preview ((t (:inherit (company-tooltip-selection company-tooltip) :background "#262626" :foreground "#D7AF00"))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "#262626" :foreground "#AF8700"))))
 '(company-tooltip-annotation ((t (:foreground "#FFD700"))))
 '(company-tooltip-common ((t (:foreground "#FFD700"))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#ffd700"))))
 '(company-tooltip-scrollbar-track ((t (:background "#303030"))))
 '(company-tooltip-selection ((t (:background "#303030"))))
 '(custom-button-pressed-unraised ((t (:inherit custom-button-pressed-unraised :foreground "#875F00"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "#AF8700" :weight bold :height 1.2))))
 '(custom-group-tag-1 ((t (:inherit variable-pitch :foreground "#875F00" :weight bold :height 1.2))))
 '(custom-state ((t (:foreground "#00FF5F"))))
 '(custom-variable-tag ((t (:foreground "#AF8700" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :background "#DDFFDD" :foreground "#008700"))))
 '(diff-context ((t (:foreground "#6C6C6C"))))
 '(diff-file-header ((t (:background "#1C1C1C" :foreground "#FFD700" :weight bold))))
 '(diff-function ((t (:foreground "#EEEEEE"))))
 '(diff-header ((t (:foreground "#FFD700"))))
 '(diff-hunk-header ((t (:inherit diff-header :foreground "#EEEEEE"))))
 '(diff-indicator-removed ((t (:inherit diff-removed :foreground "#FF0000"))))
 '(diff-removed ((t (:inherit diff-changed :background "#FFDDDD" :foreground "#FF0000"))))
 '(eieio-custom-slot-tag-face ((t (:foreground "#AF8700"))) t)
 '(error ((t (:foreground "#FF0000" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#B0C5DE"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#4E4E4E"))))
 '(font-lock-comment-face ((t (:foreground "#3A3A3A"))))
 '(font-lock-constant-face ((t (:foreground "#7FFFD4"))))
 '(font-lock-delimiter-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-function-name-face ((t (:foreground "#87CEFA"))))
 '(font-lock-keyword-face ((t (:foreground "#00FFFF"))))
 '(font-lock-negation-char-face ((t (:foreground "#FF0000"))))
 '(font-lock-operator-face ((t (:foreground "#87AFFF"))))
 '(font-lock-string-face ((t (:foreground "#FFA07A"))))
 '(font-lock-type-face ((t (:foreground "#98FB98"))))
 '(font-lock-variable-name-face ((t (:foreground "#FFD787"))))
 '(fringe ((t (:background "#767676" :inverse-video t))))
 '(highlight ((t (:foreground "#00FF00"))))
 '(isearch ((t (:background "#303030" :foreground "#FFD700"))))
 '(isearch-fail ((t (:background "#303030" :foreground "#FF0000"))))
 '(isearch-group-1 ((t (:background "#303030" :foreground "#D7AF00"))))
 '(isearch-group-2 ((t (:background "#303030" :foreground "#AF8700"))))
 '(ivy-current-match ((t (:extend t :foreground "#FFD700"))))
 '(ivy-grep-info ((t (:inherit compilation-info))))
 '(ivy-grep-line-number ((t (:inherit compilation-line-number))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#FFAF00" :weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "#FF8700" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "#FF5F00" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "#FF0000" :weight bold))))
 '(lazy-highlight ((t (:inverse-video t))))
 '(link ((t (:foreground "#D7AF00" :underline t))))
 '(link-visited ((t (:foreground "#875f00" :inherit link))))
 '(linum ((t (:inherit (shadow default) :foreground "#767676"))))
 '(magit-diff-added ((t (:background "#005F00" :foreground "#00FF00"))))
 '(magit-diff-added-highlight ((t (:background "#005F00" :foreground "#00FF00"))))
 '(magit-diff-context-highlight ((t (:background "#303030" :foreground "#BCBCBC"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#303030" :foreground "#BCBCBC"))))
 '(magit-diff-removed ((t (:background "#5F0000" :foreground "#FF0000"))))
 '(magit-diff-removed-highlight ((t (:background "#5F0000" :foreground "#FF0000"))))
 '(magit-section-highlight ((t (:background "#262626"))))
 '(match ((t (:foreground "#FFD700"))))
 '(mc/cursor-face ((t (:inverse-video t))))
 '(menu ((t nil)))
 '(minibuffer-prompt ((t (:foreground "#FFD700"))))
 '(mode-line ((t (:box (:line-width (1 . -1) :style released-button) :foreground "#FFFFFF" :background "#1C1C1C"))))
 '(mode-line-buffer-id ((t (:foreground "#D7AF00" :weight bold))))
 '(mode-line-emphasis ((t (:foreground "#AF8700" :weight bold))))
 '(mode-line-inactive ((t (:weight light :box (:color "#666666") :foreground "#767676" :background "#080808" :inherit mode-line))))
 '(my-linum-hl ((t (:foreground "#FFD700"))))
 '(org-block ((t (:foreground "#EEEEEE"))))
 '(org-date ((t (:foreground "#FFD700" :underline t))))
 '(org-table ((t (:foreground "#FFAF00"))))
 '(region ((t (:inverse-video t))))
 '(secondary-selection ((t (:background "#3A3A3A"))))
 '(show-paren-match ((t (:foreground "#00FF00"))))
 '(show-paren-mismatch ((t (:foreground "#FF00FF"))))
 '(smerge-base ((t (:extend t :foreground "#00cd66"))))
 '(smerge-lower ((t (:foreground "#ddffdd"))))
 '(smerge-markers ((t (:foreground "#7f7f7f"))))
 '(smerge-upper ((t (:extend t :foreground "#ff7256"))))
 '(success ((t (:foreground "#00ff87" :weight extra-bold))))
 '(trailing-whitespace ((t (:background "#FF0000"))))
 '(web-mode-html-attr-equal-face ((t (:inherit default))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face))))
 '(web-mode-html-tag-bracket-face ((t (:inherit default))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(whitespace-empty ((t (:background "#FFFF00" :foreground "#FF0000"))))
 '(whitespace-hspace ((t (:foreground "#262626"))))
 '(whitespace-line ((t (:background "#1C1C1C" :foreground "#FF0000" :inverse-video t))))
 '(whitespace-space ((t (:foreground "brightwhite")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dockerfile-mode docker-compose-mode typescript-mode impatient-showdown which-key web-mode sideline-lsp sideline-flymake sideline-flycheck sideline-blame python-mode pyenv-mode puppet-mode powershell pip-requirements php-mode nginx-mode multiple-cursors markdown-preview-mode kivy-mode json-mode js2-mode ivy highlight-indent-guides hideshowvis fold-this flymake-python-pyflakes exec-path-from-shell emmet-mode crontab-mode company-auctex comment-tags auto-virtualenv)))
