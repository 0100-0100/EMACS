;; ──────────── Emacs Config for no hassle at HolbertonSchool. ───────────── ;;
;;                                                                           ;;
;;  This is my configuration file for emacs, was done for use with Vagrant   ;;
;;    running Ubuntu 14.04.6 LTS trusty on VirtualBox over SSH.              ;;
;;                                                                           ;;
;;   ──── Latest update March 21st 2022 With the use of GNU Emacs 26.3 ───   ;;
;;                                                                           ;;
;;  Most of the color configuration was done through emacs's own configs for ;;
;;    utility colors like the highlight of the whitespaces left around,      ;;
;;    etc..., to change those or pick a different theme, do:                 ;;
;;                                                                           ;;
;;    M-x and write "menu-bar-open"                                          ;;
;;    press 'o' for Options.                                                 ;;
;;    press 'c' for Customize Emacs                                          ;;
;;                                                                           ;;
;;  And from there you can pick where to go, but is always faster to use     ;;
;;    commands like:                                                         ;;
;;                                                                           ;;
;;      M-x customize-themes                                                 ;;
;;                                                                           ;;
;;  This configuration comes with the tab indentation required by the betty  ;;
;;    coding style in the C language used at Holberton school, Python, JS    ;;
;;    Html, css syntax highlight and indentation support, highlight for      ;;
;;    lines no more than 80 characters in lenght and some extra features.    ;;
;;                                                                           ;;
;;                      Any contributions are welcome.                       ;;
;; ──────────────── | Udpated by Diego Lopez  Feb-2022 | ─────────────────── ;;
;; ────────────────────| Cohort 13 @Holberton School |────────────────────── ;;

;; ───────────────────────────────────────────────────────────────────────── ;;
;;                    Default setting and configurations.                    ;;
;; ───────────────────────────────────────────────────────────────────────── ;;

;; Enables auto load function to load any chage on files automatically. ──── ;;
;; (global-auto-revert-mode t)

;; Set org evaluate to nil by default.
(setq org-confirm-babel-evaluate nil)

;; Enable downcase region command. ───────────────────────────────────────── ;;
(put 'downcase-region 'disabled nil)

;; Prevents creation of back-up ~files. ──────────────────────────────────── ;;
(setq backup-inhibited t)

;; Toggles truncation of long lines instead of wrapping. ─────────────────── ;;
(setq-default truncate-lines t)

;; Highlights whitespaces and lines over 80 characters. ──────────────────── ;;
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))

;; Turn on whitespace mode when entering a c-type or python file. ────────── ;;
(add-hook 'c-mode-common-hook 'whitespace-mode t)
(add-hook 'python-mode-hook 'whitespace-mode t)
(add-hook 'js2-mode-hook 'whitespace-mode t)
(add-hook 'css-mode-hook 'whitespace-mode t)
(add-hook 'html-mode-hook 'whitespace-mode t)

(add-hook 'js2-mode-hook (lambda ()
                           (setq whitespace-style '(face empty trailing))
                           ))
(add-hook 'html-mode-hook (lambda ()
                            (setq whitespace-style '(face empty trailing))
                            ))

;; Disables auto-save. ───────────────────────────────────────────────────── ;;
(setq auto-save-default nil)

;; Enables use of C-c to copy, C-x to cut... ─────────────────────────────── ;;
(cua-mode 1)

;; Auto closing brackets. ────────────────────────────────────────────────── ;;
(electric-pair-mode 1)

;; Highlights parenthesis and brackets. ──────────────────────────────────── ;;
(show-paren-mode t)

;; Higlights current line.
;; (global-hl-line-mode t)

;; Remembers cursor's last position. ─────────────────────────────────────── ;;
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; UTF-8 as default encoding. ────────────────────────────────────────────── ;;
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Shows cursor position within line. ────────────────────────────────────── ;;
(column-number-mode 1)

;; Shows line number column on the left by default. ──────────────────────── ;;
(global-linum-mode t)

;; Set only spaces and no tabs. ──────────────────────────────────────────── ;;
;; (setq-default indent-tabs-mode t) t for tabs 
(setq-default indent-tabs-mode nil) ;; nil for no tabs

;; Set default tabulation width in spaces. ───────────────────────────────── ;;
(setq-default c-default-style "linux" c-basic-offset 4 tab-width 4
              indent-tabs-mode t)
(setq-default js-indent-level 2)
(setq-default python-indent-offset 4)
(setq-default python-indent-guess-indent-offset nil indent-tabs-mode nil)
(setq-default web-mode-code-indent-offset 2)
(setq-default sgml-basic-offset 4 indent-tabs-mode t) ;; HTML file indent.
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'nginx-mode-hook (lambda () (setq-local indent-tabs-mode t)))

;; Makes tab key always call an indent command. ──────────────────────────── ;;
(setq-default tab-always-indent t)

;; Make tab key do indent first then completion. ─────────────────────────── ;;
(setq-default tab-always-indent 'complete)

;; Makes lines column wider. ─────────────────────────────────────────────── ;;
;; (setq linum-format "%3d\u2502")

;; Turn off linum-mode in the modes below. ───────────────────────────────── ;;
(add-hook 'diff-mode-hook (lambda () (linum-mode -1)))
(add-hook 'special-mode-hook (lambda () (linum-mode -1)))

;; Bind [f5] to comment and C-[f5] to uncomment region. ──────────────────── ;;
(global-set-key [f5] 'comment-region)
(global-set-key (kbd "<C-f5>") 'uncomment-region)

;; Fold this region and unfold with f6 and C-f6. ─────────────────────────── ;;
(global-set-key [f6] 'fold-this)
(global-set-key (kbd "<C-f6>") 'fold-this-unfold-at-point)

;; Bind [f7] key to rectangle selection. ─────────────────────────────────── ;;
(global-set-key [f7] 'cua-rectangle-mark-mode)

;; Bind [f8] key to re-load file from disc. ──────────────────────────────── ;;
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))
(global-set-key [f8] 'revert-buffer-no-confirm)

;; Binds for multicursor. ────────────────────────────────────────────────── ;;
(global-set-key [f9] 'mc/edit-lines)
(global-set-key (kbd "<C-f9>") 'mc/edit-ends-of-lines)
(global-set-key (kbd "<M-f9>") 'mc/edit-beginnings-of-lines)

;;(global-set-key [f12] 'diff-hl--global-turn-on)

;; Bind C-x C-<up> to move line up ───────────────────────────────────────── ;;
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key (kbd "C-x C-<up>") 'move-line-up)

;; Bind C-x C-<down> to move line down ───────────────────────────────────── ;;
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "C-x C-<down>") 'move-line-down)

;; Disables overly aggressive indentation, for recent emacs versions. ────── ;;
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Pretty print json files when they're opened ───────────────────────────── ;;
(add-to-list
 'auto-mode-alist
 '("\\.json\\'" . (lambda ()
                    (javascript-mode)
                    (json-pretty-print (point-min) (point-max))
                    (goto-char (point-min))
                    (set-buffer-modified-p nil))))

;; Adds colorhiglighting for hex codes in html and css modes. ─────────────── ;;
(defun xah-syntax-color-hex ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))
(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)
(add-hook 'js2-mode-hook 'xah-syntax-color-hex)
(add-hook 'xml-mode 'xah-syntax-color-hex)

;; Highlight the number on the line numbers. ─────────────────────────────── ;;
(require 'hl-line)
(defface my-linum-hl
  `((t :inherit linum :background ,(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)
(defvar my-linum-format-string "%3d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d \u2503 ")))
    (setq my-linum-format-string format)))
(defvar my-linum-current-line-number 0)
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'my-linum-hl
                'linum)))
(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

;; Increment and Decrement Integer at Point ─────────────────────────────── ;;
(require 'thingatpt)
(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(global-set-key (kbd "C-c +") #'increment-integer-at-point)
(global-set-key (kbd "C-c -") #'decrement-integer-at-point)

;; Sets default mode for .js files as tide-mode instead of js2-mode. ─────── ;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . tide-mode))

(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
(setq org-babel-python-command "python3")
'(lazy-lock-mode t nil (lazy-lock))
;; (setq org-babel-python-command "python3")
;; (org-babel-do-load-languages 'org-babel-load-languages
;; 							 '(
;; 							   (emacs-lisp . t)
;; 							   (python . t)
;; 							   )
;; 							 )

;; Sets default mode for .js files as tide-mode instead of js2-mode. ─────── ;;
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . emmet-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . tide-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; ───────────────────────────────────────────────────────────────────────── ;;
;;                             Custom variables.                             ;;
;; ───────────────────────────────────────────────────────────────────────── ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.05)
 '(company-minimum-prefix-length 1)
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
	(all-the-icons-dired all-the-icons-ivy all-the-icons-ivy-rich company csproj-mode diff-hl direx docker-compose-mode dockerfile-mode dumb-jump elpy emmet-mode exec-path-from-shell flycheck-pycheckers flycheck-rust flymake-eslint flymake-python-pyflakes fold-this gnu-elpa-keyring-update go-mode jedi jinja2-mode js2-highlight-vars js2-mode json-mode lsp-mode lsp-ui magit markdown-mode markdown-preview-mode multiple-cursors nginx-mode omnisharp org puppet-mode rust-mode rustic tide use-package web-mode yasnippet)))
 '(safe-local-variable-values (quote ((flycheck-checker . pycodestyle)))))

;; ───────────────────────────────────────────────────────────────────────── ;;
;;                               Custom faces.                               ;;
;; ───────────────────────────────────────────────────────────────────────── ;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#FFFFFF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(button ((t (:foreground "color-178" :overline t :underline t))))
 '(company-preview ((t (:background "color-235" :foreground "color-178"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "color-178"))))
 '(company-scrollbar-bg ((t (:background "color-236"))))
 '(company-scrollbar-fg ((t (:background "color-220"))))
 '(company-tooltip ((t (:background "color-235" :foreground "color-136"))))
 '(company-tooltip-annotation ((t (:foreground "color-220"))))
 '(company-tooltip-common ((t (:foreground "color-220"))))
 '(company-tooltip-selection ((t (:background "color-236"))))
 '(custom-button-pressed-unraised ((t (:inherit custom-button-pressed-unraised :foreground "color-94"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "color-136" :weight bold :height 1.2))))
 '(custom-group-tag-1 ((t (:inherit variable-pitch :foreground "color-94" :weight bold :height 1.2))))
 '(custom-state ((t (:foreground "color-47"))))
 '(custom-variable-tag ((t (:foreground "color-136" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :background "#ddffdd" :foreground "color-28"))))
 '(diff-context ((t (:foreground "color-242"))))
 '(diff-file-header ((t (:background "color-234" :foreground "color-220" :weight bold))))
 '(diff-function ((t (:foreground "color-255"))))
 '(diff-header ((t (:foreground "color-220"))))
 '(diff-hunk-header ((t (:inherit diff-header :foreground "color-255"))))
 '(diff-indicator-removed ((t (:inherit diff-removed :foreground "brightred"))))
 '(diff-removed ((t (:inherit diff-changed :background "#ffdddd" :foreground "brightred"))))
 '(eieio-custom-slot-tag-face ((t (:foreground "color-136"))) t)
 '(error ((t (:foreground "brightred" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#B0C5DE"))))
 '(font-lock-comment-face ((t (:foreground "#FF7F24"))))
 '(font-lock-constant-face ((t (:foreground "#7FFFD4"))))
 '(font-lock-function-name-face ((t (:foreground "#87CEFA"))))
 '(font-lock-keyword-face ((t (:foreground "#00FFFF"))))
 '(font-lock-negation-char-face ((t (:foreground "#FF0000"))))
 '(font-lock-string-face ((t (:foreground "#FFA07A"))))
 '(font-lock-type-face ((t (:foreground "#98FB98"))))
 '(font-lock-variable-name-face ((t (:foreground "#FFD787"))))
 '(fringe ((t (:background "color-243" :inverse-video t))))
 '(highlight ((t (:background "color-235"))))
 '(link ((t (:foreground "color-178" :underline t))))
 '(link-visited ((t (:inherit link :foreground "color-94"))))
 '(linum ((t (:inherit (shadow default) :foreground "color-243"))))
 '(magit-diff-added ((t (:background "color-22" :foreground "brightgreen"))))
 '(magit-diff-added-highlight ((t (:background "color-22" :foreground "brightgreen"))))
 '(magit-diff-context-highlight ((t (:background "color-236" :foreground "color-250"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "color-236" :foreground "color-250"))))
 '(magit-diff-removed ((t (:background "color-52" :foreground "brightred"))))
 '(magit-diff-removed-highlight ((t (:background "color-52" :foreground "brightred"))))
 '(magit-section-highlight ((t (:background "color-235"))))
 '(menu ((t nil)))
 '(minibuffer-prompt ((t (:foreground "color-220"))))
 '(mode-line ((t (:foreground "brightwhite" :box (:line-width -1 :color "red" :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground "color-178" :weight bold))))
 '(mode-line-emphasis ((t (:foreground "color-136" :weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "color-243" :box (:line-width -1 :color "grey40") :weight light))))
 '(my-linum-hl ((t (:foreground "color-220"))))
 '(org-block ((t (:foreground "color-255"))))
 '(org-date ((t (:foreground "color-220" :underline t))))
 '(org-table ((t (:foreground "color-214"))))
 '(region ((t (:inverse-video t))))
 '(secondary-selection ((t (:background "color-237"))))
 '(show-paren-match ((t (:foreground "#00FF00"))))
 '(show-paren-mismatch ((t (:foreground "#FF00FF"))))
 '(trailing-whitespace ((t (:background "brightred"))))
 '(whitespace-empty ((t (:background "yellow" :foreground "brightred"))))
 '(whitespace-hspace ((t (:foreground "color-235"))))
 '(whitespace-line ((t (:background "color-234" :foreground "brightred" :inverse-video t))))
 '(whitespace-space ((t (:foreground "brightwhite")))))

;; ───────────────────────────────────────────────────────────────────────── ;;
;;                            Added lisp packages.                           ;;
;; ───────────────────────────────────────────────────────────────────────── ;;
;;                                                                           ;;
;; Tells emacs where the elisp lib dir is.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Adds Melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; 01. Loads Indent-guide package.
;; Use the command below for downloading the indent higlight file:
;;
;; wget https://github.com/zk-phi/indent-guide/raw/master/indent-guide.el -P ~/.emacs.d/lisp
;;
(load "indent-guide")
;; Sets color of indentation-guide character.
(set-face-foreground 'indent-guide-face "color-243")
(setq indent-guide-char "\u2502")
(indent-guide-global-mode)

;; To enable completion install the package running the command:
;;
;;    M-x install-packages ENT company
;;
;; 02. Enables complete-anything on all buffers.
(add-hook 'after-init-hook 'global-company-mode)

;; To enable custom snippet auto-completion.
;;
;;    M-x install-packages ENT yasnippet
;;
;; 03. Enables yasnippet on html.
(add-to-list 'load-path
             "~/.emacs.d/elpa/yasnippet-0.14.0")
(require 'yasnippet)
(yas-reload-all)
(yas-global-mode)

;; To enable emmet completion for html documents.
;;
;;    M-x install-packages ENT emmet-mode
;;
;; 04. Enables emmet mode on html.
(add-to-list 'load-path "~/.emacs.d/elpa/emmet-mode-1.0.8/emmet/")
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; Crontab mode.
(add-to-list 'load-path "~/.emacs.d/elpa/crontab-mode-20210715.133/")
(load "crontab-mode")
(add-to-list 'auto-mode-alist '("\\crontab\\'" . crontab-mode))

;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (setq-default flycheck-temp-prefix ".flycheck")
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(json-jsonlist)))

;; ;; Rust Development mode configuration.
;; (use-package rustic
;;   :ensure
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; uncomment for less flashiness
;;   ;; (setq lsp-eldoc-hook nil)
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save t)
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t)))

;; (use-package lsp-mode
;;   :ensure
;;   :commands lsp
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.6)
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(put 'upcase-region 'disabled nil)

;; Python3 flycmake configurations.
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

;; TypeScript tide mode configuration.
(defun setup-tide-mode ()
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

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Ivy mode configs.
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
