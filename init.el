(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-pinned-packages '(use-package . "melpa-stable") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; use-package is used to both configure and install packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dockerfile-mode org-drill editorconfig company codeium typescript-mode python-mode lsp-python-ms poetry use-package-ensure dap-dlv-go flyspell-mode icicles mermaid-mode yaml-mode dap-mode flycheck lsp-ui lsp-mode go-mode evil use-package magit exec-path-from-shell))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Package customizations
;;

;; bootstrap use-package: https://github.com/jwiegley/use-package
(require 'use-package)
(use-package use-package-ensure
  :config (setq use-package-always-ensure t) ;; always ensure packages are installed
  )

;; icicles: https://www.emacswiki.org/emacs/Icicles
(add-to-list 'load-path "~/.emacs.d/icicles/") ;; installed as a Git submodule
(use-package icicles
  :ensure nil
  :config
  (icy-mode 1))

;; Git integration
(use-package magit
  :pin melpa-stable)

;; Enable vim keybindings
(use-package evil
  :pin melpa-stable
  :config
  (evil-mode 1))

;; Functions to insert the current date for org-mode doc headers
(use-package calendar
  :config
  (defun insdate-insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale. With a prefix argument, the date is inserted without the day of the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				   omit-day-of-week-p)))
  (defun insdate-insert-current-datetime ()
    "Insert current date and time, including timezone."
    (interactive)
    (let ((current-time (nth 3 (split-string (current-time-string)))))
	(insert (concat
		(calendar-date-string (calendar-current-date) nil)
		" " current-time " " (nth 1 (current-time-zone))))))
  (global-set-key "\C-x\M-d" `insdate-insert-current-date)
  (global-set-key "\C-x\M-t" `insdate-insert-current-datetime))

;; Org mode: https://orgmode.org/
(use-package org
  :init
  (setq org-todo-keywords '("TODO" "IN PROGRESS" "|" "DONE" "DEFERRED" "DELEGATED")) ;; Update TODO states
  :config
  (setq org-log-done t)
  (defun get-org-dir (&optional default)
    "Get main directory containing .org files. Configurable using either $ORG_DIR env var, or defaults to $HOME/org/. Relative filenames are expanded."
    (interactive)
    (let ((default (if default default "~/org/")) ;; ~/org/ is where I typically keep my org files
	  (envv (getenv "ORG_DIR"))) 
      (expand-file-name (if envv
			    envv
			  default)))))
(use-package ox-md ;; markdown backend for org-mode
  :after org
  :ensure nil)
(use-package org-drill ;; Spaced repetition for Org mode: https://orgmode.org/worg/org-contrib/org-drill.html
  :after org
  :config
  (defun org-drill-refresh-scope (&optional dir)
    "Updates org-drill-scope to include all .org files. DIR default is the result of get-org-dir."
    (interactive) ;; TODO: allow passing in directory arg interactively
    (let ((org-dir (if dir dir (get-org-dir))) 
	  (org-regexp "^[^#].*\\.org$")) 
      (setq org-drill-scope (directory-files-recursively org-dir org-regexp))
      (message "%s" "Updated org-drill-scope.")))
  (org-drill-refresh-scope))

;; spellchecking
(use-package flyspell-mode
  :ensure nil
  :config
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined)
  :hook
  (text-mode
   ;; prog-mode ;; TODO: make this smarter about code vs. comments
   ))

;; autocomplete using company-mode: https://company-mode.github.io/
(use-package company
  :config
  (global-company-mode t)
  (setq-default
        company-idle-delay 0.05
        company-require-match nil
        company-minimum-prefix-length 1))

;; LSP: https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :hook prog-mode ;; try LSP mode for all prog-mode
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer) ;; format on save
  ;; language-specific settings
  (lsp-register-custom-settings
   '(
     ;; ts/js settings: https://github.com/typescript-language-server/typescript-language-server#workspacedidchangeconfiguration
     ("typescript.format.indentSize" 2 t)
     ("typescript.format.convertTabsToSpaces" t t)
     ("javascript.format.indentSize" 2 t)
     ("javascript.format.convertTabsToSpaces" t t))
  ))
(use-package lsp-ui ;; intellisense-like context hover
  :init
  (setq gc-cons-threshold 100000000) ;; See: https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
  (setq read-process-output-max (* 1024 1024)) ;; 1mb, See: https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
  )
(use-package flycheck) ;; syntax highlighting

;; DAP: https://github.com/emacs-lsp/dap-mode
(use-package dap-mode)
(use-package dap-dlv-go ;; Go support
  :ensure nil) 
(use-package dap-node ;; NodeJS support
  :ensure nil
  :config
  (dap-node-setup)
  ;; special run configurations
  (dap-register-debug-template
   "Node Jest Run Configuration"
   (list :type "node"
         :cwd "${workspaceFolder}"
         :request "launch"
         :program "${workspaceFolder}/node_modules/jest/bin/jest.js"
	 :args "-i")))

;; major mode for working with YAML files: https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;; major mode for working with mermaid.js: https://mermaid.js.org/
(use-package mermaid-mode)

;; major mode for working with Golang: https://github.com/dominikh/go-mode.el
(use-package go-mode
  :init
  (setq gofmt-command "goimports"))

;; major mode for typescript: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode)

;; Codeium AI assistant: https://github.com/Exafunction/codeium.el
(add-to-list 'load-path "~/.emacs.d/codeium.el/") ;; installed as a Git submodule
(use-package codeium
  :ensure nil
  ;;:after corfu
  :init
  ;; read API key from environment
  (setq codeium/metadata/api_key (getenv "CODEIUM_API_KEY"))
  :config
  ;; keybinding to enable/disable suggestions (overrides other autocompletion backends)
  (defun codeium-completion-toggle ()
    "Toggles Codeium AI autocomplete suggestions. If enabled, overrides any other completion-at-point functions."
    (interactive)
    (if (member #'codeium-completion-at-point completion-at-point-functions)
	(progn
	  ;; toggle from ON to OFF
	  (setq-local completion-at-point-functions previous-completion-at-point-functions
		      company-frontends previous-company-frontends)
	  (message "Disabling Codeium in current buffer"))
      (progn
	;; toggle from OFF to ON
	(setq-local previous-completion-at-point-functions completion-at-point-functions
		    previous-company-frontends company-frontends
		    completion-at-point-functions (list #'codeium-completion-at-point)
		    company-frontends '(company-preview-frontend))
	(message "Enabling Codeium in current buffer")))) 
  (global-set-key (kbd "M-C-S-<tab>") 'codeium-completion-toggle))

;; GitHub Copilot: https://github.com/zerolfx/copilot.el
(add-to-list 'load-path "~/.emacs.d/copilot.el/") ;; installed as a Git sumbodule
(use-package dash)
(use-package s)
(use-package editorconfig)
(use-package copilot
  :ensure nil
  :after (dash s editorconfig)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (global-set-key (kbd "M-C-<tab>") 'copilot-mode))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (memq window-system '(mac ns x))
    (use-package exec-path-from-shell))

(use-package dockerfile-mode ;; Syntax highlighting for Dockerfiles: https://github.com/spotify/dockerfile-mode
  )

;;
;; Other customizations
;;

;; Disable startup splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; keybinding for quickly inserting zero-length strings so radio links to will work with plurals
(defun insert-zero-length-space ()
  "Insert a zero-length space under cursor."
  (interactive)
  (insert-char 8203))
(global-set-key "\C-x\M-z" `insert-zero-length-space)

;; enable line wrap
(global-visual-line-mode t)

;; show line numbers
(global-display-line-numbers-mode t)

;; add closing brackets & parens
(electric-pair-mode t)

;; add ruler
(setq display-fill-column-indicator-column 80) ;; default
(add-hook 'java-mode-hook (lambda () (setq-local display-fill-column-indicator-column 100))) ;; java
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; quickly cycle buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; quickly swap between windows
(windmove-default-keybindings)
(setq windmove-wrap-around nil)

;; generate TAGS table file with etags
(defun etags (d &optional append?)
  "Uses etags to generate a TAGS table file in directory D. If APPEND? is truthy, equivalent of calling etags with -a flag."
  (interactive "F")
  (let ((command (string-join (list "find . -not \\( -path \"./.git\" -prune \\) -type f | xargs etags" (if append? "-a" "")) " ")) 
	(default-directory d))
    (shell-command-to-string command)))
