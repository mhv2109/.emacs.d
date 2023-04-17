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
   '(lsp-java company codeium typescript-mode python-mode lsp-python-ms poetry use-package-ensure dap-dlv-go flyspell-mode icicles mermaid-mode yaml-mode dap-mode flycheck lsp-ui lsp-mode go-mode evil use-package magit exec-path-from-shell))
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
  (setq org-log-done t))
(use-package ox-md ;; markdown backend for org-mode
  :after org
  :ensure nil)

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
        company-minimum-prefix-length 0))

;; LSP: https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :hook prog-mode ;; try LSP mode for all prog-mode
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer) ;; format on save
  ;; language-specific settings
  (lsp-register-custom-settings
   '(
     ;; ts/js settings: https://github.com/typescript-language-server/typescript-language-server#workspacedidchangeconfiguration
     ("typescript.format.indentSize" 4 t)
     ("typescript.format.convertTabsToSpaces" t t)
     ("javascript.format.indentSize" 4 t)
     ("javascript.format.convertTabsToSpaces" t t)
     ("java.format.settings.url" "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"))
  ))
(use-package lsp-ui ;; intellisense-like context hover
  :after lsp-mode
  :init
  (setq gc-cons-threshold 100000000) ;; See: https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
  (setq read-process-output-max (* 1024 1024)) ;; 1mb, See: https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
  )
(use-package flycheck) ;; syntax highlighting
(use-package lsp-java  ;; Java support: https://emacs-lsp.github.io/lsp-java/
  :after lsp-mode
  :config
  (let ((lombok-path (expand-file-name "~/.emacs.d/assets/lombok.jar")))
    (setq lsp-java-vmargs (list (concat "-javaagent:" lombok-path)
				"-noverify"
				"-Xmx100m"
				"-Xmx2G"
				"-XX:+UseG1GC"
				"-XX:+UseStringDeduplication"))))

;; DAP: https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :after lsp-mode)
(use-package dap-dlv-go ;; Go support
  :ensure nil
  :after dap-mode) 
(use-package dap-java ;; Java support
  :ensure nil
  :after dap-mode)

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
  (global-set-key (kbd "M-C-<tab>") 'codeium-completion-toggle))

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

;; quickly cycle buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; generate TAGS table file with etags
(defun etags (d &optional append?)
  "Uses etags to generate a TAGS table file in directory D. If APPEND? is truthy, equivalent of calling etags with -a flag."
  (interactive "F")
  (let ((command (string-join (list "find . -not \\( -path \"./.git\" -prune \\) -type f | xargs etags" (if append? "-a" "")) " ")) 
	(default-directory d))
    (shell-command-to-string command)))
