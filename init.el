(setq gc-cons-threshold--original gc-cons-threshold)
(setq gc-cons-threshold 100000000) ;; raise GC threshold to 10MB

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
   '(protobuf-mode lsp-java terraform-mode rainbow-delimiters paredit cider fuzzy slime-company helm-slime ac-slime auto-complete slime dash-at-point treesit-auto ob-go fish-mode yasnippet auto-package-update dockerfile-mode org-drill editorconfig company codeium typescript-mode python-mode lsp-python-ms poetry use-package-ensure dap-dlv-go flyspell-mode icicles mermaid-mode yaml-mode dap-mode flycheck lsp-ui lsp-mode go-mode use-package magit exec-path-from-shell))
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

;; automatically update packages
(use-package auto-package-update)

;; Git integration
(use-package magit
  :pin melpa-stable)

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
  (defun get-org-dir (&optional override)
    "Get main directory containing .org files. Configurable using either $ORG_DIR env var, or defaults to ORG-DIRECTORY ($HOME/org/). Relative filenames are expanded."
    (interactive)
    (expand-file-name (or override (getenv "ORG_DIR") org-directory)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (go . t)))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively nil)
  ;; setup org-capture
  (setq org-default-notes-file (concat (get-org-dir) "/notes.org"))
  (setq org-capture-templates
        `(("t" "Todo" entry (file+datetree ,(concat (get-org-dir) "/todo.org")) "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree ,(concat (get-org-dir) "/journal.org")) "* %?\n  %i\n")))
  ;; setup org-agenda
  (setq  org-agenda-files (list (get-org-dir)))
  )
(use-package ox-md ;; markdown backend for org-mode
  :after org
  :ensure nil)
(use-package ob-go ;; org-babel support for Go: https://github.com/pope/ob-go
  :after org)
(use-package org-drill ;; Spaced repetition for Org mode: https://orgmode.org/worg/org-contrib/org-drill.html
  :after org
  :config
  (defun org-drill-refresh-scope (&optional dir)
    "Updates org-drill-scope to include all .org files. DIR default is the result of get-org-dir."
    (interactive) ;; TODO: allow passing in directory arg interactively
    (let ((org-dir (or dir (get-org-dir)))
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
  (text-mode))
(use-package flyspell-prog-mode
  :ensure nil
  :hook
  (prog-mode))

;; autocomplete using company-mode: https://company-mode.github.io/
(use-package company
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 1))

;; LSP: https://github.com/emacs-lsp/lsp-mode
(add-to-list 'image-types 'svg) ;; error workaround: https://github.com/Alexander-Miller/treemacs/issues/1017#issuecomment-1515602288
(use-package lsp-mode
  :hook prog-mode ;; try LSP mode for all prog-mode
  :init
  (setq lsp-eslint-format nil)
  :config
  ;; format on save
  ;; uses eslint for ts/js if eslint lsp server is installed and available
  (add-hook 'before-save-hook (lambda ()
				(if (and (or (eq major-mode 'typescript-mode)
					     (eq major-mode 'typescript-ts-mode)
					     (eq major-mode 'javascript-mode)
					     (eq major-mode 'javascript-ts-mode)
					     (eq major-mode 'js-mode)
					     (eq major-mode 'js2-mode))
					 (functionp 'lsp-eslint-apply-all-fixes))
				    (lsp-eslint-apply-all-fixes)
				  (lsp-format-buffer)))) 
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
  ;; (setq gc-cons-threshold 100000000) ;; See: https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold ;; instead, set at top of file to speed up loading
  (setq read-process-output-max (* 1024 1024)) ;; 1mb, See: https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
  )
(use-package flycheck) ;; syntax highlighting
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package lsp-java
  :config
  ;; below is copied and adapted from https://github.com/sei40kr/lsp-java-lombok/blob/f77514f0b2fae634c4e02070afce04287032f13e/lsp-java-lombok.el
  ;; as the provided commands needed some updates for Java modules
  (defgroup lsp-java-lombok nil
    "Lombok for Java LSP"
    :prefix "lsp-java-lombok-"
    :group 'languages)
  (defcustom lsp-java-lombok-jar-path (expand-file-name
                                       (locate-user-emacs-file ".cache/lombok.jar"))
    "The location of the Lombok JAR."
    :group 'lsp-java-lombok
    :risky t
    :type 'directory)
  (defun lsp-java-lombok-download ()
    "Download the latest Lombok JAR file and install it into `lsp-java-lombok-jar-path'."
    (interactive)
    (if (and (y-or-n-p (format "Download the latest Lombok JAR into %s? "
                               lsp-java-lombok-jar-path))
             (or (not (file-exists-p lsp-java-lombok-jar-path))
                 (y-or-n-p (format "The Lombok JAR already exists at %s, overwrite? "
                                   lsp-java-lombok-jar-path))))
        (progn
          (mkdir (file-name-directory lsp-java-lombok-jar-path) t)
          (message "Downloading Lombok JAR into %s" lsp-java-lombok-jar-path)
          (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lsp-java-lombok-jar-path t))
      (message "Aborted.")))
  (defun lsp-java-lombok ()
    "Configure lsp-java to let the server to load the Lombok JAR."
    (setq lsp-java-vmargs
          (append lsp-java-vmargs
                  (list (concat "-javaagent:" lsp-java-lombok-jar-path)))))
  (when (file-exists-p lsp-java-lombok-jar-path)
      (lsp-java-lombok)))

;; DAP: https://github.com/emacs-lsp/dap-mode
(use-package dap-mode)
(use-package dap-dlv-go ;; Go support
  :ensure nil
  :config
  ;; special run configurations
  (defun dap-register-go-launch-configuration (path)
    "Register a 'Go Launch' DAP Run Configuration using PATH."
    (interactive (list
		  (read-string "Go Package Path: " "${workspaceFolder}")))
    (dap-register-debug-template
     (format "Go Dlv Launch Package Configuration (%s)" path)
     (list :type "go"
	:cwd "${workspaceFolder}"
        :request "launch"
        :mode "auto"
        :program path))))
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
(use-package dap-java
  :ensure nil)

;; major mode for working with YAML files: https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook (lambda () (setq tab-width 2))))

;; major mode for working with mermaid.js: https://mermaid.js.org/
(use-package mermaid-mode)

;; major mode for working with Golang: https://github.com/dominikh/go-mode.el
(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  :hook
  (before-save-hook gofmt-before-save))

;; major mode for typescript: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :init
  (setq typescript-indent-level 2))

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

;; macOS specific packages
(when (memq window-system '(mac ns x))
  ;; On OS X, an Emacs instance started from the graphical user
  ;; interface will have a different environment than a shell in a
  ;; terminal window, because OS X does not run a shell during the
  ;; login. Obviously this will lead to unexpected results when
  ;; calling external utilities like make from Emacs.
  ;; This library works around this problem by copying important
  ;; environment variables from the user's shell.
  ;; https://github.com/purcell/exec-path-from-shell
  (use-package exec-path-from-shell)

  ;; Local documentation for macOS: https://github.com/stanaka/dash-at-point#readme
  (use-package dash-at-point))

(use-package dockerfile-mode ;; Syntax highlighting for Dockerfiles: https://github.com/spotify/dockerfile-mode
  )

(add-to-list 'load-path "~/.emacs.d/github.el/") ;; installed as a Git submodule
(use-package github
  :ensure nil)

(use-package fish-mode ;; https://github.com/wwwjfy/emacs-fish
  )

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
    (use-package treesit-auto ;; Automatically install + setup treesitter modes: https://github.com/renzmann/treesit-auto
      :demand t
      :config
      (setq treesit-auto-install t)
      (global-treesit-auto-mode)
      ;; custom recipes
      (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
					                          :lang 'gomod
					                          :ts-mode 'go-mod-ts-mode
					                          :remap '(go-dot-mod-mode)
					                          :url "https://github.com/camdencheek/tree-sitter-go-mod"))
      (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
					                          :lang 'fish
					                          :url "https://github.com/ram02z/tree-sitter-fish")))
    (use-package go-ts-mode
      :ensure nil
      :after treesit-auto
      :custom
      (go-ts-mode-indent-offset 4))
    (use-package yaml-ts-mode
      :ensure nil
      :after treesit-auto
      :config
      (add-hook 'yaml-ts-mode-hook (lambda () (setq tab-width 2)))))

(defun slime-auto-complete-setup-hook ()
  "This disables company-mode for slime-mode, while still seting up & configuring
  so it can be enabled and still work as expected. Doesn't work as well as
  auto-complete+ac-slime since company is missing inline docstrings."
  (add-to-list 'slime-completion-at-point-functions #'slime-fuzzy-complete-symbol)
  (add-hook 'slime-mode-hook (lambda ()
                               (company-mode 0)
                               (auto-complete-mode 1)))
  (add-hook 'slime-repl-mode-hook (lambda ()
                                    (company-mode 0)
                                    (auto-complete-mode 1))))

(use-package slime ;; Lisp support: https://slime.common-lisp.dev/
  :config
  ;; https://slime.common-lisp.dev/doc/html/Contributed-Packages.html#Contributed-Packages
  (slime-setup '(slime-fancy
                 slime-quicklisp
                 slime-asdf
                 slime-autodoc
                 helm-slime
                 ;; slime-company
                 slime-xref-browser
                 slime-fuzzy)))
(use-package helm-slime ;; https://github.com/emacs-helm/helm-slime
  :after slime)
(use-package slime-company ;; https://github.com/anwyn/slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space)
  (slime-auto-complete-setup-hook))
(use-package auto-complete ;; Similar function to Company: https://github.com/auto-complete/auto-complete
  :after slime ;; to make sure hooks are in order
  :config
  (slime-auto-complete-setup-hook))
(use-package fuzzy ;; https://github.com/auto-complete/fuzzy-el
  :after auto-complete)
(use-package ac-slime ;; slime autocomplete: https://github.com/purcell/ac-slime
  :after (slime auto-complete)
  :config
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))
  (slime-auto-complete-setup-hook))

(use-package cider) ;; Clojure support: https://cider.mx/
(use-package paredit ;; Lisp programming conveniences: http://paredit.org/
  :after (cider slime)
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode clojure-mode cider-repl-mode) . paredit-mode))
(use-package rainbow-delimiters ;; Make reading nested parens easier: https://github.com/Fanael/rainbow-delimiters
  :after (cider slime)
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package doom-themes ;; Themes from doomacs: https://github.com/doomemacs/themes
  :config
  (load-theme 'doom-dracula t))

(use-package uniquify ;; Overrides Emacs’ default mechanism for making buffer names unique, from: https://git.sr.ht/~technomancy/better-defaults
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package terraform-mode ;; Major mode for Hashicorp Terraform: https://github.com/hcl-emacs/terraform-mode
  )

;; minibuffer autocomplete config
(use-package icomplete ;; incremental completion, part of Emacs: https://www.emacswiki.org/emacs/IcompleteMode
  :ensure nil
  :config
  (icomplete-mode 1)
  (icomplete-vertical-mode 1)
  (setq completion-cycle-threshold t))

;; protobuf support
(use-package protobuf-mode)

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

;; Treat snake_case as one word
(global-superword-mode 1)

;; generate TAGS table file with etags
(defun etags (d &optional append?)
  "Uses etags to generate a TAGS table file in directory D. If APPEND? is truthy, equivalent of calling etags with -a flag."
  (interactive "F")
  (let ((command (string-join (list "find . -not \\( -path \"./.git\" -prune \\) -type f | xargs etags" (if append? "-a" "")) " ")) 
	    (default-directory d))
    (shell-command-to-string command)))


;; Load secrets with Emacs' built-in GPG support
;; See: https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources

(defun load-if-exists (f)
  (let ((expanded (expand-file-name f)))
    (if (file-exists-p expanded)
	(load-file expanded))))

(load-if-exists "~/.emacs.d/secrets.el.gpg")
(load-if-exists "~/.emacs.d/secrets.el")

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Eshell helpers
;; https://howardism.org/Technical/Emacs/eshell-fun.html

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; quickly swap from horizontal to vertical split & vice-versa
;; copied from here: https://stackoverflow.com/questions/14881020/emacs-shortcut-to-switch-from-a-horizontal-split-to-a-vertical-split-in-one-move
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; General "improved" defaults from: https://git.sr.ht/~technomancy/better-defaults
;; See also: https://idiomdrottning.org/bad-emacs-defaults (I don't agree with _everything_ there)
(unless (memq window-system '(mac ns)) ;; Disable menu bar on everything but mac
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) ;; Disable tool-bar-mode
  (tool-bar-mode -1))
(save-place-mode 1) ;; https://www.emacswiki.org/emacs/SavePlace
(setq-default indent-tabs-mode nil) ;; change tab behavior
(setq-default tab-width 4)
(setq apropos-do-all t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-by-copying t
      show-trailing-whitespace t)
(unless backup-directory-alist ;; don't litter directory with backups and autosaves, but still backup and auto-save
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

;; reset GC threshold
(setq gc-cons-threshold gc-cons-threshold--original)
