;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my Emacs configuration file.

;;; Code:

(setq max-lisp-eval-depth 10000) ;; This is covering up a deeper problem, but hasn't been an issue on my hardware. Blame our Java dependency management.

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
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(agent-shell auto-package-update cider corfu counsel dape
		 dockerfile-mode doom-themes editorconfig eldoc-box
		 elfeed embark embark-consult exec-path-from-shell
		 fish-mode flymake-golangci forge gcmh git-link
		 go-mode gotest hotfuzz ivy lua-mode magit marginalia
		 markdown-mode mcp mermaid-mode nov ob-go ob-mermaid
		 org org-remark org-roam org-web-tools paredit
		 protobuf-mode pyvenv pyvenv-auto rainbow-delimiters
		 rg terraform-mode treemacs treesit-auto
		 typescript-mode ultra-scroll use-package
		 use-package-ensure vline vterm which-key yaml-mode
		 yasnippet))
 '(package-vc-selected-packages
   '((flymake-golangci :url
		       "https://github.com/storvik/flymake-golangci.git")))
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

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config (exec-path-from-shell-initialize))

;; Garbage Collector Magic Hack: https://github.com/emacsmirror/gcmh
(use-package gcmh
  :config (gcmh-mode 1))

;;
;; UI
;;

(set-face-attribute 'default nil :height 140) ;; set font to 14pt

(use-package doom-themes ;; Themes from doomacs: https://github.com/doomemacs/themes
  :config
  (load-theme 'doom-solarized-light t))

;; Syntax highlighting, built-in
(use-package flymake
  :ensure nil
  :hook ((prog-mode text-mode) . flymake-mode))

;; autocomplete using corfu-mode: https://github.com/minad/corfu
(use-package corfu
  :custom
  (corfu-cycle t)
  ;; (corfu-auto t)
  ;; (corfu-auto-delay 0.1)
  ;; (corfu-auto-prefix 1)
  ;; (corfu-popupinfo-delay '(0.5 . 0.5))
  (corfu-quit-no-match t)
  (corfu-on-exact-match 'quit)
  (corfu-quit-at-boundary t)
  (corfu-preselect 'prompt)
  (tab-always-indent 'complete)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("ESC" . corfu-quit)
        ([escape] . corfu-quit))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode nil))

;; Display eldoc in a childframe: https://github.com/casouri/eldoc-box
(use-package eldoc-box
  :custom
  (eldoc-box-max-pixel-height 350)
  :hook
  ;; use for all prog-mode
  (prog-mode . (lambda ()
                 ;; eldoc-box-hover-at-point-mode conflicts with corfu popupinfo
                 ;; eldoc-box-hover-mode is up and out of the way
                 (eldoc-box-hover-mode t))))

;; https://github.com/editorconfig/editorconfig-emacs/
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; horizontal and vertical line highlighting
;; super slow
(use-package vline)

;; search w/ ripgrep: https://rgel.readthedocs.io/en/latest/index.html
(use-package rg)

;; minibuffer autocomplete config
;; https://github.com/abo-abo/swiper
;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        ivy-wrap t)
  ;; enable swiper
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-S-s") 'swiper-isearch-thing-at-point)
  (global-set-key (kbd "M-C-s") 'swiper-all)
  (global-set-key (kbd "C-r") 'swiper-isearch-backward)
  (global-set-key (kbd "M-%") 'swiper-query-replace)
  (global-set-key (kbd "M-C-%") 'swiper-all-query-replace))

(use-package counsel
  :config
  (counsel-mode 1))

;; Additional docs in minibuffer: https://github.com/minad/marginalia
(use-package marginalia
  :config
  (marginalia-mode))

;; Like right-click context menu for emacs: https://github.com/oantolin/embark
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  ;; see: https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package uniquify ;; Overrides Emacs’ default mechanism for making buffer names unique, from: https://git.sr.ht/~technomancy/better-defaults
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Highlight key combos for incomplete commands: https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode))

;; Faster fuzzy completion: https://github.com/axelf4/hotfuzz
(use-package hotfuzz
  :config
  (setq completion-styles '(hotfuzz)
        completion-ignore-case t))

;; better integrated terminal: https://github.com/akermu/emacs-libvterm
(use-package vterm)

;; smoother scrolling: https://github.com/jdtsmith/ultra-scroll
(use-package ultra-scroll
  :config (ultra-scroll-mode 1))

;; file tree explorer: https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :custom
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-file-event-delay 500)
  :bind
  (:map global-map
        ("<f8>" . treemacs)))

;;
;; Git
;;

;; Git integration
(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1) ;; open magit buffer in same window: https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfunction
  )

;; Git forge (GitHub) integration: https://magit.vc/manual/forge/
(use-package forge
  :after magit)

;; GitHub Permalink at Point (what I was using github.el for):
(use-package git-link
 :after magit)

;; Handle diff3 in the editor
(use-package smerge-mode
  :ensure nil ;; built-in
  :hook (prog-mode))

;;
;; Major+minor modes
;;

;; spellchecking
(use-package flyspell-mode
  :ensure nil
  :config
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined)
  (setq flyspell-issue-message-flag nil) ;; allegedly improves performance
  :hook
  (text-mode))

(use-package flyspell-prog-mode
  :ensure nil
  :after flyspell-mode
  :hook
  (prog-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; major mode for working with YAML files: https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook (lambda () (setq tab-width 2 standard-indent 2))))

;; major mode for working with Golang: https://github.com/dominikh/go-mode.el
(use-package go-mode)

;; quickly run Go unit tests:
(use-package gotest)

;; integrate flymake and golangci-lint: https://github.com/storvik/flymake-golangci
(use-package flymake-golangci
  :after go-mode
  :vc (:url "https://github.com/storvik/flymake-golangci.git" :rev :newest)
  :hook ((eglot-managed-mode . (lambda ()
                                 (when (derived-mode-p '(go-mode go-ts-mode))
                                   (flymake-golangci-load-backend)))) ;; using flymake-golangci with eglot
         ((go-mode go-ts-mode) . flymake-golangci-load-backend) ;; using flymake-golangci with go-mode
         ))

;; major mode for typescript: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode)

(use-package dockerfile-mode ;; Syntax highlighting for Dockerfiles: https://github.com/spotify/dockerfile-mode
  )

(use-package fish-mode ;; https://github.com/wwwjfy/emacs-fish
  )

(use-package paredit ;; Lisp programming conveniences: http://paredit.org/
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode clojure-mode cider-repl-mode) . paredit-mode))

(use-package rainbow-delimiters ;; Make reading nested parens easier: https://github.com/Fanael/rainbow-delimiters
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package terraform-mode ;; Major mode for Hashicorp Terraform: https://github.com/hcl-emacs/terraform-mode
  )

;; protobuf support
(use-package protobuf-mode)

;; customize built-in python.el
(use-package python
  :ensure nil
  :custom
  (python-indent-offset 4)
  :config
  (defun set-python-shell-interpreter-ipython ()
    "If ipython is available, configure `python-shell-interpreter' to use it with autoloads."
    (interactive)
    (when-let ((found (locate-file "ipython" (reverse exec-path) exec-suffixes 1))) ;; don't use `executable-find' because I want to use local ipython bin first
      (setq python-shell-interpreter found
            python-shell-interpreter-args (concat "--no-confirm-exit --simple-prompt --InteractiveShell.display_page=True --InteractiveShell.autosuggestions_provider=None -i " (file-name-directory user-init-file) "autoload.ipy"))))
  (set-python-shell-interpreter-ipython)

  ;; open python shell in same window
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-name action)
                   (string-match-p "*Python*" buffer-name))
                 (display-buffer-same-window))))

(use-package pyvenv
  :after python
  :config
  ;; update python shell to use project-local ipython
  (add-to-list 'pyvenv-post-activate-hooks #'set-python-shell-interpreter-ipython))

(use-package pyvenv-auto
  :after pyvenv)

;; markdown-mode: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (if-let ((found (executable-find "pandoc")))
      (setq markdown-command (concat found " --quiet -f gfm -s"))
    (message "'pandoc' not found, markdown rendering not available.")))

;; lua editing: https://github.com/immerrr/lua-mode
(use-package lua-mode)

;; Mermaid diagrams: https://github.com/abrochard/mermaid-mode
(use-package mermaid-mode
  :mode ("\\.mmd\\'" "\\.mermaid\\'"))

;; Clojure development: https://cider.mx/
(use-package cider)

;;
;; Treesitter
;;

(use-package treesit-auto ;; Automatically install + setup treesitter modes: https://github.com/renzmann/treesit-auto
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode)
  ;; custom recipes
  ;; having issues with newer versions of libtree-sitter-go on linux
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'go
                                          :ts-mode 'go-ts-mode
                                          :remap 'go-mode
                                          :requires 'gomod
                                          :url "https://github.com/tree-sitter/tree-sitter-go"
                                          :revision "v0.19.1"
                                          :ext  "\\.go\\'"))
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
  (add-hook 'yaml-ts-mode-hook (lambda () (setq tab-width 2 standard-indent 2))))

;;
;; Org mode
;;

;; Org mode: https://orgmode.org/
(use-package org
  :init
  (setq org-todo-keywords '("TODO" "IN PROGRESS" "|" "DONE" "DEFERRED" "DELEGATED") ;; Update TODO states
        org-log-done t
        org-preview-latex-default-process 'dvisvgm
        org-confirm-babel-evaluate nil
        org-src-tab-acts-natively nil
        org-startup-with-inline-images t
        org-attach-use-inheritance t)
  :config
  ;; setup org-agenda
  (let* ((org-dir (file-truename org-directory))
         (archived-dir (expand-file-name "archived/" org-dir))
         (agenda-dirs
          (cons org-dir
                (seq-filter
                 #'file-directory-p
                 (directory-files-recursively org-dir ".*" t)))))
    (setq org-agenda-files
          (seq-remove
           (lambda (dir)
             (let* ((normalized-dir (file-truename dir))
                    (relative-dir (file-relative-name normalized-dir org-dir)))
               (or (file-in-directory-p normalized-dir archived-dir)
                   (and (not (string= relative-dir "."))
                        (string-match-p "\\(?:^\\|/\\)\\.[^/]+" relative-dir)))))
           agenda-dirs)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (go . t)
     (mermaid . t)))
  ;; make bolded text appear red
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "red")))
  ;; make underlined text appear orange
  (add-to-list 'org-emphasis-alist
               '("_" (:foreground "orange")))
  ;; make italic text appear green
  (add-to-list 'org-emphasis-alist
               '("/" (:foreground "green")))
  ;; configure attachment directory (absolute)
  (setq org-attach-id-dir (concat (file-truename org-directory) "/data/"))
  ;; configure archival (absolute)
  (setq org-archive-location (concat (file-truename org-directory) "/archived/%s::"))
  ;; open org in ~/org directory in same window
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-name action)
                   (and (string-match-p "\\.org\\'" buffer-name)
                        (string-match-p org-directory (buffer-file-name (get-buffer buffer-name)))))
                 (display-buffer-same-window)))
  ;; display org-agenda in same window vs. closing others+splitting
  (add-to-list 'display-buffer-alist
               '("\\*Agenda Commands\\*" (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("\\*Org Agenda\\*" (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("\\*Org Select\\*" (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("Calendar" (display-buffer-same-window))) ;; not technically org, but I usually use it w/ org
  :hook
  ;; disable electric-indent-mode for org (annoying with lists
  (org-mode . (lambda ()
                (electric-indent-local-mode -1)))
  ;; auto-format all tables on save
  (before-save . (lambda ()
                   (org-table-map-tables 'org-table-align))))

(use-package ox-md ;; markdown backend for org-mode
  :after org
  :ensure nil)

(use-package ob-go ;; org-babel support for Go: https://github.com/pope/ob-go
  :after org)

;; Mermaid diagrams in org-mode: https://github.com/arnm/ob-mermaid
(use-package ob-mermaid
  :after org)

;; retrieve web pages as org files: https://github.com/alphapapa/org-web-tools
(use-package org-web-tools
  :after org
  :custom
  (org-web-tools-pandoc-sleep-time 5.0)) ;; 25x longer than default

;; org-roam: https://www.orgroam.com/
(use-package org-roam
  :after (org org-web-tools)
  :init
  (setq org-roam-directory (file-truename org-directory) ;; file-truename required since ~/org is often a symlink
        org-roam-file-exclude-regexp '("data/" "archived/" "#recycle/") ;; exclude special directories
        org-roam-dailies-directory "dailies/"
        org-roam-completion-everywhere t ;; automatically autocomplete links for notes
        ;; Templates include a top-level heading so we can attach files using org-attach, which doesn't seem to work without a heading
        org-roam-capture-templates '(("d" "default" plain "* ${title}\n%?"
                                      :target (file+head "${directory}/${slug}.org"
                                                         "#+title: ${title}\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("p" "project" plain "* ${title}\n%?"
                                      :target (file+head "projects/${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :projects:\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("a" "area" plain "* ${title}\n%?"
                                      :target (file+head "areas/${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :areas:\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("r" "resource" plain "* ${title}\n%?"
                                      :target (file+head "resources/${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :resources\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("b" "book (resource)" plain "* ${title}\n%?"
                                      :target (file+head "resources/${slug}.org"
                                                         "#+title: ${title}\n#+author: ${author}\n#+edition: ${edition}\n#+publisher: ${publisher}\n#+year: ${year}\n#+created: %U\n#+filetags: :book:resources:\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("w" "website (resource, capture page)" plain "%(org-web-tools--url-as-readable-org \"${ref}\")"
                                      :target (file+head "resources/${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :website:resources:\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("l" "website (resource, link only)" plain "* ${title}\n\nLink: ${ref}\n%?"
                                      :target (file+head "resources/${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :website:resources:\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("i" "person (area)" plain "* ${title}\n%?"
                                      :target (file+head "areas/${slug}.org"
                                                         "#+title: ${title}\n#+company: ${company}\n#+filetags: :person:areas:\n")
                                      :unnarrowed t
                                      :empty-lines 1)))
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)

  ;; add #+filetags: to org-node-find
  (setq org-roam-node-display-template
        (concat (propertize "${tags} " 'face 'org-tag)
                "${title}"))

  ;; open capture buffers in same window
  (add-to-list 'display-buffer-alist
             '((lambda (buffer-name action)
                 (and (string-prefix-p "CAPTURE-" buffer-name)
                      (string-match-p "\\.org\\'" buffer-name)))
               (display-buffer-same-window)))

  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i"    . completion-at-point))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map))

;; Read EPUB from emacs: https://depp.brause.cc/nov.el/
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  ;; open ebook in same window
  (add-to-list 'display-buffer-alist
             '("\\.\\(epub\\|pdf\\|mobi\\|azw3\\|djvu\\)\\'"
               (display-buffer-same-window))))

;; Highlight and annotate text and org files: https://github.com/nobiot/org-remark
(use-package org-remark
  :after org
  :hook after-init
  :custom
  (org-remark-notes-file-name #'org-remark-notes-file-name-function) ;; since my ~/org is a flat structure with a lot of files, keep notes separate
  :bind
  (("C-c m m" . org-remark-mark)
   ("C-c m l" . org-remark-mark-line)
   :map org-remark-mode-map
   ("C-c m o" . org-remark-open)
   ("C-c m ]" . org-remark-view-next)
   ("C-c m [" . org-remark-view-prev)
   ("C-c m r" . org-remark-remove)
   ("C-c m d" . org-remark-delete))
  :config
  ;; automatically enable org-remark when file opened
  (org-remark-global-tracking-mode +1)
  ;; Selectively keep or comment out the following if you want to use
  ;; extensions for Info-mode, EWW, and NOV.el (EPUB) respectively.
  (use-package org-remark-info :ensure org-remark :after info :config (org-remark-info-mode +1))
  (use-package org-remark-eww  :ensure org-remark :after eww  :config (org-remark-eww-mode +1))
  (use-package org-remark-nov  :ensure org-remark :after nov  :config (org-remark-nov-mode +1))
  ;; add magenta highlighter
  (org-remark-create "magenta-highlighter"
                     '(:background "dark magenta")
                     '(CATEGORY "important")))

;;
;; LSP: eglot+dape
;;

(use-package eglot
  :ensure nil
  :custom
  ;; (eglot-events-buffer-size 0)          ;; disable eglot events buffer
  (eldoc-echo-area-prefer-doc-buffer t) ;; prefer eldoc buffer, if visible
  :config
  ;; suppress logging
  ;; (fset #'jsonrpc--log-event #'ignore)

  (defmacro add-server-program-if-found (exec append &rest forms)
    "If EXEC is in `exec-path', bind COMMAND and add FORMS to
EGLOT-SERVER-PROGRAMS. If APPEND is truthy, add to end of list,
otherwise add to start of list."
    `(if-let ((command (locate-file ,exec exec-path exec-suffixes 1)))
         (add-to-list
          'eglot-server-programs
          ,@forms ,append)
       (message "EXEC not found, not adding to EGLOT-SERVER-PROGRAMS: %s" ,exec)))

  (add-server-program-if-found "autotools-language-server" t
                               `((makefile-mode makefile-bsdmake-mode) ,command))
  (add-server-program-if-found "sql-language-server" t
                               `((sql-mode) ,command "up" "--method" "stdio"))

  :hook
  ((prog-mode org-mode markdown-mode) . eglot-ensure) ;; try LSP for all prog mode
  (before-save . (lambda ()
                   ;; autoformatting only behaves well for certain modes
                   ;; TODO: having trouble configuring typescript-language-server
                   (when (seq-contains-p '(go-mode
                                           go-ts-mode
                                           terraform-mode
                                           terraform-ts-mode) major-mode)
                     (let ((jsonrpc-default-request-timeout 5))
                       (condition-case nil
                           (eglot-code-action-organize-imports 1)
                         (error nil))
                       (condition-case nil
                           (eglot-format-buffer)
                         (error nil)))))))

;; Debug Adapter Protocol: https://github.com/svaante/dape
(use-package dape
  :after eglot
  :config
  ;; kubebuilder tests take a little bit to start
  (setq dape-request-timeout 30)
  ;; Debug Go file under point
  (add-to-list 'dape-configs
               `(dlv-current-file
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 fn dape-config-autoport
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd ,(lambda () (file-name-directory (buffer-file-name)))
                 port :autoport
                 :type "debug"
                 :request "launch"
                 :mode "debug"
                 :program ,(lambda () (buffer-file-name))))
  ;; Run Go unit test under point: https://github.com/svaante/dape/wiki#go---dlv
  (add-to-list 'dape-configs
               `(dlv-unit-test
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 fn dape-config-autoport
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-cwd-fn
                 port :autoport
                 :type "debug"
                 :request "launch"
                 :mode (lambda () (if (string-suffix-p "_test.go"   (buffer-name)) "test" "debug"))
                 :cwd dape-cwd-fn
                 :program (lambda () (if (string-suffix-p "_test.go"   (buffer-name))
                                         (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn)))
                                       (funcall dape-cwd-fn)))
                 :args (lambda ()
                         (require 'which-func)
                         (if (string-suffix-p "_test.go"   (buffer-name))
                             (when-let* ((test-name (which-function))
                                         (test-regexp (concat "^" test-name "$")))
                               (if test-name `["-test.run" ,test-regexp]
                                 (error "No test selected")))
                           []))))
  ;; Run Jest unit tests in buffer: https://github.com/svaante/dape/wiki#debug-jest-unit-test
  ;; I skip all the extra 'ensure' steps.
  (add-to-list 'dape-configs
               `(jest
                 modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 ensure dape-ensure-command
                 command (lambda ()
                           (if (string-suffix-p ".ts" (buffer-name))
                               "ts-node"
                             "node"))
                 command-cwd dape-command-cwd
                 command-args (,(expand-file-name
                                 (file-name-concat dape-adapter-dir
                                                   "js-debug"
                                                   "src"
                                                   "dapDebugServer.js"))
                               :autoport)
                 port :autoport
                 fn dape-config-autoport
                 :type "pwa-node"
                 :cwd dape-cwd
                 :program "node_modules/.bin/jest"
                 :args (lambda ()
                         (let ((file (dape-buffer-default)))
                           (if file
                               `["--runInBand" "--no-coverage" ,file]
                             (user-error "No file found"))))
                 :outputCapture "console"
                 :sourceMapRenames t
                 :pauseForSourceMap nil
                 :autoAttachChildProcesses t
                 :console "internalConsole"
                 :outputCapture "std"
                 :killBehavior "forceful"))
  ;; Java debugging: https://github.com/svaante/dape?tab=readme-ov-file#java---jdtls-with-java-debug-server-plugin
  ;; Also adding Lombok here, although not related to dape
  ;; See:
  ;; - https://github.com/joaotavora/eglot/discussions/888#discussioncomment-2384693
  ;; - https://github.com/joaotavora/eglot/discussions/868
  ;; - https://github.com/eclipse-jdtls/eclipse.jdt.ls?tab=readme-ov-file#running-from-command-line-with-wrapper-script
  (add-server-program-if-found "jdtls" nil
                               `((java-mode java-ts-mode) .
                                 (,command ,(concat "--jvm-arg=-javaagent:" (expand-file-name (file-name-concat dape-adapter-dir "lombok.jar")))
                                           :initializationOptions
                                           (:bundles [,(expand-file-name (file-name-concat dape-adapter-dir "com.microsoft.java.debug.plugin-0.52.0.jar"))])))))

;;
;; AI
;;

;; Integrate with AI Agents via ACP
;; https://agentclientprotocol.com/get-started/introduction
(use-package agent-shell
  :custom
  (agent-shell-github-acp-command '("copilot" "--acp" "--allow-all-tools"))
  (agent-shell-cursor-acp-command '("agent" "acp" "--yolo" "--trust"))
  :hook (agent-shell-mode . (lambda ()
                              (require 'server)
                              (unless (server-running-p)
                                (server-start)))))

;;
;; Misc.
;;

;; RSS Reader: https://github.com/skeeto/elfeed
(use-package elfeed
  :custom
  (url-queue-timeout 30)
  (elfeed-search-title-max-width 120)
  (elfeed-search-filter "@1-days-ago +unread")
  (elfeed-feeds '(("https://aws.amazon.com/about-aws/whats-new/recent/feed/" aws tech firehose)
                  ("https://aws.amazon.com/blogs/aws/feed/" aws tech)
                  ("https://blog.cloudflare.com/rss" cloudflare tech)
                  ("https://www.docker.com/feed/" docker tech)
                  ("https://kubernetes.io/docs/reference/issues-security/official-cve-feed/feed.xml" tech k8s cve)
                  ("https://kubernetes.io/feed.xml" tech k8s)
                  ("https://go.dev/blog/feed.atom" go tech)
                  ("https://cprss.s3.amazonaws.com/golangweekly.com.xml" go tech)
                  ("https://appliedgo.net/index.xml" go tech)
                  ("https://dave.cheney.net/feed" go tech)
                  ("https://eli.thegreenplace.net/feeds/all.atom.xml" go tech)
                  ("https://benhoyt.com/writings/rss.xml" go tech)
                  ("https://crawshaw.io/atom.xml" go tech)
                  ("https://jerf.org/iri/rss.xml" go tech)
                  ("https://www.alexedwards.net/static/feed.rss" go tech)
                  ("https://peps.python.org/peps.rss" python tech)
                  ("https://blog.python.org/feeds/posts/default" python tech)
                  ("https://www.python.org/downloads/feed.rss" python tech)
                  ("https://pythonspeed.com/atom.xml" python tech)
                  ;;("https://feeds.feedblitz.com/baeldung&x=1" java tech)
                  ;;("https://spring.io/blog.atom" java spring tech)
                  ("http://research.swtch.com/feed.atom" tech)
                  ("https://www.ardanlabs.com/blog/index.xml" tech)
                  ("https://engineering.fb.com/feed/" tech)
                  ("https://github.blog/engineering.atom" tech)
                  ("https://blog.janestreet.com/feed.xml" tech)
                  ("https://hacks.mozilla.org/feed/" tech)
                  ("https://netflixtechblog.com/feed" tech)
                  ;;("https://open.nytimes.com/feed" tech)
                  ;;("https://blogs.nvidia.com/blog/category/generative-ai/feed/" ai tech)
                  ("https://slack.engineering/rss" tech)
                  ("https://engineering.atspotify.com/feed" tech)
                  ("https://stackoverflow.blog/feed" tech)
                  ("https://news.ycombinator.com/rss" tech firehose)
                  ("https://feeds.feedburner.com/TheHackersNews" tech firehose)
                  ("https://feed.infoq.com/" tech)
                  ("https://huggingface.co/blog/feed.xml" tech ai)
                  ;;("https://techcrunch.com/feed/" news tech)
                  ;;("https://www.localfirstnews.com/rss/" tech)
                  ("https://wiredream.com/atom.xml" tech)
                  ("https://passo.uno/posts/index.xml" tech writing)
                  ("https://simonwillison.net/atom/everything/" tech ai)
                  ("https://qntm.org/rss.php" tech)
                  ("http://research.swtch.com/feed.atom" go tech)
                  ;;("https://mastodon.sdf.org/@beejjorgensen.rss" tech)
                  ("https://magazine.sebastianraschka.com/feed" tech ai)
                  ("https://www.allthingsdistributed.com/atom.xml" tech)
                  ("https://www.gilesthomas.com/feed/rss.xml" tech ai python)
                  ("https://sethmlarson.dev/feed" tech python)
                  ("https://commandcenter.blogspot.com/feeds/posts/default?alt=rss" tech go)
                  ("https://rachelbythebay.com/w/atom.xml" tech)
                  ("https://www.masteringemacs.org/feed" tech emacs)
                  ("https://nullprogram.com/feed/" tech)
                  ("https://queue.acm.org/rss/feeds/queuecontent.xml" tech)
                  ("https://www.seangoedecke.com/rss.xml" tech)
                  ("https://funcall.blogspot.com/feeds/posts/default" tech lisp)
                  ("https://blog.christianposta.com/feed.xml" tech)
                  ("https://lobste.rs/rss" tech firehose)
                  ("https://bubbles.town/feed" tech firehose)
                  ("https://antirez.com/rss" tech)
                  ("https://github.blog/feed/" tech github)
                  ("https://brianchambers.substack.com/feed" tech cfa)
                  ("https://developer.mozilla.org/en-US/blog/rss.xml" tech web)
                  ("https://lowendbox.com/feed/" tech)
                  ("https://kagifeedback.org/atom/t/release-notes" tech kagi)
                  ("https://blog.kagi.com/rss.xml" tech kagi)
                  ("https://inside.java/feed.xml" java tech)
                  ("https://feeds.feedburner.com/martinkl" tech)
                  ("https://www.jeffgeerling.com/blog.xml" tech)
                  ("https://www.drehmflight.com/blog-feed.xml" tech drones)
                  ("https://fedoramagazine.org/feed/" tech fedora)
                  ("https://ai.fedoraproject.org/feed/" tech ai fedora)
                  ("https://boristane.com/rss.xml" tech)
                  ("https://www.jamesshore.com/v2/feed" tech)
                  ("https://www.pcloadletter.dev/feed/feed.xml" tech)
                  ("https://jmmv.dev/feed.xml" tech)
                  ("https://thoughtspile.github.io/atom.xml" tech)
                  ("https://blog.lawrencejones.dev/feed.xml" tech)
                  ("https://stanbright.com/feed.xml" tech)
                  ("https://seanvoisen.com/feed.xml" tech)
                  ("https://norikitech.com/rss.xml" tech)
                  ("https://mitchellh.com/feed.xml" tech)
                  ("https://lmno.lol/alvaro/feed" tech emacs)
                  ("https://chriskiehl.com/rss.xml" tech java)
                  ("https://serce.me/feed.xml" tech)
                  ("https://claytonwramsey.com/blog/rss.xml" tech)
                  ("https://endler.dev/rss.xml" tech)
                  ("https://blog.miguelgrinberg.com/feed" tech python)
                  ("https://thisdavej.com/index.xml" tech python rust)
                  ("https://lukasrotermund.de/index.xml" tech)
                  ("https://technicalwriting.dev/rss.xml" tech writing)
                  ("https://notes.billmill.org/blog.atom.xml" tech)
                  ("https://www.jefago.com/feed/atom.xml" tech management)
                  ("https://staysaasy.com/feed.xml" tech management)
                  ("https://shkspr.mobi/blog/feed/atom" tech management)
                  ("https://www.bleepingcomputer.com/feed/" tech)
                  ("https://daniel.haxx.se/blog/feed/" tech)
                  ("https://www.wheresyoured.at/rss/" tech)
                  ("https://abcnews.go.com/abcnews/topstories" news firehose)
                  ("https://feeds.bbci.co.uk/news/world/rss.xml" news world firehose)
                  ("https://moxie.foxnews.com/google-publisher/latest.xml" news firehose)
                  ("https://www.wcnc.com/feeds/syndication/rss/news" news clt firehose)
                  ("https://news.kagi.com/world.xml" news world firehose)
                  ("https://news.kagi.com/usa.xml" news usa firehose)
                  ("https://news.kagi.com/business.xml" news business firehose)
                  ("https://news.kagi.com/tech.xml" news tech firehose)
                  ("https://news.kagi.com/science.xml" news science firehose)))
  :config
  ;; function to open entries in another window: https://github.com/skeeto/elfeed/pull/383/commits/9c15ba2549f31a484953964e33114d7833348569
  (defun elfeed-entry-other-window ()
    "In elfeed-search mode, open elfeed entry in the other window
if other window is present, else sensibly splits the frame if
there is only a single window and opens the elfeed entry in the
other window."

    (interactive)
    (if (get-buffer "*elfeed-search*")
        (progn
	      (split-window-sensibly (selected-window))
	      (switch-to-buffer-other-window "*elfeed-search*")
	      (call-interactively #'elfeed-search-show-entry)
	      (other-window 1)
	      (forward-line))
      (message "Start elfeed first!")))
  :bind
  (("C-c r" . elfeed)
   :map elfeed-search-mode-map
   ("o" . elfeed-entry-other-window)))

;;
;; Window and buffer management
;;

;; quickly cycle buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; quickly swap between windows
(windmove-default-keybindings)
(setq windmove-wrap-around nil)

;; See "Recommended Settings": https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)

;;
;; Other customizations
;;

(defun copy-buffer-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (if-let ((filename (buffer-file-name))
           (filename-absolute (expand-file-name filename)))
      (progn
        (kill-new filename-absolute)
        (message "Copied to kill ring: %s" filename-absolute))
    (message "Buffer is not visiting a file")))

(defun copy-project-path ()
  "Copy the current project's root directory to the kill ring."
  (interactive)
  (if-let ((current (project-current))
           (project-root (car (last current)))
           (project-root-absolute (expand-file-name project-root)))
      (progn
        (kill-new project-root-absolute)
        (message "Project root copied to kill ring: %s" project-root-absolute))
    (message "Not in a project")))

;; configure TRAMP: https://www.gnu.org/software/tramp/
(use-package tramp
  :ensure nil ;; included with Emacs
  :config
  ;; Add remote path to TRAMP path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Disable startup splash screen
(setq inhibit-splash-screen t
      inhibit-startup-message t)

;; enable line wrap
(global-visual-line-mode t)

;; show line numbers
(global-display-line-numbers-mode t)

;; show column numbers in status bar
(setq column-number-mode t)

;; add closing brackets & parens
(electric-pair-mode t)

;; add ruler
(setq display-fill-column-indicator-column 80) ;; default
(add-hook 'java-mode-hook (lambda () (setq-local display-fill-column-indicator-column 100))) ;; java
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Treat snake_case as one word
(global-superword-mode 1)

;; Load secrets with Emacs' built-in GPG support
;; See: https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources

(defun load-if-exists (f)
  "Load file F if it exists.  F is expanded to an absolute path."
  (when-let* ((expanded (expand-file-name f))
              (exists (file-exists-p expanded)))
	(load-file expanded)))

(load-if-exists "~/.emacs.d/secrets.el.gpg")

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; quickly swap from horizontal to vertical split & vice-versa
;; copied from here: https://stackoverflow.com/questions/14881020/emacs-shortcut-to-switch-from-a-horizontal-split-to-a-vertical-split-in-one-move
;; adapted to work with treemacs
(defun toggle-window-split ()
  (interactive)
  (let* ((content-windows
          (seq-remove (lambda (w)
                        (and (fboundp 'treemacs-is-treemacs-window?)
                             (treemacs-is-treemacs-window? w)))
                      (window-list)))
         (n (length content-windows)))
    (when (= n 2)
      (let* ((this-win (if (memq (selected-window) content-windows)
                           (selected-window)
                           (car content-windows)))
             (other-win (car (seq-remove (lambda (w) (eq w this-win)) content-windows)))
             (this-buf (window-buffer this-win))
             (other-buf (window-buffer other-win))
             (this-edges (window-edges this-win))
             (other-edges (window-edges other-win))
             (this-is-2nd (not (and (<= (car this-edges) (car other-edges))
                                    (<= (cadr this-edges) (cadr other-edges)))))
             (splitter (if (= (car this-edges) (car other-edges))
                           'split-window-horizontally
                           'split-window-vertically)))
        (delete-window other-win)
        (select-window this-win)
        (let ((new-win (funcall splitter)))
          (if this-is-2nd
              (progn
                (set-window-buffer this-win other-buf)
                (set-window-buffer new-win this-buf)
                (select-window new-win))
              (set-window-buffer this-win this-buf)
              (set-window-buffer new-win other-buf)
              (select-window this-win)))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; allow undo + redo of window layout changes with C-c <left> and C-c <right>
(winner-mode 1)

;; undelete frames with M-x undelete-frame
(undelete-frame-mode 1)

;; remember window configurations used in every tab
(tab-bar-history-mode 1)

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

;; don't litter directory with backups and autosaves
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosaves/\\1") t))
      delete-old-versions t
      auto-save-interval 20)

;; allow commands in minibuffer
(setq enable-recursive-minibuffers t)

;; Delete trailing whitespace and trailing empty line from files.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Disallow splitting existing windows with SPLIT-WINDOW-SENSIBLY (dired, magit, etc.)
;; See: https://emacs.stackexchange.com/a/15123
(setq split-width-threshold nil
      split-height-threshold nil)

;; horizontal line highlighting
(global-hl-line-mode 1)

;; automatically revert buffers when files on disk change
(global-auto-revert-mode 1)

;; Render PDFs more legibly @ 300dpi
(setq doc-view-resolution 300)

;; Enable smooth scrolling pixel-by-pixel vs line-by-line
(pixel-scroll-mode +1)
(setq pixel-dead-time 0
      pixel-resolution-fine-flag t
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

;; more gracefully handle files with long lines
(global-so-long-mode 1)

;; I know this is bad, but...
(setq warning-minimum-level :emergency)

;;; init.el ends here
