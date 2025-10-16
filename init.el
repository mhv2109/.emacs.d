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
   '(auto-package-update corfu counsel dape deft dockerfile-mode
                         doom-themes editorconfig eldoc-box elfeed
                         elysium embark embark-consult
                         exec-path-from-shell fish-mode
                         flymake-golangci flymake-grammarly forge gcmh
                         git-link go-mode gotest gptel gptel-commit
                         hotfuzz ivy lua-mode macher magit marginalia
                         mcp minimap neotree nov ob-go org-remark
                         org-roam org-web-tools paredit projectile
                         protobuf-mode pyvenv pyvenv-auto
                         rainbow-delimiters rg terraform-mode
                         treesit-auto typescript-mode ultra-scroll
                         vline vterm which-key yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((flymake-golangci :url
                       "https://github.com/storvik/flymake-golangci.git")
     (macher :url "https://github.com/kmontag/macher.git")
     (aider :url "https://github.com/tninja/aider.el")))
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

(use-package doom-themes ;; Themes from doomacs: https://github.com/doomemacs/themes
  :config
  (load-theme 'doom-gruvbox t))

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

;; project management utilities: https://github.com/bbatsov/projectile
(use-package projectile
  :custom
  (projectile-switch-project-action 'neotree-projectile-action)
  :bind-keymap
  (("C-c p" . projectile-command-map))
  :config
  (defun copy-projectile-project-path ()
    "Copy the current Projectile project root directory to the kill ring."
    (interactive)
    (if (projectile-project-p)
        (let ((project-root (projectile-project-root)))
          (kill-new project-root)
          (message "Project root copied to kill ring: %s" project-root))
      (message "Not in a Projectile project")))
  :hook
  (after-init . projectile-mode))

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
    (when-let ((found (executable-find "ipython")))
      (setq python-shell-interpreter found
            python-shell-interpreter-args (concat "--no-confirm-exit --simple-prompt --InteractiveShell.display_page=True --InteractiveShell.autosuggestions_provider=None -i " (file-name-directory user-init-file) "autoload.ipy"))))
  (set-python-shell-interpreter-ipython))

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
  :mode ("README\\.md\\'" . gfm-mode))

;; lua editing: https://github.com/immerrr/lua-mode
(use-package lua-mode)

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
         (dailies-dir (concat org-dir "/dailies"))
         (resources-dir (concat org-dir "/resources"))
         (projects-dir (concat org-dir "/projects"))
         (areas-dir (concat org-dir "/areas")))
    (setq org-agenda-files (list org-dir
                                 dailies-dir
                                 resources-dir
                                 projects-dir
                                 areas-dir)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (go . t)))
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
  :hook
  ;; auto-format all tables on save
  (before-save . (lambda ()
                   (org-table-map-tables 'org-table-align))))

(use-package ox-md ;; markdown backend for org-mode
  :after org
  :ensure nil)

(use-package ob-go ;; org-babel support for Go: https://github.com/pope/ob-go
  :after org)

;; retrieve web pages as org files: https://github.com/alphapapa/org-web-tools
(use-package org-web-tools
  :after org
  :custom
  (org-web-tools-pandoc-sleep-time 1.0)) ;; 5x longer than default

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
                                                         "#+title: ${title}\n#+author: ${author}\n#+edition: ${edition}\n#+publisher: ${publisher}\n#+year: ${year}\n#+created: %U\n#+filetags: :resources:\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("w" "website (resource, capture page)" plain "%(org-web-tools--url-as-readable-org \"${ref}\")"
                                      :target (file+head "resources/${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :resources:\n")
                                      :unnarrowed t
                                      :empty-lines 1)
                                     ("l" "website (resource, link only)" plain "* ${title}\n\nLink: ${ref}\n%?"
                                      :target (file+head "resources/${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :resources:\n")
                                      :unnarrowed t
                                      :empty-lines 1))
        org-roam-dailies-capture-templates '(("j" "journal" entry "* %?"
                                              :target (file+head "%<%Y-%m-%d>_daily.org"
                                                                 "#+title: %<%Y-%m-%d>\n")
                                              :empty-lines 1
                                              :unnarrowed t)
                                             ("t" "todo" entry "* TODO %?"
                                              :target (file+head "%<%Y-%m-%d>_daily.org"
                                                                 "#+title: %<%Y-%m-%d>\n")
                                              :empty-lines 1
                                              :unnarrowed t)))

  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)

  ;; add #+filetags: to org-node-find
  (setq org-roam-node-display-template
        (concat (propertize "${tags} " 'face 'org-tag)
                "${title}"))

  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i"    . completion-at-point))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map))

;; Full Text Search for org files: https://jblevins.org/projects/deft/
;; Config adapted from: https://www.orgroam.com/manual.html#Full_002dtext-search-with-Deft
(use-package deft
  :bind ("<f9>" . deft)
  :commands (deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  (deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|archived\\)$"))

;; Read EPUB from emacs: https://depp.brause.cc/nov.el/
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

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
                     (condition-case nil
                         (eglot-code-action-organize-imports 1)
                       (error nil))
                     (eglot-format-buffer)))))

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

;; LLM Chat client: https://github.com/karthink/gptel
(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-track-media t)
  (gptel-include-tool-results t)
  :config
  ;; configuration for making chat more legible: https://github.com/karthink/gptel?tab=readme-ov-file#additional-configuration
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* User:\n\n"
        (alist-get 'org-mode gptel-response-prefix-alist) "** Response:\n\n")
  ;; configure Ollama, if installed
  (when (locate-file "ollama" exec-path exec-suffixes)
    (setq gptel-ollama-backend (gptel-make-ollama "Ollama"
                                 :host "localhost:11434"
                                 :stream t
                                 :models '(qwen3-coder:30b ;; coding assistant
                                           qwen3:30b-a3b-instruct-2507-q4_K_M ;; general-purpose (non-thinking)
                                           gpt-oss:20b ;; general-purpose (thinking)
                                           mistral-nemo:12b ;; general-purpose (non-thinking)
                                           gemma3n:e2b ;; for resource-constrained devices (no tools
                                           llama3.2:3b ;; for resource-constrained devices (with tools)
                                           )
                                 :request-params '(:options (:num_ctx 8192)))))
  ;; configure Anthropic, if configured
  (when-let (api-key (getenv "ANTHROPIC_API_KEY"))
    (setq gptel-anthropic-backend (gptel-make-anthropic "Claude"
                                    :stream t
                                    :key api-key)
          gptel-anthropic-thinking-backend (gptel-make-anthropic "Claude-thinking"
                                             :stream t
                                             :key api-key
                                             :header (lambda () (when-let* ((key (gptel--get-api-key)))
                                                                  `(("x-api-key" . ,key)
                                                                    ("anthropic-beta" . "pdfs-2024-09-25")
                                                                    ("anthropic-beta" . "output-128k-2025-02-19")
                                                                    ("anthropic-beta" . "prompt-caching-2024-07-31"))))
                                             :request-params '(:thinking (:type "enabled" :budget_tokens 32000)
                                                                         :max_tokens 64000))))
  ;; configure Copilot Chat (I get for free from work), uses OAuth
  (setq gptel-copilot-backend (gptel-make-gh-copilot "Copilot")
        gptel-backend gptel-copilot-backend
        gptel-model 'claude-sonnet-4)
  :bind
  (("C-c g" . gptel-menu)))

;; Integrate with MCP servers: https://github.com/lizqwerscott/mcp.el
(use-package mcp
  :if (version<= "30.1" emacs-version)
  :custom
  ;; not sure why, but getting better results with mcp-remote vs. using :url
  (mcp-hub-servers `(;; general
                     ("fetch" . (:command "docker" :args ("run" "-i" "--rm" "mcp/fetch:latest"))) ;; fetch web content
                     ("duckduckgo" . (:command "docker" :args ("run" "-i" "--rm" "mcp/duckduckgo:latest"))) ;; search web content
                     ("sequential-thinking" . (:command "docker" :args ("run" "-i" "--rm" "mcp/sequentialthinking:latest"))) ;; break down complex tasks into steps
                     ("markitdown" . (:command "uvx" :args ("markitdown-mcp"))) ;; convert files to markdown for text analysis -- uses uvx since it generally needs access to the filesystem
                     ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(getenv "HOME")))) ;; grant filesystem access to home directory
                     ;; programming libraries, platforms, and tools
                     ("serena" . (:command "uvx" :args ("--from" "git+https://github.com/oraios/serena" "serena" "start-mcp-server" "--transport" "stdio" "--enable-web-dashboard" "false"))) ;; coding agent toolkit implemented as MCP server: https://github.com/oraios/serena (Docker image doesn't really work well, this MCP is blessed by Cybersecurity)
                     ("context7" . (:command "docker" :args ("run" "-i" "--rm"
                                                             "-e" "MCP_TRANSPORT=stdio" ;; use stdin/stdout vs HTTP API
                                                             "mcp/context7:latest"))) ;; Library docs
                     ("aws" . (:command "docker" :args ("run" "-i" "--rm" "mcp/aws-documentation:latest"))) ;; AWS documentation
                     ("cloudflare" . (:command "npx" :args ("-y" "mcp-remote" "https://docs.mcp.cloudflare.com/sse"))) ;; Cloudflare docs (uses mcp-remote)
                     ("terraform" . (:command "docker" :args ("run" "-i" "--rm" "hashicorp/terraform-mcp-server:latest"))) ;; Terraform and registry docs
                     ))
  :config
  (require 'mcp-hub)
  (require 'gptel-integrations)
  :hook
  (gptel-mode . (lambda ()
                  ;; choose tools interactively
                  (gptel-mcp-connect nil nil t))))

;; AI-generated commit messages with gptel: https://github.com/lakkiy/gptel-commit
(use-package gptel-commit
  :after gptel)

;;
;; Misc.
;;

;; RSS Reader: https://github.com/skeeto/elfeed
(use-package elfeed
  :custom
  (elfeed-search-title-max-width 120)
  (elfeed-search-filter "@1-days-ago +unread")
  (elfeed-feeds '(("https://aws.amazon.com/about-aws/whats-new/recent/feed/" aws tech)
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
                  ("https://peps.python.org/peps.rss" python tech)
                  ("https://feeds.feedburner.com/PythonInsider" python tech)
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
                  ("https://shopify.engineering/blog.atom" tech)
                  ("https://slack.engineering/rss" tech)
                  ("https://engineering.atspotify.com/feed" tech)
                  ("https://stackoverflow.blog/feed" tech)
                  ("https://news.ycombinator.com/rss" tech)
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
                  ("https://lobste.rs/rss" tech)
                  ("https://antirez.com/rss" tech)
                  ("https://github.blog/feed/" tech github)
                  ("https://brianchambers.substack.com/feed" tech cfa)
                  ("https://developer.mozilla.org/en-US/blog/rss.xml" tech web)
                  ("https://lowendbox.com/feed/" tech)
                  ("https://kagifeedback.org/atom/t/release-notes" tech kagi)
                  ("https://abcnews.go.com/abcnews/topstories" news)
                  ("https://feeds.bbci.co.uk/news/world/rss.xml" news)
                  ("https://moxie.foxnews.com/google-publisher/latest.xml" news)
                  ("https://www.wcnc.com/feeds/syndication/rss/news" news clt)))
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
;; Other customizations
;;

(defun copy-buffer-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (progn
        (kill-new filename)
        (message "Copied to kill ring: %s" filename))
    (message "Buffer is not visiting a file")))

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

;; allow undo + redo of window layout changes with C-c <left> and C-c <right>
(winner-mode 1)

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
