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
 '(auth-source-save-behavior 'ask)
 '(package-selected-packages
   '(aider copilot org-remark-nov org-remark-eww org-remark-info nov org-remark org-web-tools flymake-grammarly eldoc-box elfeed neotree git-link forge elpy corfu deft ellama gcmh org-roam projectile vline counsel ivy markdown-mode gotest gotest.el dape hotfuzz lsp-grammarly which-key marginalia protobuf-mode lsp-java terraform-mode rainbow-delimiters paredit fuzzy auto-complete dash-at-point treesit-auto ob-go fish-mode yasnippet auto-package-update dockerfile-mode org-drill editorconfig codeium typescript-mode python-mode lsp-python-ms poetry use-package-ensure dap-dlv-go flyspell-mode icicles yaml-mode dap-mode lsp-ui lsp-mode go-mode use-package magit exec-path-from-shell))
 '(package-vc-selected-packages '((aider :url "https://github.com/tninja/aider.el")))
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
  (load-theme 'doom-horizon t))

;; Syntax highlighting, built-in
(use-package flymake
  :ensure nil
  :hook ((prog-mode text-mode) . flymake-mode))

;; autocomplete using corfu-mode: https://github.com/minad/corfu
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-popupinfo-delay '(0.5 . 0.5))
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

 ;; file tree UI: https://github.com/jaypei/emacs-neotree
(use-package neotree
  :bind ("<f8>" . neotree-toggle)
  :custom
  (neo-smart-open t)
  (neo-show-hidden-files t)
  (neo-autorefresh t)
  (neo-window-width 48)
  (neo-vc-integration '(face char)))

;; project management utilities: https://github.com/bbatsov/projectile
(use-package projectile
  :after neotree
  :custom
  (projectile-switch-project-action 'neotree-projectile-action)
  :config
  (projectile-mode 1)
  ;; setup keybindings
  (if (memq window-system '(mac ns x))
      (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; Recommended keymap prefix on macOS
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map) ;; Recommended keymap prefix on Windows/Linux
    ))

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
  :init
  (marginalia-mode))

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

;;
;; Git
;;

;; Git integration
(use-package magit
  :pin melpa-stable
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1) ;; open magit buffer in same window: https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfunction
  )

;; Git forge (GitHub) integration: https://magit.vc/manual/forge/
(use-package forge
  :after magit
  :pin melpa-stable)

;; GitHub Permalink at Point (what I was using github.el for):
(use-package git-link
 :after magit
 :pin melpa-stable)

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

;; major mode for typescript: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode)

(use-package dockerfile-mode ;; Syntax highlighting for Dockerfiles: https://github.com/spotify/dockerfile-mode
  )

(use-package fish-mode ;; https://github.com/wwwjfy/emacs-fish
  )

(use-package paredit ;; Lisp programming conveniences: http://paredit.org/
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . paredit-mode))

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
  ;; if ipython is available, use it w/ autoloads
  ;; autoloads lets you use a lisp-like repl-driven development where you can load a buffer into the repl and modify imported modules
  (when-let ((found (executable-find "ipython")))
    (setq python-shell-interpreter found
          python-shell-interpreter-args (concat "--no-confirm-exit --simple-prompt --InteractiveShell.display_page=True --InteractiveShell.autosuggestions_provider=None -i " (file-name-directory user-init-file) "autoload.ipy"))))

;; Python IDE for emacs: https://github.com/jorgenschaefer/elpy
(use-package elpy
  :init
  (elpy-enable))

;; markdown-mode: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

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
  (setq org-archive-location (concat (file-truename org-directory) "/archived/%s::")))

(use-package ox-md ;; markdown backend for org-mode
  :after org
  :ensure nil)

(use-package ob-go ;; org-babel support for Go: https://github.com/pope/ob-go
  :after org)

(add-to-list 'load-path "~/.emacs.d/org-drill/") ;; installed as a Git submodule (includes a bugfix)
(use-package org-drill ;; Spaced repetition for Org mode: https://orgmode.org/worg/org-contrib/org-drill.html
  :after org
  :ensure nil)

;; retrieve web pages as org files: https://github.com/alphapapa/org-web-tools
(use-package org-web-tools
  :after org)

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
                                                         "#+title: ${title}\n#+author: ${author-lastname}, ${author-firstname}\n#+edition: ${edition}\n#+publisher: ${publisher}\n#+year: ${year}\n#+created: %U\n#+filetags: :resources:\n")
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
  :after (org org-roam)
  :bind ("<f9>" . deft)
  :commands (deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; Read EPUB from emacs: https://depp.brause.cc/nov.el/
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Highlight and annotate text and org files: https://github.com/nobiot/org-remark
;; docs: https://nobiot.github.io/org-remark/
(use-package org-remark
  :after (org nov)
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
  :init
  (org-remark-global-tracking-mode +1)
  :config
  ;; add magenta highlighter
  (org-remark-create "magenta-highlighter"
                     '(:background "dark magenta")
                     '(CATEGORY "important")))

;; org-remark integrations
(use-package org-remark-info
  :after (org-remark info)
  :ensure nil
  :config (org-remark-info-mode +1))
(use-package org-remark-eww
  :after (org-remark  eww)
  :ensure nil
  :config (org-remark-eww-mode +1))
(use-package org-remark-nov
  :after (org-remark nov)
  :ensure nil
  :config (org-remark-nov-mode +1))

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

;; GitHub Copilot: https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (global-set-key (kbd "M-C-<tab>") 'copilot-mode))

;; Integrate with Ollama: https://github.com/s-kostyaev/ellama
(use-package ellama
  :if (locate-file "ollama" exec-path exec-suffixes) ;; install only if Ollama is installed: https://ollama.com/
  )

;; Integrate Grammarly with Flymake: https://github.com/emacs-grammarly/flymake-grammarly
(use-package flymake-grammarly
  :config (grammarly-load-from-authinfo) ;; See: https://github.com/emacs-grammarly/grammarly
  :hook (text-mode . (lambda ()
                       (when (and (buffer-file-name) (< 0 (buffer-size)) ;; this prevents flymake-grammarly from loading in temp buffers (used by org-web-tools)
                                  (not (memq major-mode '(yaml-mode yaml-ts-mode)))) ;; this prevents flymake-grammarly from loading in YAML buffers
                         (flymake-grammarly-load)))))

;; AI Pair Programming: https://github.com/tninja/aider.el
;; https://aider.chat/
(unless (package-installed-p 'aider)
  (package-vc-install '(aider :url "https://github.com/tninja/aider.el")))
(use-package aider
  :ensure nil
  :config
  ;; externalize aider config with standard config file
  (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml"))))

;;
;; Misc.
;;

;; RSS Reader: https://github.com/skeeto/elfeed
(use-package elfeed
  :custom
  (elfeed-search-title-max-width 120)
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
                  ("https://peps.python.org/peps.rss" python tech)
                  ("https://feeds.feedburner.com/PythonInsider" python tech)
                  ("https://www.python.org/downloads/feed.rss" python tech)
                  ("https://pythonspeed.com/atom.xml" python tech)
                  ("https://feeds.feedblitz.com/baeldung&x=1" java tech)
                  ("https://spring.io/blog.atom" java spring tech)
                  ("https://dev.to/feed" tech)
                  ("http://research.swtch.com/feed.atom" tech)
                  ("https://www.ardanlabs.com/blog/index.xml" tech)
                  ("https://engineering.fb.com/feed/" tech)
                  ("https://github.blog/engineering.atom" tech)
                  ("https://blog.janestreet.com/feed.xml" tech)
                  ("https://hacks.mozilla.org/feed/" tech)
                  ("https://netflixtechblog.com/feed" tech)
                  ("https://open.nytimes.com/feed" tech)
                  ("https://blogs.nvidia.com/blog/category/generative-ai/feed/" ai tech)
                  ("https://shopify.engineering/blog.atom" tech)
                  ("https://slack.engineering/rss" tech)
                  ("https://engineering.atspotify.com/feed" tech)
                  ("https://stackoverflow.blog/feed" tech)
                  ("https://news.ycombinator.com/rss" tech)
                  ("https://feed.infoq.com/" tech)
                  ("https://huggingface.co/blog/feed.xml" tech ai)
                  ("https://techcrunch.com/feed/" news tech)
                  ("https://www.localfirstnews.com/rss/" tech)
                  ("https://wiredream.com/atom.xml" tech)
                  ("https://passo.uno/posts/index.xml" tech writing)
                  ("https://simonwillison.net/atom/everything/" tech ai)
                  ("https://qntm.org/rss.php" tech)
                  ("http://research.swtch.com/feed.atom" go tech)
                  ("https://mastodon.sdf.org/@beejjorgensen.rss" tech)
                  ("https://magazine.sebastianraschka.com/feed" tech ai)
                  ("https://abcnews.go.com/abcnews/topstories" news)
                  ("https://feeds.bbci.co.uk/news/world/rss.xml" news)
                  ("https://moxie.foxnews.com/google-publisher/latest.xml" news)
                  ("https://www.wcnc.com/feeds/syndication/rss/news" news clt)))
  :bind
  ("C-c r" . elfeed))

;;
;; Other customizations
;;

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
(unless backup-directory-alist ;; don't litter directory with backups and autosaves, but still backup and auto-save
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

;; allow commands in minibuffer
(setq enable-recursive-minibuffers t)

;; Delete trailing whitespace and trailing empty line from files.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Disallow splitting existing windows with SPLIT-WINDOW-SENSIBLY (dired, magit, etc.)
;; See: https://emacs.stackexchange.com/a/15123
(setq split-width-threshold nil
      split-height-threshold nil)

;; Save window and buffer layout on exit:
;; - https://bmag.github.io/2015/12/26/desktop.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(when (display-graphic-p) ;; only save desktop when running in GUI mode
  (desktop-save-mode 1)
  (setq desktop-path (list (concat user-emacs-directory ".cache"))
        desktop-save 'ask))

;; horizontal line highlighting
(global-hl-line-mode 1)

;; automatically revert buffers when files on disk change
(global-auto-revert-mode 1)

;; Render PDFs more legibly @ 300dpi
(setq doc-view-resolution 300)



;; I know this is bad, but...
(setq warning-minimum-level :emergency)
