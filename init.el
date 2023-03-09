(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(use-package . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(evil . "melpa-stable") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    ;; git integration
    magit

    ;; for package configuration
    use-package

    ;; vim keybindings
    evil

    ;; major mode for YAML
    yaml-mode

    ;; major mode for golang
    go-mode

    ;; Language Server Protocol support and recommended packages
    lsp-mode
    lsp-ui   ;; intellisense-like context hover
    flycheck ;; syntax highlighting
    company  ;; autocomplete
    dap-mode ;; lsp, but for debuggers
    ))

;; Locally installed packages
(add-to-list 'load-path "~/.emacs.d/icicles/")

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (memq window-system '(mac ns x))
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode dap-mode company flycheck lsp-ui lsp-mode go-mode evil use-package magit exec-path-from-shell))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; icicles: https://www.emacswiki.org/emacs/Icicles
(require 'icicles)
(icy-mode 1)

;; Enable vim keybindings
(evil-mode 1)

;; Disable startup splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Functions to insert the current date for org-mode doc headers
(require 'calendar)

(defun insdate-insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
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
(global-set-key "\C-x\M-t" `insdate-insert-current-datetime)

;; load org mode
(require 'org)
(setq org-log-done t)

;; Update TODO states
(setq org-todo-keywords '("TODO" "IN PROGRESS" "|" "DONE" "DEFERRED" "DELEGATED"))

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

;; spellchecking
(add-hook 'text-mode-hook (lambda () (flyspell-mode t)))
;; (add-hook 'prog-mode-hook (lambda () (flyspell-mode t))) ;; TODO: make this smarter about code vs. comments
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; generate TAGS table file with etags
(defun etags (d &optional append?)
  "Uses etags to generate a TAGS table file in directory D. If APPEND? is truthy, equivalent of calling etags with -a flag."
  (interactive "F")
  (let ((command (string-join (list "find . -not \\( -path \"./.git\" -prune \\) -type f | xargs etags" (if append? "-a" "")) " ")) 
	(default-directory d))
    (shell-command-to-string command)))

;; global autocomplete
(add-hook 'after-init-hook 'global-company-mode)

;; LSP
(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp) ;; try LSP mode for all prog-mode
(add-hook 'before-save-hook #'lsp-format-buffer) ;; format on save
(use-package lsp-ui)
(setq gc-cons-threshold 100000000) ;; See: https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(setq read-process-output-max (* 1024 1024)) ;; 1mb, See: https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process

;; DAP
(require 'dap-dlv-go) ;; Go support

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; go-mode
(setq gofmt-command "goimports")
