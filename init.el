(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

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
    evil))

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

;; Enable vim keybindings
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil use-package magit exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; spellchecking
(add-hook 'text-mode-hook (lambda () (flyspell-mode t)))
(add-hook 'prog-mode-hook (lambda () (flyspell-mode t)))
