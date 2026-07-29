;;; early-init.el --- Pre-init startup tuning -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded before package.el and before the first frame is created.  This is the
;; only place where GC tuning, `file-name-handler-alist' suppression and frame
;; chrome can take effect for the whole of startup.
;;
;; This is not a configuration module -- it is the startup file Emacs itself
;; defines.  Durable configuration still belongs in init.el; keep this file to
;; things that provably cannot work later.

;;; Code:

;; Startup loads several hundred features; the default 800KB threshold collects
;; repeatedly while doing it.  `gcmh-mode' (init.el) owns the steady-state
;; values, so only `gc-cons-percentage' is restored here.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-percentage 0.1)))

;; Every `load' walks this alist.  Nothing loaded during init needs a handler
;; *except* the GPG-encrypted secrets file, which init.el loads inside a `let'
;; that rebinds this variable back -- see `early-init--file-name-handler-alist'
;; there.  Restored on `emacs-startup-hook' so TRAMP and friends work after.
(defvar early-init--file-name-handler-alist file-name-handler-alist
  "Value of `file-name-handler-alist' before startup suppressed it.")
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist early-init--file-name-handler-alist)))

;; Decide frame chrome before the first frame exists, so it is never drawn and
;; then removed.  init.el still calls `menu-bar-mode'/`tool-bar-mode' to keep
;; the mode variables consistent; these only stop the initial flicker.
(push '(tool-bar-lines . 0) default-frame-alist)
(unless (memq initial-window-system '(mac ns))
  (push '(menu-bar-lines . 0) default-frame-alist))

;; Don't let Emacs resize the frame to fit chrome changes during startup.
(setq frame-inhibit-implied-resize t
      inhibit-splash-screen t
      inhibit-startup-message t)

;;; early-init.el ends here
