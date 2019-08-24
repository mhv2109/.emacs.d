(defvar gopath-initialized nil "Only set GOPATH and GOROOT once")

(defun my-go-mode-hook ()
  ;; Use goimports instead of gofmt
  (setq gofmt-command "goimports")
  ;; call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; autocomplete
  (auto-complete-mode 1)
  ;; set GOPATH and GOROOT from shell
  (setup-gopath)
  ;; disable company-mode for go files
  (company-mode -1))

(defun setup-gopath ()
  ;; set GOPATH and GOROOT from shell
  (when (and (memq window-system '(mac ns x)) (not gopath-initialized))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "GOROOT")
    (exec-path-from-shell-initialize)
    (setq gopath-initialized t)))

(add-hook 'go-mode-hook 'my-go-mode-hook)
(with-eval-after-load 'go-mode
    (require 'go-autocomplete))
