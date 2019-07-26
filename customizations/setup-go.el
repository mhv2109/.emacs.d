(defun my-go-mode-hook ()
  ;; call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; autocomplete
  (auto-complete-mode 1)
  ;; set GOPATH and GOROOT from shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "GOROOT")
    (exec-path-from-shell-initialize)))

(add-hook 'go-mode-hook 'my-go-mode-hook)
(with-eval-after-load 'go-mode
    (require 'go-autocomplete))
