(defun my-go-mode-hook ()
  ;; call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; autocomplete
  (auto-complete-mode 1)
)

(add-hook 'go-mode-hook 'my-go-mode-hook)
(with-eval-after-load 'go-mode
    (require 'go-autocomplete))
