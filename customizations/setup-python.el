(defun my-python-hook ()
  ;; enable elpy
  (elpy-enable)
  ;; Use flycheck instead of flymake with elpy
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;; Setup autopep8
  (require 'py-autopep8)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  ;; IPython integration
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
  ;; miniconda config
  (require 'conda)
  (custom-set-variables
   '(conda-anaconda-home (expand-file-name "~/miniconda3"))))

(add-hook 'python-mode-hook 'my-python-hook)
