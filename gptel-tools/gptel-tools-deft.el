;;; gptel-tools-deft -- deft-related tools.

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t; -*-

(require 'gptel)
(require 'deft)

(defun gt--deft-search (str)
  "Use deft to search configured files for regexp STR and return filenames."
  (let ((orig-buffer (current-buffer)) ;; (deft) will swap to dedicated buffer
        (result))
    (unwind-protect ;; swap works even on exceptions
        (progn
          (deft) ;; needed to initialize deft
          (deft-filter str t)
          (setq result (deft-current-files)))
      (switch-to-buffer orig-buffer))
    result))

(gptel-make-tool
 :name "deft_search_files"
 :function (lambda (term)
             (when-let ((found (gt--deft-search term)))
               (string-join found "\n")))
 :description "Use deft to search contents of org-mode notes and return filenames."
 :args (list '(:name "term"
                     :type string
                     :description "term for which to search in file contents"))
 :category "org")

(provide 'gptel-tools-deft)

;;; gptel-tools-deft.el ends here
