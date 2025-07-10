;;; gptel-tools-deft -- deft-related tools.

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t; -*-

(require 'gptel)
(require 'deft)

(defun gt--deft-search (term)
  "Use deft to search configured files for TERM and return filenames.
TERM is split on spaces and each individual element is used in search.
Unique results are combined."
  (let ((splitted (split-string term))
        (orig-buffer (current-buffer)) ;; (deft) will swap to dedicated buffer
        (result))
    (dolist (element splitted)
      (unwind-protect ;; swap works even on exceptions
          (progn
            (deft) ;; needed to initialize deft
            (deft-filter element t)
            (dolist (filename (deft-current-files))
              (when (not (member filename result))
                (add-to-list 'result filename))))
        (switch-to-buffer orig-buffer)))
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
