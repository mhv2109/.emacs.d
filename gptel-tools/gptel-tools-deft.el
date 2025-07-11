;;; gptel-tools-deft -- deft-related tools. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'deft)
(require 'gptel-tools-tfidf)

(defun gt--deft-search (term)
  "Use deft to search configured files for TERM and return filenames.
TERM is split on spaces and each individual element is used in search.
Unique results are combined."
  (let ((splitted (gt--tfidf-tokenize term))
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
               ;; score each doc based on similarity w/ TF-IDF
               (let* ((documents (append (list term) (mapcar (lambda (path)
                                                               (with-temp-buffer
                                                                 (insert-file-contents path)
                                                                 (buffer-string)))
                                                             found)))
                      (vectorizer (make-gt--tfidf-vectorizer))
                      (matrix (gt--tfidf-fit-transform documents vectorizer))
                      (search-vector (cl-first matrix))
                      (document-matrix (cl-rest matrix)))
                 (json-encode (list :files (vconcat (cl-loop for i from 0 below (length found)
                                                             collect (list :path (nth i found) :score (gt--tfidf-cosine-similarity search-vector (nth i document-matrix))))))))))
 :description "Use deft to search contents of org-mode notes and return filenames and cosine similarity scores in JSON format."
 :args (list '(:name "term"
                     :type string
                     :description "term for which to search in file contents"))
 :category "org")

(provide 'gptel-tools-deft)

;;; gptel-tools-deft.el ends here
