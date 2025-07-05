;;; gptel-tools -- additional tools for gptel.

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t; -*-

(require 'gptel)

;;; common

(defun gt--join-strings-into-csv (strings)
  "Join a list of STRINGS into a single CSV-formatted string.
Strings containing commas or double quotes are properly escaped,
but already escaped characters are ignored."
  (mapconcat
   (lambda (s)
     (if (string-match-p "[,\"]" s)
         (concat "\"" (replace-regexp-in-string "\\([^\"]\\)\"" "\\1\"\"" s) "\"")
       s))
   strings
   ","))

(defun gt--load-file-into-ctx-string (path)
  "Load the file at PATH into a single string.
The format of the returned string is suitable for loading file contents
into context as text, including the file name for providing links in
responses."
  (with-temp-buffer
         (insert-file-contents path)
         (concat "file:" path "\n"
                 (mapconcat (lambda (row)
                              (concat ">" row "\n"))
                            (split-string (buffer-string) "\n")))))

;;; elfeed tools

(require 'elfeed)

(defun gt--elfeed-get-entries ()
  "Get all elfeed entries using ELFEED-SEARCH-FILTER VAR."
  (let (entries)
    (with-elfeed-db-visit (entry feed)
                          (when (elfeed-search-filter (elfeed-search-parse-filter elfeed-search-filter) entry feed)
                            (push entry entries)))
    entries))

(gptel-make-tool
 :name "elfeed_get_headlines"
 :function (lambda ()
             (string-join
              (mapcar #'elfeed-entry-title (gt--elfeed-get-entries))
              "\n"))
 :description "Return news headlines from Elfeed RSS feed."
 :category "rss")

(gptel-make-tool
 :name "elfeed_get_entries"
 :function (lambda ()
             (string-join
              (mapcar (lambda (entry)
                        (gt--join-strings-into-csv `(,(elfeed-entry-title entry)
                                                     ,(elfeed-entry-link entry))))
                      (gt--elfeed-get-entries))
              "\n"))
 :description "Return news headlines and urls from Elfeed RSS feed."
 :category "rss")

;;; deft tools

(require 'deft)

(defun gt--deft-search (str)
  "Use deft to search configured files for regexp STR and return filenames."
  (let ((orig-buffer (current-buffer)) ;; (deft) will swap to dedicated buffer
        (result))
    (unwind-protect ;; swap works even on exceptions
        (progn
          (deft) ;; needed to initialize deft
          (deft-filter str t)
          (setq result (deft-current-files))))
    (switch-to-buffer orig-buffer)
    result))

(gptel-make-tool
 :name "deft_search_files"
 :function (lambda (term)
             (when-let ((found (gt--deft-search term)))
               (string-join
                (mapcar #'gt--load-file-into-ctx-string found)
                "\n\n")))
 :description "Use deft to search contents of org-mode notes and return filenames and contents."
 :args (list '(:name "term"
               :type string
               :description "term or regexp for which to search in file contents"))
 :category "org")

(provide 'gptel-tools)
;;; gptel-tools.el ends here
