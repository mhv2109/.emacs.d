;;; gptel-tools -- additional tools for gptel.

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t; -*-

(require 'gptel)

;;; common

(defun gt--join-strings-into-csv (strings)
  "Join a list of STRINGS into a single CSV-formatted string.
Strings containing commas or double quotes are properly escaped."
  (mapconcat
   (lambda (s)
     (if (string-match-p "[,\"]" s)
         (concat "\"" (replace-regexp-in-string "\"" "\"\"" s) "\"")
       s))
   strings
   ","))

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

(provide 'gptel-tools)
;;; gptel-tools.el ends here
