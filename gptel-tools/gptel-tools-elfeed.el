;;; gptel-tools-elfeed -- efleed-related tools.

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t; -*-

(require 'gptel)
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
             (json-encode `(:headlines ,(mapcar #'elfeed-entry-title (gt--elfeed-get-entries)))))
 :description "Return news headlines from Elfeed RSS feed."
 :category "rss")

(gptel-make-tool
 :name "elfeed_get_entries"
 :function (lambda ()
             (json-encode `(:entries ,(vconcat (mapcar (lambda (entry)
                                                         (let ((title (elfeed-entry-title entry))
                                                               (link (elfeed-entry-link entry)))
                                                           (list :title title :link link)))
                                                       (gt--elfeed-get-entries))))))
 :description "Return news headlines and urls from Elfeed RSS feed."
 :category "rss")

(provide 'gptel-tools-elfeed)

;;; gptel-tools-elfeed.el ends here
