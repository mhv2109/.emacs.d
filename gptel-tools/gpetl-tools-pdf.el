;;; gptel-tools-pdf -- pdf-related tools. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)

(if-let ((found (executable-find "pdftotext")))
    (progn
      (defun gt--pdf-to-string (path)
        "Return `pdftotext` output of PDF file at PATH."
        (with-temp-buffer
          (call-process found nil t nil (expand-file-name path) "-")
          (buffer-string)))
      (gptel-make-tool
       :name "pdf_to_text"
       :function (lambda (path)
                   (json-encode (list :path path
                                      :contents (gt--pdf-to-string path))))
       :description "Get the contents of a PDF file as a string. Returns filepath and contents in JSON format."
       :args (list '(:name "path" :type string :description "path to PDF file"))
       :category "files"))
  (message "`pdftotext' not found on $PATH, not initializing PDF tools. Please install `pdftotext'."))

(provide 'gptel-tools-pdf)

;;; gptel-tools-pdf.el ends here
