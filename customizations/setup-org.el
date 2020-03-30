(require 'org)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook #'whitespace-mode)

;; Function to insert current date
(require 'calendar)

(defun insdate-insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				  omit-day-of-week-p)))

(global-set-key "\C-x\M-d" `insdate-insert-current-date)
