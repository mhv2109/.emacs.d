(require 'org)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook #'whitespace-mode)

;; Function to insert current date
(require 'calendar)

(defun insdate-insert-current-date ()
  "Insert today's date using the current locale."
  (interactive)
  (insert (calendar-date-string (calendar-current-date) nil)))

(defun insdate-insert-current-datetime ()
  "Insert current date and time, including timezone."
  (interactive)
  (let ((current-time (nth 3 (split-string (current-time-string)))))
    (insert (concat
             (calendar-date-string (calendar-current-date) nil)
             " " current-time " " (nth 1 (current-time-zone))))))


(global-set-key "\C-x\M-d" `insdate-insert-current-date)
(global-set-key "\C-x\M-t" `insdate-insert-current-datetime)

