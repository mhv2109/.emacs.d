;;; gptel-agent-session.el --- Save and load gptel-agent chat sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Marshall Van Loon
;; Keywords: convenience, tools

;;; Commentary:

;; Provides save/load functionality for gptel-agent chat sessions.
;;
;; Sessions are stored as .org files in a `.gptel/' directory at the root of
;; the project from which `gptel-agent' was dispatched.  gptel's built-in
;; Org state persistence (GPTEL_BOUNDS, GPTEL_MODEL, etc.) handles most of
;; the heavy lifting.  This module layers agent-specific state on top:
;;
;;   - Project directory (default-directory)
;;   - Agent preset (gptel-agent vs gptel-plan)
;;   - Todo list state (gptel-agent--todos)
;;   - Creation timestamp
;;
;; Usage:
;;   M-x gptel-agent-save-session  -- Save current agent session to .gptel/
;;   M-x gptel-agent-load-session  -- Load a saved agent session
;;   M-x gptel-agent              -- Now offers to resume saved sessions

;;; Code:

(require 'gptel)
(require 'gptel-agent)
(require 'gptel-org)

;;; Customization

(defcustom gptel-agent-session-dir-name ".gptel/"
  "Directory name for saved gptel-agent sessions, relative to project root."
  :type 'string
  :group 'gptel)

(defcustom gptel-agent-session-auto-save-on-kill t
  "If non-nil, prompt to save agent session when killing the buffer."
  :type 'boolean
  :group 'gptel)

;;; Helpers

(defun gptel-agent-session-dir (&optional project-dir)
  "Return the `.gptel/' directory path for the current or given PROJECT-DIR.
When PROJECT-DIR is nil, uses the project root of the current agent buffer."
  (expand-file-name gptel-agent-session-dir-name
                    (or project-dir
                        (gptel-agent--project-root)
                        default-directory)))

(defun gptel-agent-buffer-p ()
  "Return non-nil if the current buffer is a gptel-agent session."
  (string-prefix-p "*gptel-agent:" (buffer-name)))

(defun gptel-agent-saved-session-p ()
  "Return non-nil if this agent buffer is backed by a saved session file."
  (and (gptel-agent-buffer-p) buffer-file-name))

(defun gptel-agent--list-sessions (&optional project-dir)
  "Return alist of (name . filepath) for saved sessions in PROJECT-DIR.
Skips Emacs lock files (symlinks starting with `.#')."
  (let* ((dir (gptel-agent-session-dir project-dir))
         (files (and (file-directory-p dir)
                     (directory-files dir t "\\.org\\'"))))
    (cl-loop for f in files
             unless (file-symlink-p f)
             collect (cons (file-name-base f) f))))

(defun gptel-agent--session-first-message (filepath)
  "Extract the first user message from a saved session at FILEPATH.
Returns a short string suitable for display in a picker, or nil."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    ;; Skip property drawer
    (when (re-search-forward "^:END:" nil t)
      (forward-line 1)
      ;; Skip blank lines and org headings
      (skip-chars-forward "\n[:space:]")
      (when (looking-at "\\*+ ")
        (forward-line 1)
        (skip-chars-forward "\n[:space:]"))
      ;; Get first non-empty line as preview
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (unless (string-empty-p line)
          (when (> (length line) 80)
            (setq line (concat (substring line 0 77) "...")))
          line)))))

(defun gptel-agent--session-created (filepath)
  "Return the creation timestamp for session at FILEPATH, or the file mtime."
  (with-temp-buffer
    (insert-file-contents filepath nil 0 500)
    (goto-char (point-min))
    (if-let* ((ts (org-entry-get (point-min) "GPTEL_AGENT_CREATED")))
        ts
      (format-time-string "%Y-%m-%d %H:%M"
                          (file-attribute-modification-time
                           (file-attributes filepath))))))

(defun gptel-agent--generate-session-name ()
  "Generate a default session name based on project and timestamp."
  (let* ((dir (or (gptel-agent--project-root) default-directory))
         (project-name (cadr (nreverse (file-name-split dir))))
         (timestamp (format-time-string "%Y%m%d-%H%M")))
    (format "%s-%s" project-name timestamp)))

(defun gptel-agent--ensure-session-dir ()
  "Ensure the `.gptel/' directory exists, creating it if needed.
Returns the directory path, or nil on failure."
  (let ((dir (gptel-agent-session-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun gptel-agent--project-root ()
  "Return the project root for the current gptel-agent buffer.
Looks at the GPTEL_AGENT_PROJECT_DIR property for saved sessions,
otherwise falls back to `project-current'."
  (or (and buffer-file-name
           (derived-mode-p 'org-mode)
           (org-with-wide-buffer
            (goto-char (point-min))
            (let ((dir (org-entry-get (point-min) "GPTEL_AGENT_PROJECT_DIR")))
              (and dir (file-directory-p dir) dir))))
      (if-let ((proj (project-current nil default-directory)))
          (project-root proj)
        default-directory)))

;;; Save agent-specific Org properties

(defun gptel-agent-session--save-agent-props ()
  "Write agent-specific Org properties to the property drawer.
Added to `gptel-save-state-hook' so it runs alongside gptel's own state save."
  (when (and (gptel-agent-buffer-p) buffer-file-name)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((pt (point-min)))
       (org-entry-put pt "GPTEL_AGENT_PROJECT_DIR" default-directory)
       (when gptel--preset
         (org-entry-put pt "GPTEL_AGENT_PRESET" (symbol-name gptel--preset)))
       (when gptel-agent--todos
         (org-entry-put pt "GPTEL_AGENT_TODOS"
                        (prin1-to-string
                         (mapcar (lambda (todo)
                                   (list :content (plist-get todo :content)
                                         :activeForm (plist-get todo :activeForm)
                                         :status (plist-get todo :status)))
                                 (append gptel-agent--todos nil)))))
       ;; Set creation time on first save
       (unless (org-entry-get pt "GPTEL_AGENT_CREATED")
         (org-entry-put pt "GPTEL_AGENT_CREATED"
                        (format-time-string "%Y-%m-%d %H:%M")))))))

;;; Restore agent-specific state

(defun gptel-agent-session--fold-sections ()
  "Fold Tools and Reasoning blocks in the current buffer when loading a session.
Folds both #+begin_tool...#+end_tool and #+begin_reasoning...#+end_reasoning blocks."
  (org-with-wide-buffer
   (goto-char (point-min))
   ;; Fold #+begin_tool blocks
   (while (re-search-forward "^#\\+begin_tool" nil t)
     (org-hide-block-toggle t))
   ;; Fold #+begin_reasoning blocks
   (goto-char (point-min))
   (while (re-search-forward "^#\\+begin_reasoning" nil t)
     (org-hide-block-toggle t))))

(defun gptel-agent-session--restore-agent-state ()
  "Restore gptel-agent-specific state from Org properties after loading a session.
This runs after `gptel--restore-state' has already handled standard gptel state."
  (when (and (gptel-agent-buffer-p) buffer-file-name)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((pt (point-min)))
       ;; Restore project directory
       (let* ((saved-dir (org-entry-get pt "GPTEL_AGENT_PROJECT_DIR"))
              (derived-dir (when (and buffer-file-name (stringp buffer-file-name))
                             (let ((dir (file-name-directory buffer-file-name)))
                               (when dir (file-name-directory dir))))))
         (cond
          ((and saved-dir (file-directory-p saved-dir))
           (setq default-directory saved-dir))
          ((and derived-dir (file-directory-p derived-dir))
           (setq default-directory derived-dir))
          ((and saved-dir (not (file-directory-p saved-dir)))
           (message "Warning: Saved project dir %s is invalid, using %s instead" 
                    saved-dir default-directory))))
       ;; Restore agent preset (re-apply with buffer-local setter)
       (when-let* ((preset-str (org-entry-get pt "GPTEL_AGENT_PRESET"))
                   (preset (intern preset-str))
                   (spec (gptel-get-preset preset)))
         (gptel--apply-preset preset
                              (lambda (sym val) (set (make-local-variable sym) val)))
         (setq gptel--preset preset))
       ;; Restore todos
       (when-let* ((todos-str (org-entry-get pt "GPTEL_AGENT_TODOS")))
         (condition-case err
             (let ((todos (read todos-str)))
               (when (and todos (listp todos))
                 (gptel-agent--write-todo (vconcat todos))))
           (error
            (message "Could not restore agent todos: %s" err))))))))

(defun gptel-agent-session--setup-loaded-buffer ()
  "Set up agent-specific UI for a loaded session buffer.
Called after `find-file' on a saved session .org file."
  (with-current-buffer (current-buffer)
    ;; Restore agent state from properties
    (gptel-agent-session--restore-agent-state)
    ;; Set up agent-specific features that gptel-agent normally does
    (gptel-agent-update)
    ;; Ensure agent tools are available
    (unless gptel-max-tokens
      (setq-local gptel-max-tokens 8192))
    ;; Rename buffer to match agent convention
    (let* ((dir default-directory)
           (project-name (cadr (nreverse (file-name-split dir))))
           (agent-buffer-name (format "*gptel-agent:%s*" project-name)))
      (unless (get-buffer agent-buffer-name)
        (rename-buffer agent-buffer-name)))
    ;; Fold Tools and Reasoning sections
    (gptel-agent-session--fold-sections)
    ;; Set up the Agent/Plan header-line toggle
    (when gptel-use-header-line
      (let* ((agent-mode (equal (symbol-name gptel--preset) "gptel-agent"))
             (switch-mode
              (lambda (&rest _)
                (gptel--apply-preset
                 (if agent-mode 'gptel-plan 'gptel-agent)
                 (lambda (sym val) (set (make-local-variable sym) val)))
                (setq agent-mode (not agent-mode))
                (force-mode-line-update)))
             (display-mode
              (lambda () (concat
                     (propertize " " 'display '(space :align-to 0))
                     (format "%s" (gptel-backend-name gptel-backend))
                     (if agent-mode
                         (propertize (buttonize "[Agent]" switch-mode nil
                                                "Switch to planning preset")
                                     'face 'font-lock-keyword-face)
                       (propertize (buttonize "[Plan]" switch-mode nil
                                              "Switch to agent preset")
                                   'face 'font-lock-doc-face))))))
        (setcar header-line-format
                `(:eval (funcall ,display-mode)))))
    ;; Set up file/directory completion
    (gptel-agent--setup-completion)))

;;; Interactive Commands

;;;###autoload
(defun gptel-agent-save-session (&optional name)
  "Save the current gptel-agent session to a file in `.gptel/'.

The file is saved as an Org file with gptel and agent-specific properties
embedded in the property drawer.  On subsequent saves, the file is updated
in place.

When called interactively, prompts for a session name.  The default name
is derived from the project name and current timestamp."
  (interactive
   (list (read-string "Session name: " (gptel-agent--generate-session-name))))
  (unless (gptel-agent-buffer-p)
    (user-error "Not in a gptel-agent buffer"))
  (let* ((session-dir (gptel-agent--ensure-session-dir))
         (filepath (expand-file-name (concat name ".org") session-dir)))
    (unless session-dir
      (user-error "Could not create session directory"))
    ;; Associate buffer with file (or update if already saved)
    (unless (equal buffer-file-name filepath)
      (set-visited-file-name filepath t))
    ;; Save buffer -- this triggers gptel--save-state via before-save-hook,
    ;; which calls gptel-save-state-hook, which calls our agent props function
    (save-buffer)
    (message "Agent session saved to %s" filepath)))

;;;###autoload
(defun gptel-agent-load-session (filepath)
  "Load a saved gptel-agent session from FILEPATH.

Opens the .org file, restores gptel state, then layers agent-specific
state on top (project directory, agent preset, todos)."
  (interactive
   (let* ((sessions (gptel-agent--list-sessions))
          (choices (mapcar
                    (lambda (s)
                      (let* ((name (car s))
                             (path (cdr s))
                             (created (gptel-agent--session-created path))
                             (preview (gptel-agent--session-first-message path))
                             (display (concat name
                                              (when created (concat "  (" created ")"))
                                              (when preview (concat " -- " preview)))))
                        (cons display path)))
                    sessions)))
     (unless choices
       (user-error "No saved sessions in %s" (gptel-agent-session-dir)))
     (let* ((choice (completing-read "Load session: " choices nil t))
            (filepath (cdr (assoc choice choices))))
       (list filepath))))
  (find-file filepath)
  ;; gptel-mode should already be on from the file's local variables,
  ;; but ensure it is
  (unless gptel-mode
    (gptel-mode +1))
  (gptel-agent-session--setup-loaded-buffer))

;;;###autoload
(defun gptel-agent-session-delete (filepath)
  "Delete a saved gptel-agent session file at FILEPATH."
  (interactive
   (let* ((sessions (gptel-agent--list-sessions))
          (choices (mapcar
                    (lambda (s)
                      (let* ((name (car s))
                             (path (cdr s))
                             (created (gptel-agent--session-created path))
                             (display (concat name
                                              (when created (concat "  (" created ")")))))
                        (cons display path)))
                    sessions)))
     (unless choices
       (user-error "No saved sessions in %s" (gptel-agent-session-dir)))
     (let* ((choice (completing-read "Delete session: " choices nil t))
            (filepath (cdr (assoc choice choices))))
       (list filepath))))
  (when (y-or-n-p (format "Delete session %s? " (file-name-base filepath)))
    (delete-file filepath)
    (message "Session deleted: %s" filepath)))

;;; Modify gptel-agent to offer session resumption

(defvar gptel-agent--original-function (symbol-function 'gptel-agent)
  "Original `gptel-agent' function, before we advised it.")

;;;###autoload
(defun gptel-agent-session--around-gptel-agent (orig-fn &optional project-dir agent-preset)
  "Advice around `gptel-agent' to offer resuming saved sessions.

If `.gptel/' exists with saved sessions, presents a picker offering
to resume an existing session or start a new one.  Otherwise, proceeds
with the original behavior."
  (interactive
   (list (if current-prefix-arg
             (funcall project-prompter)
           (if-let ((proj (project-current)))
               (project-root proj)
             default-directory))
         'gptel-agent))
  (let* ((sessions (gptel-agent--list-sessions project-dir)))
    (if (not sessions)
        ;; No saved sessions -- proceed as normal
        (funcall orig-fn project-dir agent-preset)
      ;; Saved sessions exist -- offer picker
      (let* ((new-choice "* New session *")
             (choices (cons (cons new-choice nil)
                            (mapcar
                             (lambda (s)
                               (let* ((name (car s))
                                      (path (cdr s))
                                      (created (gptel-agent--session-created path))
                                      (preview (gptel-agent--session-first-message path))
                                      (display (concat name
                                                       (when created (concat "  (" created ")"))
                                                       (when preview (concat " -- " preview)))))
                                 (cons display path)))
                             sessions)))
             (choice (completing-read
                      (format "gptel-agent session (%d saved): " (length sessions))
                      choices nil t)))
        (if (equal choice new-choice)
            (funcall orig-fn project-dir agent-preset)
          ;; Load selected session
          (let ((filepath (cdr (assoc choice choices))))
            (find-file filepath)
            (unless gptel-mode (gptel-mode +1))
            (gptel-agent-session--setup-loaded-buffer)))))))

;;; Auto-save on buffer kill

(defun gptel-agent-session--maybe-save-on-kill ()
  "Prompt to save agent session when killing the buffer."
  (when (and (gptel-agent-buffer-p)
             (not (gptel-agent-saved-session-p))
             gptel-agent-session-auto-save-on-kill
             (> (buffer-size) 0))
    (when (y-or-n-p "Save gptel-agent session before closing? ")
      (gptel-agent-save-session))))

;;; Gitignore helper

(defun gptel-agent-session--maybe-add-gitignore (dir)
  "Prompt to add `.gptel/' to .gitignore in DIR if not already present."
  (let ((gitignore (expand-file-name ".gitignore" dir)))
    (when (and (file-directory-p (expand-file-name ".git" dir))
               (not (file-exists-p gitignore))
               (y-or-n-p
                (format "Add `%s' to .gitignore? " gptel-agent-session-dir-name)))
      (with-temp-buffer
        (insert gptel-agent-session-dir-name "\n")
        (write-region (point-min) (point-max) gitignore 'append)
        (message "Added %s to .gitignore" gptel-agent-session-dir-name)))))

;;; Kill-hook setup for gptel-mode-hook

(defun gptel-agent-session--maybe-add-kill-hook ()
  "Add `gptel-agent-session--maybe-save-on-kill' to `kill-buffer-hook' locally.
Intended for `gptel-mode-hook' so it only applies in gptel agent buffers."
  (when (gptel-agent-buffer-p)
    (add-hook 'kill-buffer-hook #'gptel-agent-session--maybe-save-on-kill nil t)))

;;; Activation

;;;###autoload
(defun gptel-agent-session-setup ()
  "Activate gptel-agent session save/load functionality.

Adds `gptel-agent-session--save-agent-props' to `gptel-save-state-hook',
advises `gptel-agent' to offer session resumption, and adds
`gptel-agent-session--maybe-add-kill-hook' to `gptel-mode-hook'."
  (add-hook 'gptel-save-state-hook #'gptel-agent-session--save-agent-props)
  (advice-add 'gptel-agent :around #'gptel-agent-session--around-gptel-agent)
  (add-hook 'gptel-mode-hook #'gptel-agent-session--maybe-add-kill-hook))

;;;###autoload
(defun gptel-agent-session-teardown ()
  "Deactivate gptel-agent session save/load functionality."
  (remove-hook 'gptel-save-state-hook #'gptel-agent-session--save-agent-props)
  (remove-hook 'gptel-mode-hook #'gptel-agent-session--maybe-add-kill-hook)
  (advice-remove 'gptel-agent #'gptel-agent-session--around-gptel-agent))

(provide 'gptel-agent-session)
;;; gptel-agent-session.el ends here
