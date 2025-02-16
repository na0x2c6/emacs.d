(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

(defun my/paste-from-system-clipboard ()
  "Paste text from the system clipboard."
  (interactive)
  (insert (gui--selection-value-internal 'CLIPBOARD)))

(defun my/copy-to-system-clipboard ()
  "Copy selected region to the system clipboard."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
	(gui-set-selection 'CLIPBOARD text)
	(message "Copied to system clipboard"))
    (message "No region selected")))

(defun my/toggle-select-enable-clipboard ()
  "Toggle the value of `select-enable-clipboard`."
  (interactive)
  (setq select-enable-clipboard (not select-enable-clipboard))
  (message "select-enable-clipboard is now %s"
           (if select-enable-clipboard "enabled" "disabled")))

(defun my/git-link-copy-only ()
  "Copy git link to clipboard without opening browser."
  (interactive)
  (let ((git-link-add-to-kill-ring nil)
        (git-link-open-in-browser nil))
    (when-let ((link (call-interactively 'git-link nil)))
      (gui-set-selection 'CLIPBOARD link)
      link)))

(defun my/git-link-open-browser ()
  (interactive)
  (let ((git-link-add-to-kill-ring nil)
	(git-link-open-in-browser t))
    (call-interactively 'git-link nil)))

(defun my/copy-buffer-file-path-to-clipboard (&optional arg)
  "Copy the current buffer's file path to clipboard.
With prefix ARG:
  'h' - Copy path from home directory
  'p' - Copy path from git project root (default)
  'a' - Copy absolute path
If buffer is not visiting a file, copy buffer name instead."
  (interactive "P")
  (let* ((file-path (or (buffer-file-name) (buffer-name)))
         (path-type (if arg
                       (read-char-choice
                        "Copy path type [h]ome [p]roject [a]bsolute: "
                        '(?h ?p ?a))
                     ?p))
         (result-path
          (cond
           ;; Buffer is not visiting a file
           ((not (buffer-file-name))
            (buffer-name))
           
           ;; Home directory path
           ((eq path-type ?h)
            (abbreviate-file-name file-path))
           
           ;; Project relative path
           ((eq path-type ?p)
            (if (and (fboundp 'project-current)
                     (project-current))
                (file-relative-name file-path
                                  (project-root (project-current)))
              (abbreviate-file-name file-path)))
           
           ;; Absolute path
           ((eq path-type ?a)
            (expand-file-name file-path))
           
           ;; Default case
           (t file-path))))
    (gui-set-selection 'CLIPBOARD result-path)
    
    ;; Copy to clipboard
    ; (with-temp-buffer
    ;   (insert result-path)
    ;   (clipboard-kill-ring-save (point-min) (point-max)))
    
    (message "Copied to clipboard: %s" result-path)))
