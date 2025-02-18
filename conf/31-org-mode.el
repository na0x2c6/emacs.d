(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-agenda-files
      '("~/org/agenda/"))

(setq org-todo-keywords
  '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))

(setq org-capture-templates
      '(("t" "TODO to GTD inbox" entry (file+headline "~/org/agenda/gtd.org" "Inbox")
         "*** TODO %?"
         :jump-to-captured t)
        ("i" "interrupted task" entry
         (file+headline "~/org/agenda/gtd.org" "Inbox")
         "* %?\n" :clock-in t :clock-resume t :jump-to-captured t)
        ("j" "Journal entry" plain (function my/org-journal-find-location)
         "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
         :jump-to-captured t :immediate-finish t)))

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(defun my/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(defun my/set-archive-location (&rest _)
  "Set org-archive-location dynamically before archiving"
  (let* ((date (format-time-string "%Y/%Y-%m"))
         (archive-dir "~/org/archive")
         (archive-path (format "%s/%s.org" archive-dir date)))
    (make-directory (file-name-directory archive-path) t)
    (setq org-archive-location (format "%s::" archive-path))))

(advice-add 'org-archive-subtree :before #'my/set-archive-location)

(defcustom my/auto-git-commit-directories '("~/org")
  "List of directories to automatically commit and push changes if they are git repositories.
Each path should be a string representing the directory path."
  :type '(repeat string)
  :group 'my-auto-git)

(defun my/auto-git-commit-directory (dir)
  "Automatically commit and push changes in DIR if it's a git repository."
  (let* ((full-dir (expand-file-name dir))
         (default-directory full-dir))
    (when (vc-git-root full-dir)
      (let ((git-status (shell-command-to-string "git status --porcelain")))
        (when (not (string-empty-p git-status))
          (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
            (shell-command "git add -A")
            (shell-command (format "git commit -m 'Auto commit: %s'" timestamp))
            (shell-command "git push origin HEAD")))))))

(defun my/auto-git-commit-all ()
  "Run auto-commit for all directories in `my/auto-git-commit-directories'."
  (dolist (dir my/auto-git-commit-directories)
    (my/auto-git-commit-directory dir)))

(add-hook 'after-save-hook
          (lambda ()
            (when (buffer-file-name)
              (let ((file-path (buffer-file-name)))
                (dolist (dir my/auto-git-commit-directories)
                  (when (string-prefix-p
                         (expand-file-name dir)
                         file-path)
                    (my/auto-git-commit-directory dir)))))))

; (setq org-log-done 'time)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (awk . t)
   (C . t)
   (emacs-lisp . t)
   (js . t)
   (perl . t)
   (python . t)
   ))

(defun my/create-journal-entry-on-clock-in ()
  "Create a journal entry when clocking in, with a super-link to the clocked task."
  (when (org-clocking-p)
    (let* ((task-marker org-clock-marker)
           (task-heading (org-with-point-at task-marker
                          (org-get-heading t t t t)))
           (task-buffer (marker-buffer task-marker))
           (task-position (marker-position task-marker))
           (current-time (format-time-string "%H:%M"))
           (journal-file (org-journal--get-entry-path)))
      ;; Create journal entry if it doesn't exist
      (unless (file-exists-p journal-file)
        (org-journal-new-entry t))
      ;; Store link at the clocked task first
      (with-current-buffer task-buffer
        (goto-char task-position)
        (org-super-links-store-link))
      ;; Switch to journal file and create entry
      (let ((journal-buffer (find-file-noselect journal-file)))
        (with-current-buffer journal-buffer
          (widen)
          (goto-char (point-min))
          (unless (org-journal--daily-p)
            (org-journal--insert-header nil)
            (org-journal--insert-entry-header nil))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          ;; Add time-stamped entry with super-link
          (insert (format "** %s " current-time))
          (let ((link-pos (point)))
            (goto-char link-pos)
            (org-super-links-insert-link))
          ;; Save the journal file
          (save-buffer))
        ;; Switch to journal buffer and move cursor to the new entry
        (pop-to-buffer journal-buffer)
        (goto-char (point-max))
        (org-back-to-heading)))))

;; Add to org-clock-in-hook
(add-hook 'org-clock-in-hook #'my/create-journal-entry-on-clock-in)
