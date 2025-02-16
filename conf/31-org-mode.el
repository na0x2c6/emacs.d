(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-agenda-files
      '("~/org/agenda/"))

(setq org-todo-keywords
  '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))

(setq org-capture-templates
      '(("t" "TODO for me" entry (file+headline "~/org/agenda/me.org" "Inbox")
         "*** TODO %?\n    CAPTURED_AT: %a\n    %i")
        ("i" "interrupted task" entry
         (file "~/org/agenda/gtd.org")
         "* %?\n" :clock-in t :clock-resume t)
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
