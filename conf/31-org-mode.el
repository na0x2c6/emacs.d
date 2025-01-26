(setq org-agenda-files
      '("~/org/agenda/"))

(setq org-todo-keywords
  '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))

(setq org-capture-templates
      '(("t" "TODO for me" entry (file+headline "~/org/agenda/me.org" "Inbox")
         "*** TODO %?\n    CAPTURED_AT: %a\n    %i")))

(defun my/set-archive-location (&rest _)
  "Set org-archive-location dynamically before archiving"
  (let* ((date (format-time-string "%Y/%Y-%m"))
         (archive-dir "~/org/archive")
         (archive-path (format "%s/%s.org" archive-dir date)))
    (make-directory (file-name-directory archive-path) t)
    (setq org-archive-location (format "%s::" archive-path))))

(advice-add 'org-archive-subtree :before #'my/set-archive-location)


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
