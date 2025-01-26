(setq org-agenda-files
      '("~/org/agenda/"))

(setq org-todo-keywords
  '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))

(setq org-capture-templates
      '(("t" "TODO for me" entry (file+headline "~/org/agenda/me.org" "Inbox")
         "*** TODO %?\n    CAPTURED_AT: %a\n    %i")))

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
