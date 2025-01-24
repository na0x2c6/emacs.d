(defun my/set-grep-template-based-on-project ()
  "Set `grep-template' based on whether the current buffer is in a project."
  (cond
   ((project-current)
      (grep-apply-setting 'grep-command '("git --no-pager grep -n -e ")))
   (t
      (grep-apply-setting 'grep-command '("grep --color=auto -nH --null -e ")))))

(add-hook 'post-command-hook 'my/set-grep-template-based-on-project)
