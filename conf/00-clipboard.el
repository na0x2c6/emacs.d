(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

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
