(defun my/multi-term-toggle ()
  "Toggle terminal buffer with the following behavior:
- If current buffer is a terminal, close it (and close tab if it's the only buffer)
- If current buffer is not a terminal, create new tab and open terminal"
  (interactive)
  (if (term-check-proc (current-buffer))
      (let ((buffers-in-tab (mapcar #'window-buffer (window-list))))
        (if (cl-every #'(lambda (buf)
                         (term-check-proc buf))
                     buffers-in-tab)
            ;; if all buffers in the tab are terminal, close the tab
            (tab-close)
          (kill-buffer (current-buffer))))
    (tab-new)
    (multi-term-next)))
