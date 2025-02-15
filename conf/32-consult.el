;; Start consult-line search with symbol at point
;; https://github.com/minad/consult/wiki#start-consult-line-search-with-symbol-at-point
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun consult-line-literal ()
  (interactive)
  (let ((completion-styles '(fussy basic)))
    (consult-line)))

(defun my/isearch-or-consult (use-consult)
  (interactive "p")
  (cond ((eq use-consult 1)
         (call-interactively 'isearch-forward))
        ((eq use-consult 4)
         (call-interactively 'consult-line-symbol-at-point))
         (call-interactively 'consult-line-migemo)))
