;; Assign C-h to delete the previous character
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(defun paste-from-system-clipboard ()
  "Paste text from the system clipboard."
  (interactive)
  (insert (gui-get-selection 'CLIPBOARD)))
(global-set-key (kbd "s-v") 'paste-from-system-clipboard)

;; embark
(global-set-key (kbd "C-S-a") 'embark-act)

;; vetico
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-r") 'vertico-previous)
  (define-key vertico-map (kbd "C-s") 'vertico-next))

;; evil
(define-key evil-normal-state-map (kbd "C-p") 'consult-buffer)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-dispatch)
(global-set-key (kbd "C-c f") 'magit-file-dispatch)

;; git-gutter
(define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)

;; git-link
(global-set-key (kbd "C-c C-g l") 'git-link)

;; avy
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-:") 'avy-migemo-goto-char-timer)

;; Avy + Isearch
(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)
