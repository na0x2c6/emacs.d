;; Assign C-h to delete the previous character
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(defun my/paste-from-system-clipboard ()
  "Paste text from the system clipboard."
  (interactive)
  (insert (gui-get-selection 'CLIPBOARD)))

(defun my/copy-to-system-clipboard ()
  "Copy selected region to the system clipboard."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
	(x-set-selection 'CLIPBOARD text)
	(message "Copied to system clipboard"))
    (message "No region selected")))

(defun my/toggle-select-enable-clipboard ()
  "Toggle the value of `select-enable-clipboard`."
  (interactive)
  (setq select-enable-clipboard (not select-enable-clipboard))
  (message "select-enable-clipboard is now %s"
           (if select-enable-clipboard "enabled" "disabled")))

(global-set-key (kbd "s-v") 'my/paste-from-system-clipboard)
(global-set-key (kbd "s-c") 'my/copy-to-system-clipboard)
(global-set-key (kbd "C-c k") 'my/toggle-select-enable-clipboard)

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
(global-set-key (kbd "C-c l") 'my/git-link-copy-only)
(global-set-key (kbd "C-c o") 'my/git-link-open-browser)

;; avy
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-:") 'avy-migemo-goto-char-timer)

;; Avy + Isearch
(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

;; eldoc-box
(define-key evil-normal-state-map (kbd "K") 'eldoc-box-help-at-point)
