(custom-set-variables
  '(tab-bar-select-tab-modifiers '(super)))

;; Assign C-h to delete the previous character
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; toggle line truncate
(define-key global-map (kbd "C-c L") 'toggle-truncate-lines)

(define-key evil-normal-state-map (kbd "s-s") 'save-buffer)
(define-key evil-normal-state-map (kbd "C-t") 'other-window)

(define-key minibuffer-local-map (kbd "C-l") 'delete-minibuffer-contents)

(global-set-key (kbd "s-t") 'tab-new)
(global-set-key (kbd "S-s-t") 'tab-undo)
(global-set-key (kbd "s-w") 'tab-close)

(global-set-key (kbd "s-v") 'my/paste-from-system-clipboard)
(global-set-key (kbd "s-c") 'my/copy-to-system-clipboard)
(global-set-key (kbd "C-c k") 'my/toggle-select-enable-clipboard)

;; org-mode

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x j") 'open-junk-file)

;; embark
(global-set-key (kbd "C-S-a") 'embark-act)

;; vetico
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-r") 'vertico-previous)
  (define-key vertico-map (kbd "C-s") 'vertico-next))

;; consult
(define-key evil-normal-state-map (kbd "C-p") 'consult-buffer)
(global-set-key (kbd "C-s") 'my/isearch)

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
(define-key evil-normal-state-map (kbd "C-k") 'eldoc-box-help-at-point)

(global-set-key (kbd "C-c C-p") 'my/copy-buffer-file-path-to-clipboard)

; org-super-links
(global-set-key (kbd "C-c s s") 'org-super-links-link)
(global-set-key (kbd "C-c s l") 'org-super-links-store-link)
(global-set-key (kbd "C-c s C-l") 'org-super-links-insert-link)

; zoom-frm
(define-key ctl-x-map [(control ?+)] 'zoom-in/out)
(define-key ctl-x-map [(control ?-)] 'zoom-in/out)
(define-key ctl-x-map [(control ?=)] 'zoom-in/out)
(define-key ctl-x-map [(control ?0)] 'zoom-in/out)


; multi-term
(global-set-key (kbd "C-~") 'multi-term-dedicated-toggle)
(global-set-key (kbd "C-`") 'my/multi-term-toggle)
