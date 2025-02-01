;; Assign C-h to delete the previous character
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; toggle line truncate
(define-key global-map (kbd "C-c L") 'toggle-truncate-lines)

(define-key evil-normal-state-map (kbd "s-s") 'save-buffer)
(define-key evil-normal-state-map (kbd "C-t") 'other-window)

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
(define-key evil-normal-state-map (kbd "C-k") 'eldoc-box-help-at-point)

(defun my/copy-buffer-file-path-to-clipboard (&optional arg)
  "Copy the current buffer's file path to clipboard.
With prefix ARG:
  'h' - Copy path from home directory
  'p' - Copy path from git project root (default)
  'a' - Copy absolute path
If buffer is not visiting a file, copy buffer name instead."
  (interactive "P")
  (let* ((file-path (or (buffer-file-name) (buffer-name)))
         (path-type (if arg
                       (read-char-choice
                        "Copy path type [h]ome [p]roject [a]bsolute: "
                        '(?h ?p ?a))
                     ?p))
         (result-path
          (cond
           ;; Buffer is not visiting a file
           ((not (buffer-file-name))
            (buffer-name))
           
           ;; Home directory path
           ((eq path-type ?h)
            (abbreviate-file-name file-path))
           
           ;; Project relative path
           ((eq path-type ?p)
            (if (and (fboundp 'project-current)
                     (project-current))
                (file-relative-name file-path
                                  (project-root (project-current)))
              (abbreviate-file-name file-path)))
           
           ;; Absolute path
           ((eq path-type ?a)
            (expand-file-name file-path))
           
           ;; Default case
           (t file-path))))
    
    ;; Copy to clipboard
    (with-temp-buffer
      (insert result-path)
      (clipboard-kill-ring-save (point-min) (point-max)))
    
    (message "Copied to clipboard: %s" result-path)))

(global-set-key (kbd "C-c C-p") 'my/copy-buffer-file-path-to-clipboard)
