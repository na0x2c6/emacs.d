(desktop-save-mode 1)
(tab-bar-mode 1)
(cua-mode)
(global-auto-revert-mode 1)
(electric-pair-mode -1)
(setq cua-enable-cua-keys nil)
(setq require-final-newline nil)
(setq system-time-locale "C")
(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 5 1024 1024))
(setq org-src-preserve-indentation t)

;; for staignt
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
   (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

(straight-use-package 'org)

;; for workaround
;; https://github.com/radian-software/straight.el/issues/1146#issuecomment-1949645571
(straight-use-package 'project)

(straight-use-package 'use-package)

(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo-tree-history")))))

(setq evil-want-keybinding nil)

(use-package evil
  :straight t
  :init
  (setq evil-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  (setq evil-search-module 'evil-search)
  (setq evil-symbol-word-search t)
  :config
  ;; https://emacs.stackexchange.com/a/20717
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :config
  (evil-collection-init)
  (setq evil-collection-magit-want-horizontal-movement t))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

;; show column number in mode-line
(column-number-mode t)

;; show line number
(global-display-line-numbers-mode t)

;; show full path in title
(setq frame-title-format "%& %f")

;; change backup dirctory
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))


;; echo eldoc
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; flymake
(use-package flymake
  :straight t)

(use-package projectile
  :straight t
  :config
  (setq projectile-enable-caching t))

(use-package consult
  :straight t
  :config
  (defvar my/consult--source-project-file
    `(:name "Project Whole File"
            :narrow   (?p . "Project")
            :category file
            :face     consult-file
            :history  file-name-history
            :state    ,#'consult--file-state
            :new      ,#'consult--file-action
            :items
            ,(lambda ()
               (let ((current-project (project-current)))
                 (if current-project
                     (project-files current-project)
                   nil))))
    "Project file candidate source for `project-files'.")

  ;; by typing `?`, show prefix help in mini buffer
  (define-key consult-narrow-map
              (vconcat consult-narrow-key "?") #'consult-narrow-help)
  (add-to-list 'consult-buffer-sources 'my/consult--source-project-file t)
  (setq consult-project-buffer-sources
        (list
         `(:hidden nil :narrow ?b ,@consult--source-project-buffer)
         `(:hidden nil :narrow ?f ,@my/consult--source-project-file)))
  ; https://github.com/minad/consult?tab=readme-ov-file#live-previews
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key
   '("C-."
     :debounce 0.5 "<up>" "<down>"
     :debounce 0.4 any))
  ; Use migemo for grep
  (defun my/advice--consult--compile-regexp-migemo (orig input type ignore-case)
      "Use migemo to compile INPUT to a list of regular expressions."
      (funcall orig (migemo-get-pattern input) type ignore-case))
  (advice-add 'consult--compile-regexp :around #'my/advice--consult--compile-regexp-migemo))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

; ref: https://github.com/minad/vertico?tab=readme-ov-file#configuration
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Prompt indicator for `completing-read-multiple'.  Available out of the box
  ;; on Emacs 31, see `crm-prompt'.
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args)))))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'affe)
(straight-use-package 'wgrep)
(straight-use-package 'nord-theme)
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :config
  (keymap-unset corfu-map "RET"))

;; https://codeberg.org/akib/emacs-corfu-terminal
(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;; A few more useful configurations...
(use-package emacs
  :straight t
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  ;; :bind (("M-/" . dabbrev-completion)
  ;;        ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package cape
  :straight t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;; :bind ("C-c n" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

;; web-mode
(use-package web-mode
  :straight t
  :config
  (add-to-list 'eglot-server-programs
               '((web-mode) . ("typescript-language-server" "--stdio")))
  (setq web-mode-enable-auto-quoting nil)
  :mode (("\\.tsx\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.ctp\\'" . web-mode)
	 ("\\.jsp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode))
  :hook (web-mode . eglot-ensure))

;; php-mode
(use-package php-mode
  :straight t
  :config
  (add-to-list 'eglot-server-programs
	       '(php-mode . ("intelephense" "--stdio")))
  :hook (php-mode . eglot-ensure))

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; go-mode
(use-package go-mode
  :straight t
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))

; (use-package eglot
;   :straight t
;   :config
;   (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
;   (add-hook 'go-mode-hook #'eglot-ensure))

(use-package eglot-booster
  :straight (el-patch :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
    (when (executable-find "emacs-lsp-booster")
	(eglot-booster-mode)))

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; https://github.com/joaotavora/eglot/discussions/1436#discussioncomment-11034903
;; lsp
(use-package eglot
  :straight t
  :config
  ; https://joaotavora.github.io/eglot/#Performance-1
  (setq eglot-events-buffer-config '(:size 0 :format full)))

(use-package eldoc-box
  :straight t
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

;; quickrun
(use-package quickrun
  :straight t)

;; gtags
(use-package gtags
  :straight t
  :commands gtags-mode
  :config
  (add-hook 'c-mode-common-hook 'gtags-mode))

;; magit
(use-package magit
  :straight t
  :config
  (magit-auto-revert-mode))

;; git-gutter
(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables
   '(git-gutter:update-interval 0.5)))

;; terminal
(use-package multi-term
  :straight t
  :config
  (setq multi-term-dedicated-select-after-open-p t)
  (setq multi-term-dedicated-close-back-to-open-buffer-p t))

(use-package migemo
  :straight t
  :config
  (when (executable-find "cmigemo")
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (let ((dic-paths
	   '("/usr/share/cmigemo/utf-8/migemo-dict"
	     "/opt/homebrew/share/migemo/utf-8/migemo-dict")))
      (setq migemo-dictionary
	    (seq-find #'file-exists-p dic-paths)))
    (migemo-init)))

(use-package avy-migemo
  :straight t
  :config
  (when (commandp 'cmigemo)
    (avy-migemo-mode t)))

(use-package orderless
  :straight t
  :init
  (icomplete-mode)
  (defun my/orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  :config
  (setq orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-flex
                               my/orderless-migemo))

  (setq completion-styles '(orderless)))

(use-package avy
  :straight t)

(use-package open-junk-file
  :straight t
  :config
  (setq open-junk-file-format "~/org/memo/%Y-%m-%d_%H%M%S.org"))

(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FFFF00")
        ("FIXME"  . "#FF0000")
        ("NOTE"   . "#28ABE3"))))

(use-package dtrt-indent
  :straight t
  :config
  (dtrt-indent-global-mode))

(use-package popwin
  :straight t
  :config
  (popwin-mode 1))

(use-package recentf
  :config
  (setq recentf-max-saved-items 30)
  (recentf-mode 1))

(use-package zoom-frm
  :straight t)

(use-package git-link
  :straight t
  :config
  (setq git-link-use-commit t)
  (setq git-link-open-in-browser t))

(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop")
  :config
  (advice-add 'org-capture :before #'org-super-links-store-link))

(use-package org-journal
  :straight t
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%Y-%m-%d %a")
  (setq org-journal-file-header 'org-journal-file-header-func)
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d_journal.org"))

(use-package ox-gfm
  :straight t)

;; max lines for completion
(setq vertico-count 20)

(use-package init-loader
  :straight t
  :config
  (init-loader-load "~/.emacs.d/conf/"))

(use-package org-ai
  :straight t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4o-mini")
  (org-ai-install-yasnippets))

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
  (yas-global-mode 1))
