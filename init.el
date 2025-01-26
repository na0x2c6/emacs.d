(desktop-save-mode 1)
(tab-bar-mode 1)
(cua-mode)
(setq cua-enable-cua-keys nil)

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

;; for workaround
;; https://github.com/radian-software/straight.el/issues/1146#issuecomment-1949645571
(straight-use-package 'project)

(straight-use-package 'use-package)

;; set custom file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; create custom file if the file doesn't exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

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
  :config
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

(straight-use-package 'consult)

(use-package marginalia
  :straight t)

(use-package vertico
  :straight t)


(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'affe)
(straight-use-package 'wgrep)
(straight-use-package 'nord-theme)
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)

(use-package fussy
  :straight t
  :config
  (setq fussy-filter-fn 'fussy-filter-default)
  (setq fussy-use-cache t)
  (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)
  (fussy-setup)
  (fussy-eglot-setup))

(use-package flx-rs
  :straight
  (flx-rs
   :repo "jcs-elpa/flx-rs"
   :fetcher github
   :files (:defaults "bin"))
  :config
  (setq fussy-score-fn 'fussy-flx-rs-score)
  (flx-rs-load-dyn))

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
	       '(php-mode . ("intelephense" "--stdio" :initializationOptions
			     (:licenseKey "KEY"))))
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
  :straight t)

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
  :config)

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
  (setq multi-term-program "/usr/bin/zsh"))

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

(use-package avy
  :straight t)

(use-package open-junk-file
  :straight t
  :config
  (setq open-junk-file-format "~/org/memo/%Y-%m-%d_%H%M%S.org"))

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
(with-eval-after-load 'consult
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
	  :debounce 0.4 any)))

;; Start consult-line search with symbol at point
;; https://github.com/minad/consult/wiki#start-consult-line-search-with-symbol-at-point
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun consult-line-literal ()
  (interactive)
  (let ((completion-styles '(fussy basic)))
    (consult-line)))

(use-package recentf
  :config
  (setq recentf-max-saved-items 30)
  (recentf-mode 1))

(use-package git-link
  :straight t
  :config
  (setq git-link-use-commit t)
  (setq git-link-open-in-browser t))

;; max lines for completion
(setq vertico-count 20)

;; enable vertico-mode and marginalia-mode
(defun after-init-hook ()
  (vertico-mode)
  (marginalia-mode)
  ;; enable savehist-mode to persist Vertico order
  (savehist-mode))
(add-hook 'after-init-hook #'after-init-hook)

(use-package init-loader
  :straight t
  :config
  (init-loader-load "~/.emacs.d/conf/"))
