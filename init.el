(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'load-path "~/.emacs.d/my-lisp/")

;; Add homebrew-installed packages to load-path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(straight-use-package 'aggressive-indent)
(straight-use-package 'better-defaults)
(straight-use-package 'cider)
(straight-use-package 'clojure-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'paredit)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'smex)
(straight-use-package 'switch-window)
(straight-use-package 'use-package)
(straight-use-package 'whitespace)
(straight-use-package 'yaml-mode)

(load "idle-highlight-mode") ; Manually installed in my-lisp to control version

(require 'better-defaults)

(require 'ido)
(ido-mode t)

(setq inhibit-startup-message t) ;; No splash screen
(setq initial-scratch-message nil) ;; No scratch message
;; Create backup files in .emacs-backup instead of everywhere
(defvar user-temporary-file-directory "~/.emacs-backup")
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Make the cursor a black bar
(setq-default cursor-type 'bar)
(set-cursor-color "black")

(setenv "PATH" (concat (getenv "PATH") ":/Users/chad/bin:/usr/local/bin"))
(setq exec-path (append exec-path '("/Users/chad/bin"
                                    "/usr/local/bin")))

;; Mac-specific stuff
(setq browse-url-browser-function 'browse-url-default-macosx-browser
      delete-by-moving-to-trash t)
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Set meta to be the command key
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Whitespace stuff
(setq whitespace-style
      '(empty face indentation::space lines-tail space-before-tab::space
              space-after-tab::space tabs trailing))
(setq whitespace-line-column 80)
(setq whitespace-action
      '(auto-cleanup cleanup warn-read-only))
(global-whitespace-mode 1)
(setq whitespace-global-modes '(not markdown-mode gfm-mode))

(winner-mode 1)
(column-number-mode 1)
(normal-erase-is-backspace-mode 1)
(global-auto-revert-mode t)

;; highlight words
(use-package idle-highlight-mode
  :config (setq idle-highlight-exceptions '())
  :hook ((prog-mode text-mode) . idle-highlight-mode))
;;(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; Indentation
(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

;; use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun my-c-common-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (c-toggle-electric-state 1)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state 1)
  (subword-mode 1)
  (paredit-mode 1)
  (esk-paredit-nonlisp))
(add-hook 'c-mode-common-hook 'my-c-common-mode-hook)

;; XCode uses 2 spaces for indent, so I do the same in objc-mode
(defun my-obj-c-mode-hook ()
  (setq c-basic-offset 2))
(add-hook 'objc-mode-hook 'my-obj-c-mode-hook)

;; Clojure stuff
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)

;; Cider stuff
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq nrepl-hide-special-buffers t)
(setq nrepl-log-messages nil)
(setq cider-prompt-save-file-on-load nil)
(setq nrepl-prompt-to-kill-server-buffer-on-quit nil)
(setq cider-inject-dependencies-at-jack-in nil)
;;(setq cider-clojure-cli-aliases "-M:clj:dev:test:ci")
(setq nrepl-use-ssh-fallback-for-remote-hosts t)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; rainbow-delimiters.el
(require 'rainbow-delimiters)
;; Enable rainbow-delimiters-mode in Emacs Lisp buffers
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; Enable rainbow-delimiters-mode in Clojure buffers.
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; Enable rainbow-delimiters-mode in other Lisp mode buffers.
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; switch-window.el
(global-set-key (kbd "C-x o") 'switch-window)

;; ido stuff
(ido-mode 1)
(setq ido-everywhere t)
(require 'icomplete)
(icomplete-mode 1)
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
