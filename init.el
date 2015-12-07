(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add homebrew-installed packages to load-path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(defvar my-packages
  '(ace-jump-mode cider coffee-mode exec-path-from-shell
                  ido-ubiquitous magit rainbow-delimiters rst
                  starter-kit starter-kit-bindings starter-kit-eshell
                  starter-kit-js starter-kit-lisp switch-window
                  whitespace)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Make the cursor a black bar
(setq default-cursor-type 'bar)
(set-cursor-color "black")

;; Add to ~/bin to path
(setenv "PATH" (concat (getenv "PATH") ":/Users/chad/bin"))
(setq exec-path (append exec-path '("/Users/chad/bin")))

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


(winner-mode 1)
(column-number-mode 1)
(normal-erase-is-backspace-mode 1)
(global-auto-revert-mode t)

;; use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun my-c-common-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-toggle-electric-state 1)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state 1)
  (subword-mode 1))
(add-hook 'c-mode-common-hook 'my-c-common-mode-hook)

;; XCode uses 2 spaces for indent, so I do the same in objc-mode
(defun my-obj-mode-hook ()
  (setq c-basic-offset 2))
(add-hook 'objc-mode-hook 'my-obj-mode-hook)

(setq inferior-lisp-program "lein repl")
(add-hook 'inferior-lisp-mode-hook 'paredit-mode)

;; rainbow-delimiters.el
(require 'rainbow-delimiters)
;; Enable rainbow-delimiters-mode in Emacs Lisp buffers
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; Enable rainbow-delimiters-mode in Clojure buffers.
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; Enable rainbow-delimiters-mode in other Lisp mode buffers.
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; cider
(require 'cider)

;; rst.el
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;; switch-window.el
(global-set-key (kbd "C-x o") 'switch-window)

;; ace-jump mode
(define-key global-map (kbd "<C-return>") 'ace-jump-mode)

;; Cider stuff
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq nrepl-hide-special-buffers t)
(setq cider-prompt-save-file-on-load nil)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; ido stuff
(setq ido-everywhere t)

;;;;;;; Manually-managed packages ;;;;;;
;; dockerfile-mode
(add-to-list 'load-path "/Users/chad/.emacs.d/vendor/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Explicitly initialize packages
(setq package-enable-at-startup nil)
(package-initialize)

;; Stuff that needs to happen after packages are initialized
(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "TRAVELPORT_ORIGIN_APPLICATION")
  (exec-path-from-shell-copy-env "TRAVELPORT_TARGET_BRANCH")
  (exec-path-from-shell-copy-env "TRAVELPORT_REMOTE_HOST")
  (exec-path-from-shell-copy-env "TRAVELPORT_USERNAME")
  (exec-path-from-shell-copy-env "TRAVELPORT_PASSWORD")
  (exec-path-from-shell-copy-env "DEVELOPER_AERO_API_KEY")
  (exec-path-from-shell-copy-env "MAPQUEST_API_KEY")
  (exec-path-from-shell-initialize))

;; Turn off magit warning message
(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
