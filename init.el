(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell
                                  starter-kit-js starter-kit-bindings
                                  ace-jump-mode coffee-mode company
                                  rainbow-delimiters rst switch-window)
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
  (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)
  (subword-mode 1))
(add-hook 'c-mode-common-hook 'my-c-common-mode-hook)

(setq whitespace-style
      '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 80)

(setq inferior-lisp-program "lein repl")
(add-hook 'inferior-lisp-mode-hook 'paredit-mode)

;; rainbow-delimiters.el
(require 'rainbow-delimiters)
;; Enables rainbow-delimiters-mode in Emacs Lisp buffers
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; Enables rainbow-delimiters-mode in Clojure buffers.
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; enables rainbow-delimiters-mode in other Lisp mode buffers.
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; rst.el
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;; switch-window.el
(global-set-key (kbd "C-x o") 'switch-window)

;; ace-jump mode
(define-key global-map (kbd "<C-return>") 'ace-jump-mode)

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-etags)
(add-to-list 'company-etags-modes 'clojure-mode)

;; Eval buffer
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map
               "\C-c\C-k"
               '(lambda ()
                  (interactive)
                  (let ((current-point (point)))
                    (goto-char (point-min))
                    (let ((ns-idx (re-search-forward
                                   clojure-namespace-name-regex nil t)))
                      (when ns-idx
                        (goto-char ns-idx)
                        (let ((sym (symbol-at-point)))
                          (message (format "Loading %s ..." sym))
                          (lisp-eval-string
                           (format "(require '%s :reload)" sym))
                          (lisp-eval-string (format "(in-ns '%s)" sym)))))
                    (goto-char current-point))))
             (define-key clojure-mode-map
               "\M-."
               '(lambda (next-p)
                  (interactive "P")
                  (find-tag (first (last (split-string
                                          (symbol-name (symbol-at-point)) "/")))
                            next-p)))))
