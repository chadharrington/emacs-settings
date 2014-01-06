;; Make the cursor a black bar
(setq default-cursor-type 'bar)
(set-cursor-color "black")

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


;; cider.el
;; Enable eldoc in Clojure buffers:
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; Hide the *nrepl-connection* and *nrepl-server* buffers 
(setq nrepl-hide-special-buffers t)
;; Prevent the auto-display of the REPL buffer in a separate window
;; after connection is established:
(setq cider-repl-pop-to-buffer-on-connect nil)
;; Stop the error buffer from popping up while working in buffers
;; other than the REPL:
(setq cider-popup-stacktraces nil)
;; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)
;; Auto-select the error buffer when it's displayed:
(setq cider-auto-select-error-buffer t)
;; Change the separator from space to hyphen
(setq nrepl-buffer-name-separator "-")
;; Show nREPL port
(setq nrepl-buffer-name-show-port t)
;; Make C-c C-z switch to the CIDER REPL buffer in the current window:
(setq cider-repl-display-in-current-window t)
;;Limit the number of items of each collection the printer will print to 100----------
(setq cider-repl-print-length 100) ; the default is nil, no limit


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

;; Workgroups setup
(require 'workgroups2)
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)        ; put this one at the bottom of .emacs
