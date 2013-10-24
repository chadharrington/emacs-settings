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

;; use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(column-number-mode 1)
(normal-erase-is-backspace-mode 1)
(global-auto-revert-mode t)

(defun my-c-common-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (c-toggle-electric-state 1)
  (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)
  (c-subword-mode 1))
(add-hook 'c-mode-common-hook 'my-c-common-mode-hook)

(setq whitespace-style
      '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 80)


;; rst.el
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;; Configure nrepl.el
; Enable eldoc in clojure buffers
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
; Stop the error buffer from popping up while working in the REPL buffer
(setq nrepl-popup-stacktraces nil)
; Make C-c C-z switch to the *nrepl* buffer in the current window:
(add-to-list 'same-window-buffer-names "*nrepl*") 






