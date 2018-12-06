(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; Only use melpa-stable for cider
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add homebrew-installed packages to load-path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(defvar my-packages
  '(aggressive-indent better-defaults cider clojure-mode dockerfile-mode
                      exec-path-from-shell idle-highlight-mode
                      magit markdown-mode paredit php-mode
                      rainbow-delimiters smex switch-window whitespace
                      yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
(setq default-cursor-type 'bar)
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
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

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
  (setq c-basic-offset 4)
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
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq nrepl-hide-special-buffers t)
(setq nrepl-log-messages nil)
(setq cider-prompt-save-file-on-load nil)
(setq nrepl-prompt-to-kill-server-buffer-on-quit nil)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

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

;; Rust stuff
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; Use visual-line-mode in gfm-mode
(defun my-gfm-mode-hook ()
  (visual-line-mode 1))
(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)

;;;; Manually-managed packages ;;;;;;

;;;; Explicitly initialize packages
(setq package-enable-at-startup nil)
(package-initialize)

;; ;; Stuff that needs to happen after packages are initialized

;; This stuff is very slow for some reason...
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-copy-env "AWS_ACCESS_KEY")
;;   (exec-path-from-shell-copy-env "AWS_SECRET_KEY")
;;   (exec-path-from-shell-copy-env "AWS_DDB_ENDPOINT")
;;   (exec-path-from-shell-copy-env "F1_APP_ID")
;;   (exec-path-from-shell-copy-env "F1_APP_SECRET")
;;   (exec-path-from-shell-copy-env "F1_SELLER_PROXY_PORT")
;;   (exec-path-from-shell-copy-env "F1_SERVER_PORT")
;;   (exec-path-from-shell-copy-env "GEOCODER_CA_API_KEY")
;;   (exec-path-from-shell-copy-env "GET_GW_URLS_URL")
;;   (exec-path-from-shell-copy-env "GOOGLE_DIRECTIONS_API_KEY")
;;   (exec-path-from-shell-copy-env "LEIN_USERNAME")
;;   (exec-path-from-shell-copy-env "LEIN_PASSPHRASE")
;;   (exec-path-from-shell-copy-env "PAYMENT_SVC_ENABLE_MOCK_MODE")
;;   (exec-path-from-shell-copy-env "PAYMENT_SVC_LOG_LEVEL")
;;   (exec-path-from-shell-copy-env "PAYMENT_SVC_PASSWORD")
;;   (exec-path-from-shell-copy-env "PAYMENT_SVC_USERNAME")
;;   (exec-path-from-shell-copy-env "PLANNER_SVC_ENABLE_MOCK_MODE")
;;   (exec-path-from-shell-copy-env "PLANNER_SVC_LOG_LEVEL")
;;   (exec-path-from-shell-copy-env "PLANNER_SVC_PASSWORD")
;;   (exec-path-from-shell-copy-env "PLANNER_SVC_USERNAME")
;;   (exec-path-from-shell-copy-env "STORAGE_SVC_ENABLE_MOCK_MODE")
;;   (exec-path-from-shell-copy-env "STORAGE_SVC_LOG_LEVEL")
;;   (exec-path-from-shell-copy-env "STORAGE_SVC_PASSWORD")
;;   (exec-path-from-shell-copy-env "STORAGE_SVC_USERNAME")
;;   (exec-path-from-shell-copy-env "SVC_GW_PORT")
;;   (exec-path-from-shell-copy-env "SVC_GW_LOG_LEVEL")
;;   (exec-path-from-shell-copy-env "TEST_CLIENT_LOG_LEVEL")
;;   (exec-path-from-shell-copy-env "TEST_CLIENT_PASSWORD")
;;   (exec-path-from-shell-copy-env "TEST_CLIENT_USERNAME")
;;   (exec-path-from-shell-copy-env "TEST_GW_URLS")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_AGENCY_PROFILE_ID")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_AIR_TICKETING_TTL_MINS")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_ALLOW_BOOKING")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_AUTHORIZED_BY")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_ENABLE_SOAP_LOGGING")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_HCP_PASSWORD")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_HCP_URL")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_HCP_USERNAME")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_LOG_LEVEL")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_NUM_DAS")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_ORIGIN_APPLICATION")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_PASSWORD")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_PCC_TZ_ID")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_PROVIDER")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_REMOTE_HOST")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_TARGET_BRANCH")
;;   (exec-path-from-shell-copy-env "TRAVELPORT_USERNAME")
;;   (exec-path-from-shell-initialize))

;; Turn off magit warning message
(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-lein-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(cider-cljs-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(cider-inject-dependencies-at-jack-in nil)
 '(package-selected-packages
   (quote
    (paredit yaml-mode switch-window smex rainbow-delimiters php-mode markdown-mode magit exec-path-from-shell dockerfile-mode cider better-defaults aggressive-indent ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
