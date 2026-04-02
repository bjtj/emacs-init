;;; .emacs --- init file
;;; Commentary:
;;; Code:

;; prefer encoding
;; https://www.emacswiki.org/emacs/UnicodeEncoding
(prefer-coding-system 'utf-8)

(setq make-backup-files nil)
(global-display-line-numbers-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-default-style "linux" c-basic-offset 2)
(setq column-number-mode t)
(setq compilation-scroll-output t)
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq-local indent-line-function 'js-jsx-indent-line)
(setq default-input-method "korean-hangul")
(setq ring-bell-function 'ignore)

(setq lock-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; yse/no -> y/n
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
(setq dired-listing-switches "-alh")

;; https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (version< emacs-version "29.0")
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))

;; exec-path-from-shell
;; - https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (and (daemonp) (not (memq system-type '(windows-nt ms-dos))))
    (exec-path-from-shell-initialize)))

;; ace window
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; gitignore
(use-package gitignore-templates
  :ensure t)

;; git-modes
(use-package git-modes
  :ensure t)

;; markdown
(use-package markdown-mode
  :ensure t
  :config
  (progn
    (setq markdown-fontify-code-blocks-natively t)
    (setq markdown-code-face '((t nil)))))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)
    (add-hook 'after-init-hook #'global-flycheck-mode)))

;; company-mode
(use-package company
  :ensure t
  :config
  (global-company-mode))

;; auto-complete
(use-package auto-complete
  :ensure t
  :config
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")))

;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(setq projectile-per-project-compilation-buffer t)

;; http://stackoverflow.com/a/3312236/5676460
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun create-tags (dir-name)
  "Create tags file (DIR-NAME is target path)."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" -o -name \"*.[ch]pp\" | etags -" dir-name)))

;; find . -type f -name "*.[ch]" -o -name "*.[ch]pp" | etags -

;; complation mode - ansi color
(use-package ansi-color
  :commands ansi-color-compilation-filter
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

;; http://www.emacswiki.org/emacs/PuTTY
;; PuTTY fix. Ugly. Bad. But it works. (Good)
(define-key global-map "\M-[1~" 'beginning-of-line)
(define-key global-map [select] 'end-of-line)

;; http://stackoverflow.com/questions/3592288/emacs-keybinding-to-compile-c-file
;;(add-hook 'c-mode-common-hook
;;    (lambda () (define-key c-mode-base-map (kbd "<f5>") 'compile)))

(define-key global-map (kbd "<f5>") 'compile)
(define-key global-map (kbd "<f6>") 'isearch-forward)
(define-key global-map (kbd "<f7>") 'find-file)
(define-key global-map (kbd "<f8>") 'save-buffer)
(define-key global-map (kbd "<f9>") 'kill-buffer)
(define-key global-map (kbd "M-*") 'pop-tag-mark) ;; https://github.com/nitingupta910/.emacs.d/blob/master/init.el

(define-key global-map (kbd "C--") 'undo)

;; ReformatBuffer
;; https://www.emacswiki.org/emacs/ReformatBuffer
(defun indent-buffer ()
  "Indent Buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; backward window move
;; http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window

;; C-u -1 C-x o
(define-key global-map (kbd "ESC <up>") (lambda () (interactive) (other-window -1)))
(define-key global-map (kbd "ESC <down>") 'other-window)
(define-key global-map (kbd "<M-up>") (lambda () (interactive) (other-window -1)))
(define-key global-map (kbd "<M-down>") 'other-window)

;; http://stackoverflow.com/a/17022997/5676460
(setq scroll-error-top-bottom t)

;; https://www.emacswiki.org/emacs/Scrolling
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   6)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 6)) )

;; http://stackoverflow.com/a/19589885/5676460
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; timestamp
;; https://www.emacswiki.org/emacs/InsertingTodaysDate
(defun timestamp ()
  "Get formatted timestamp."
  (interactive)
  (insert (format-time-string "%Y%m%d_%H%M%S")))

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.lsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))

;; https://unix.stackexchange.com/a/406519
;; (xterm-mouse-mode 1)

;; https://apple.stackexchange.com/a/399303
(if (and (display-graphic-p) (string= system-type "darwin"))
    (progn
      (setq default-directory "~/")
      (setq command-line-default-directory "~/")))

;; http://ergoemacs.org/emacs/emacs_customize_default_window_size.html
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (width . 120)
              (height . 45)))
      (setq default-frame-alist
            '(
              (width . 120)
              (height . 45))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))

;; clojure
(use-package cider
  :ensure t)
(use-package paredit
  :ensure t)

(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

(defun cider-connect-dot-nrepl-port ()
  "Connect to an nREPL server using the port from .nrepl-port."
  (interactive)
  (let ((port (when (file-exists-p ".nrepl-port")
                (string-trim (with-temp-buffer
                               (insert-file-contents ".nrepl-port")
                               (buffer-string))))))
    (if port
        (cider-connect `(:host "localhost" :port ,port))
      (message "No .nrepl-port file found!"))))

(defun cider-connect-shadow-port-file ()
  "Connect to an nREPL server(CLJS) using the port in .shadow-cljs/nrepl.port."
  (interactive)
  (let* ((nrepl-port-path ".shadow-cljs/nrepl.port")
         (port (when (file-exists-p nrepl-port-path)
                 (string-trim (with-temp-buffer
                                (insert-file-contents nrepl-port-path)
                                (buffer-string))))))
    (if port
        (cider-connect-cljs `(:host "localhost" :port ,port :cljs-repl-type shadow))
      (message "No .nrepl-port file found!"))))

;; -------
;; WINDOWS
;; =======

(when (eq system-type 'windows-nt)
  (setenv "PATH"
	        (concat
	         "C:/dev/xplatform/bin;"
	         (getenv "PATH")))
  (setq exec-path
	      (append
	       '("C:/dev/xplatform/bin")
	       exec-path))
  (setq find-program "C:/dev/xplatform/bin/find.exe")
  (setq grep-program "C:/dev/xplatform/bin/grep.exe"))

;; unicode font
(use-package unicode-fonts
  :if window-system
  :ensure t
  :config
  (unicode-fonts-setup))


;; React + Typescript
(use-package typescript-ts-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((typescript-ts-mode . lsp))
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package prettier
  :ensure t)

(use-package prettier-js
  :ensure t
  :hook ((typescript-ts-mode . prettier-js-mode)))

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode)))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; web-mode extra config
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook #'turn-on-smartparens-mode t)

(provide '.emacs)
;;; .emacs ends here
