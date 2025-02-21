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

(use-package auto-complete
  :ensure t
  :config
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")))

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

;; for clojure on windows
;; * <https://github.com/clojure-emacs/cider/issues/2963#issuecomment-828125977>
(with-eval-after-load 'cider
  (when (eq system-type 'windows-nt)
    (define-advice cider--list-as-lein-artifact (:override (list &optional exclusions))
      "Add missing double quotes around the version string for cmd.exe."
      (shell-quote-argument
       (format "[%s \"%S\"%s]" (car list) (cadr list) (cider--lein-artifact-exclusions exclusions))))))


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

;; ------------------
;; REACT + TYPESCRIPT
;; ==================
;; https://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh

;; ...
;; tide def func:
(defun tide-setup-hook ()
  (tide-setup)
  (eldoc-mode)
  (tide-hl-identifier-mode +1)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq lsp-eslint-server-command '("node" (concat (getenv "HOME") "/var/src/vscode-eslint/server/out/eslintServer.js") "--stdio"))
  (set (make-local-variable 'company-backends)
       '((company-tide company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev))))

;; hooks
(add-hook 'before-save-hook 'tide-format-before-save)


;; use rjsx-mode for .js* files except json and use tide with rjsx
(add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'rjsx-mode-hook 'tide-setup-hook)


;; web-mode extra config
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook #'turn-on-smartparens-mode t)

(provide '.emacs)
;;; .emacs ends here
