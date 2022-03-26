;; prefer encoding
;; https://www.emacswiki.org/emacs/UnicodeEncoding
(prefer-coding-system 'utf-8)

(setq make-backup-files nil)
(global-linum-mode 1)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq c-default-style "linux" c-basic-offset 2)
(setq column-number-mode t)
(setq compilation-scroll-output t)

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

;; install packages automatically on startup
(require 'cl-lib)
(defvar my-packages
  '(yasnippet yasnippet-snippets auto-complete web-mode exec-path-from-shell virtualenvwrapper flycheck))
(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
	   when (not (package-installed-p p)) do (cl-return nil)
	   finally (cl-return t)))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; http://stackoverflow.com/a/3312236/5676460
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" -o -name \"*.[ch]pp\" | etags -" dir-name)))

;; find . -type f -name "*.[ch]" -o -name "*.[ch]pp" | etags -

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

;; java-imports
(defun on-java-loaded ()
  (define-key java-mode-map (kbd "M-I") 'java-imports-add-import-dwim))
(setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
(add-hook 'java-mode-hook 'on-java-loaded)
(add-hook 'java-mode-hook 'java-imports-scan-file)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; timestamp
;; https://www.emacswiki.org/emacs/InsertingTodaysDate
(defun timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d_%H%M%S")))

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.lsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (virtualenvwrapper markdown-mode yaml-mode cmake-mode slime web-mode auto-complete yasnippet-snippets yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

(setq venv-location "~/env")


;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; https://unix.stackexchange.com/a/406519
;; (xterm-mouse-mode 1)

;; https://apple.stackexchange.com/a/399303
(if (and (display-graphic-p) (string= system-type "darwin"))
    (progn
      (setq default-directory "~/")
      (setq command-line-default-directory "~/")
      )
  )

;; http://ergoemacs.org/emacs/emacs_customize_default_window_size.html
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (width . 120)
              (height . 60)))
      (setq default-frame-alist
            '(
              (width . 120)
              (height . 60))))
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


;; windows
(when (eq system-type 'windows-nt)
  (setenv "PATH"
	  (concat
	   "C:/dev/xplatform/bin;"
	   (getenv "PATH")))
  (setq exec-path
	(append
	 '("C:/dev/xplatform/bin")
	 exec-path)))
