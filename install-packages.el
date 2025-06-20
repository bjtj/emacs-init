(defun install-packages-from-file (file-path)
  "Read package names from FILE-PATH and install them using `package-install`."
  (require 'package)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (with-temp-buffer
    (insert-file-contents file-path)
    (dolist (pkg (split-string (buffer-string) "\n" t))
      (let ((pkg-symbol (intern pkg)))
        (unless (package-installed-p pkg-symbol)
          (ignore-errors
            (package-install pkg-symbol)
            (message "Installed package: %s" pkg-symbol)))))))

;; Example usage:
(install-packages-from-file "packages.txt")
