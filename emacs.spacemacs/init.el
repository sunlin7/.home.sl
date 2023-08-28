;;; init.el --- the initial file
;;; Commentary:
;;; Code:

(defvar sl-root-dir (file-name-directory load-file-name)
  "The root dir of the .emacs.sl .")

(defvar sl-packages-list '()
  "The list of the package used for currrent configuration.")

(defcustom sl-savefile-dir (expand-file-name "save.d" user-emacs-directory)
  "The directory to save user data."
  :group 'sl-emacs
  :type 'string)

(add-to-list 'load-path (expand-file-name "site-lisp" sl-root-dir))

(mapc 'load (seq-uniq
             (mapcar
              (lambda (x) (substring x 0 (string-match "\\.elc?\\(\\.gz\\)?$" x)))
              (directory-files (expand-file-name  "site-start.d" sl-root-dir)
                               t "^[0-9].+\\.elc?\\(\\.gz\\)?$"))))

(provide 'sl-init)
;;; init.el ends here
