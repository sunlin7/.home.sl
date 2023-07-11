;;; init.el --- the initial file
;;; Commentary:
;;; Code:

(defvar sl-root-dir (file-name-directory load-file-name)
  "The root dir of the .emacs.sl .")
(defvar sl-site-lisp-dir (expand-file-name "site-lisp" sl-root-dir)
  "The site-lisp dir'.")
(defvar sl-site-start-dir (expand-file-name  "site-start.d" sl-root-dir)
  "This site-start.d dir.")
(defvar sl-packages-list '()
  "The list of the package used for currrent configuration.")
(defcustom sl-savefile-dir (expand-file-name "save.d" user-emacs-directory)
  "The directory to save user data."
  :group 'sl-emacs
  :type 'string)

(add-to-list 'load-path sl-site-lisp-dir)
(mapc (lambda (x)
        (require (intern (file-name-base x)) (file-name-sans-extension x)))
      (directory-files sl-site-start-dir t "^[0-9].*.el$"))
(provide 'sl-init)
;;; init.el ends here
