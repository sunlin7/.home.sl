;;; startup --- the settings for packages.
;;; Commentary:
;;; Code:

(defvar sl-packages-list
  '(htmlize ggtags)
  "The dpended packages.")

(custom-set-variables
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                      ("melpa" . "http://melpa.org/packages/"))))

(require 'package)
(defun sl-package-install-all ()
  "Install all the packages list in `sl-packages-list'."
  (interactive)
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (dolist (pkg sl-packages-list)
    (when (and (not (package-installed-p pkg))
               (assoc pkg package-archive-contents))
      (package-install pkg))))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but show only the packages that \
are installed and are not in `sl-packages-list'.  Useful for
cleaning out unwanted packages."
  (interactive)
  (require 'cl)
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (memq x sl-packages-list))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))
(provide '00startup.el)
;;; 00startup ends here
