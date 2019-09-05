;;; init.el --- the initial file
;;; Commentary:
;;; Code:

(defvar sl-root-dir (file-name-directory load-file-name)
  "The root dir of the .emacs.sl .")
(defvar sl-site-lisp-dir (expand-file-name "site-lisp" sl-root-dir)
  "The site-lisp dir'.")
(defvar sl-site-start-dir (expand-file-name  "site-start.d" sl-root-dir)
  "This site-start.d dir.")
(defvar sl-modules (expand-file-name "modules.el" sl-root-dir)
  "The modules for personal, if this file not exit, all modules will load.")
(defvar sl-packages-list '()
  "The list of the package used for currrent configuration.")
(defvar sl-jump-from-user-interactive nil
  "Flag for marking the jumpping action from user interactive.")
(defcustom sl-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "The directory to save data."
  :group 'sl-emacs
  :type 'string)

(defcustom sl-complation-engine (if (and (boundp 'global-company-mode) global-company-mode)
                                    'company 'auto-complete)
  "Choose the complation engine, `auto-complete' or `company'."
  :group 'sl-emacs
  :type '(choice
          (const :tag "(None)" nil)
          (const :tag "auto-complete" 'auto-complete)
          (const :tag "company" 'company)))

;;;;; default plugins directories
(add-to-list 'load-path sl-site-lisp-dir)
;; Load the plugins. The 90custom-variables.el will set the custom variables.
(if (file-exists-p sl-modules)
    (load sl-modules)
  (message "No 'modules.el' file in %s, load all modules." sl-modules)
  (mapc (lambda (x) (load (file-name-sans-extension x))) (directory-files sl-site-start-dir t "^[0-9].*.el$")))

(provide 'sl-init)
;;; init.el ends here
