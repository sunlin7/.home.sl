;;; init.el --- the initial file -*- lexical-binding: t; coding: utf-8 -*-
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

;; prefer chinese-gbk
;; (set-locale-environment "zh_CN.UTF-8")

(custom-set-variables
 '(delete-selection-mode t)
 '(diary-file (expand-file-name "diary" sl-savefile-dir)) ;; the diary-file name
 )

(with-eval-after-load 'dired
  (declare-function dired-omit-mode "dired")
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(with-eval-after-load 'dired-x
  (defvar dired-omit-files)
  (setq dired-omit-files (concat dired-omit-files "\\|^#.*\\|^\\..*")))

(defun sl-toggle-color ()
  "Toggle the background/foreground color."
  (interactive)
  (let* ((fgcolor (face-attribute 'default :foreground))
         (bgcolor (face-attribute 'default :background)))
    (set-foreground-color bgcolor)
    (set-background-color fgcolor)))

(defmacro sl-url-hex-macro (fsym func)
  "Define function with name `FSYM' and call the function `FUNC' on region."
  `(defun ,fsym (start end)
     "Run the url-hexify-string/url-unhex-string interactively."
     (interactive "r")
     (save-excursion
       (insert (funcall ,func (delete-and-extract-region start end))))))
(sl-url-hex-macro sl-url-hex #'url-hexify-string)
(sl-url-hex-macro sl-url-unhex #'url-unhex-string)

;; yasnippet is disabled for term by 'spacemacs/force-yasnippet-off' in
;; spacemacs "layers/+completion/auto-completion/packages.el".
(with-eval-after-load 'yasnippet
  (defvar yas-snippet-dirs)
  (when-let* ((snippets-dir (expand-file-name "snippets/" sl-savefile-dir))
              ((file-exists-p snippets-dir)))
    (add-to-list 'yas-snippet-dirs snippets-dir)))

(define-advice undo-tree-make-history-save-file-name (:around (orig-fun file) sl-adv)
  "Check the return value, if it longer than 255, generate an MD5 value instead.
ORIG-FUN is the original function.
FILE is the filename.

For many file system, the file name (without dir) should less than 255.
Please refer http://wikipedia.org/wiki/Comparison_of_file_systems for detail."
  (let ((ret (funcall orig-fun file)))
    (if (< 255 (length ret))
        (funcall orig-fun (concat (md5 (file-name-directory file)) "/" (file-name-nondirectory file)))
      ret)))


(mapc 'load (seq-uniq
             (mapcar
              (lambda (x) (substring x 0 (string-match "\\.elc?\\(\\.gz\\)?$" x)))
              (directory-files (expand-file-name  "site-start.d" sl-root-dir)
                               t "^[0-9].+\\.elc?\\(\\.gz\\)?$"))))

(provide 'sl-init)
;;; init.el ends here
