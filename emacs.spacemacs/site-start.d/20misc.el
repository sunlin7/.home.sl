;;; misc --- the misc settings      -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:

;; prefer chinese-gbk
;; (set-locale-environment "zh_CN.UTF-8")

(custom-set-variables
 '(delete-selection-mode t)
 '(diary-file (expand-file-name "diary" sl-savefile-dir)) ;; the diary-file name
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*"))

(add-hook 'dired-mode-hook #'dired-omit-mode)

(defun sl-toggle-color ()
  "Toggle the background/foreground color."
  (interactive)
  (let* ((fgcolor (face-attribute 'default :foreground))
         (bgcolor (face-attribute 'default :background)))
    (set-foreground-color bgcolor)
    (set-background-color fgcolor)))

(defmacro sl-url-hex-macro (fsym func)
  "Run the function `func' over the region between START and END in current buffer."
  `(defun ,fsym (start end)
     "Run the url-hexify-string/url-unhex-string interactively."
     (interactive "r")
     (save-excursion
       (insert (funcall ,func (delete-and-extract-region start end))))))
(sl-url-hex-macro sl-url-hex #'url-hexify-string)
(sl-url-hex-macro sl-url-unhex #'url-unhex-string)

(with-eval-after-load 'dired
  (define-key dired-mode-map [menu-bar operate open-file-or-directory-in-external-app]
              '(menu-item "Open with external app" spacemacs/open-file-or-directory-in-external-app
                          :help "Open the file with external application"))
  (define-key dired-mode-map "E" #'spacemacs/open-file-or-directory-in-external-app))

;; yasnippet is disabled for term by 'spacemacs/force-yasnippet-off' in
;; spacemacs "layers/+completion/auto-completion/packages.el".
(with-eval-after-load 'yasnippet
  (when-let* ((snippets-dir (expand-file-name "snippets/" sl-savefile-dir))
              (file-exists-p snippets-dir))
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

(provide '20misc)
;;; 20misc ends here
