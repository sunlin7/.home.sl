;;; misc --- the misc settings
;;; Commentary:
;;; Code:

;; prefer chinese-gbk
(set-locale-environment "zh_CN.UTF-8")

(custom-set-variables
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
 '(dired-omit-size-limit nil))          ; don't limit this size

(add-hook 'dired-mode-hook
          (lambda ()
            (declare-function dired-omit-mode "dired-x.el")
            (dired-omit-mode t)))

(defun sl-toggle-color ()
  "Toggle the background/foreground color."
  (interactive)
  (let* ((fgcolor (assoc 'foreground-color default-frame-alist))
         (bgcolor (assoc 'background-color default-frame-alist)))
    (cl-rotatef (cdr fgcolor) (cdr bgcolor))
    (set-foreground-color (cdr fgcolor))
    (set-background-color (cdr bgcolor))))

(defun sl-copy-full-path(&optional dir_only)
  (interactive "P")
  (let ((full-file-name (file-truename
                         (cond ((and dir_only buffer-file-name)
                                (file-name-directory (buffer-file-name)))
                               (buffer-file-name)
                               (default-directory)))))
    (message full-file-name)
    (kill-new full-file-name)))

(global-set-key (kbd "C-c . l k") 'sl-copy-full-path)

(provide '20misc)
;;; 20misc ends here
