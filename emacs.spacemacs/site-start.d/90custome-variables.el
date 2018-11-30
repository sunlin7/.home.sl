;;; custom-set-variables --- the custom variables
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-frame-alist
    '(((top . 0) (left . 0) (width . 110) (height . 40) ; get by `(frame-height)', `(frame-width)' when frame is maximum
      ;; (background-color . "black") (foreground-color . "white")
      )))
 '(delete-selection-mode t)
 '(diary-file (expand-file-name "diary" sl-savefile-dir))             ;; the diary-file name
 )

(put 'dired-find-alternate-file 'disabled nil)
;; (setq stack-trace-on-error nil)

(provide '90custome-variables)
;;; 90custome-variables ends here
