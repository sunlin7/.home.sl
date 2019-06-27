;;; custom-set-variables --- the custom variables
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-frame-alist
    ((top . 0) (left . 0) (width . 110) (height . 40) ; get by `frame-height', `frame-width' when maximum
     (background-color . "black") (foreground-color . "white")))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(make-backup-files nil)
 '(tool-bar-mode nil)
 '(tab-width 4)
 '(x-select-enable-clipboard t) ;; enable share the clipboard with
 '(speedbar-tag-hierarchy-method nil) ;; turn off speedbar hierarchy
 '(diary-file (expand-file-name "diary" sl-savefile-dir))             ;; the diary-file name
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))
 '(term-color-blue ((t (:background "#7F7FFC" :foreground "#6060FC"))))
 '(whitespace-tab ((((class color) (background dark)) (:background "grey7")))))


(put 'dired-find-alternate-file 'disabled nil)
;; (setq stack-trace-on-error nil)

(provide '90custome-variables)
;;; 90custome-variables ends here
