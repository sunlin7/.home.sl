;;; 50gdb --- gdb interface
;;; Commentary:
;;; Code:

;; (add-hook 'gdb-mode-hook
;;           '(lambda ()
;;              (define-key gud-minor-mode-map [(f9)] 'gud-run)
;;              (define-key gud-minor-mode-map [(f5)] 'gud-step)
;;              (define-key gud-minor-mode-map [(f7)] 'gud-next)
;;              (define-key gud-minor-mode-map [(f6)] 'gud-finish)
;;              (define-key gud-minor-mode-map [(f8)] 'gud-go)))

;; (defadvice gud-display-line (after gud-display-line-centered activate)
;;   "Center the line in the window."
;;   (when (and gud-overlay-arrow-position gdb-source-window)
;;     (with-selected-window gdb-source-window
;;       ;; (marker-buffer gud-overlay-arrow-position)
;;       (save-restriction
;;         (forward-line (ad-get-arg 1))
;;         (recenter)))))

(provide '50gdb)
;;; 50gdb ends here
