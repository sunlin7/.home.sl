;;; 50gdb --- gdb interface
;;; Commentary:
;;; Code:

(add-hook 'gdb-mode-hook
          #'(lambda ()
              (define-key gud-minor-mode-map [(f5)] 'gud-go)
              (define-key gud-minor-mode-map [(f6)] 'gud-print)
              (define-key gud-minor-mode-map [(S+f6)] 'gud-pstar)
              (define-key gud-minor-mode-map [(f7)] 'gud-step)
              (define-key gud-minor-mode-map [(f8)] 'gud-next)
              (define-key gud-minor-mode-map [(S-f8)] 'gud-finish)
              (define-key gud-minor-mode-map [(C-f8)] 'gud-until)
              (define-key gud-minor-mode-map [(f9)] 'gud-break)
              (define-key gud-minor-mode-map [(S-f9)] 'gud-remove)
              (define-key gud-minor-mode-map [(C-f9)] 'gud-tbreak)))

;; (define-advice gud-display-line (:after (true-file line) display-line-centered)
;;   "Center the line in the source frame"
;;   (when (and gud-overlay-arrow-position gdb-source-window)
;;     (with-selected-window gdb-source-window
;;       ;; (marker-buffer gud-overlay-arrow-position)
;;       (save-restriction
;;         ;; (forward-line (ad-get-arg 1))
;;         (recenter)))))

(provide '50gdb)
;;; 50gdb ends here
