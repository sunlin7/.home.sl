;;; smartparens --- the settings.
;;; Commentary:
;;; Code:

(defvar sl-packages-list)
(add-to-list 'sl-packages-list 'smartparens)

(add-to-list 'after-init-hook
             (lambda ()
               (when (fboundp 'smartparens-mode)
                 (smartparens-mode +1))))
(eval-after-load 'smartparens
  '(with-no-warnings
     ;; the "M-s" is prefix key for misc, so redefine to "M-s s".
     (define-key smartparens-mode-map (kbd "M-s") nil)
     (define-key smartparens-mode-map (kbd "M-s s") 'sp-splice-sexp)))
(provide '50smartparens)
;;; 50smartparens ends here
