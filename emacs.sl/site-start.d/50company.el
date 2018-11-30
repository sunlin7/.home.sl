;;; company --- company configure
;; -*-emacs-lisp-*-
;;; Commentary:
;;; Code:
(add-hook
 'after-init-hook
 (lambda ()
   (defvar sl-complation-engine)
   (when (and (eq sl-complation-engine 'company) (fboundp 'global-company-mode))
     (global-company-mode))))
(provide '50company)
;;; 50company ends here
