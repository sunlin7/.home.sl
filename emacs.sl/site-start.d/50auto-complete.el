;;; auto-complete --- auto-complete configure
;; -*-emacs-lisp-*-
;;; Commentary:
;;; Code:

(defvar sl-packages-list)
(add-to-list 'sl-packages-list 'auto-complete)

(add-hook
 'after-init-hook
 (lambda ()
   (defvar sl-complation-engine)
   (when (and (eq sl-complation-engine 'auto-complete) (fboundp 'ac-config-default))
     ;;(add-to-list 'ac-dictionary-directories "~/.emacs.sl/site-lisp/auto-complete.git/dict")
     (ac-config-default)

     ;; set the trigger key so that it can work together with yasnippet on tab key
     ;; if the word exists in yasnippet, pressing tab will cause yasnippet to
     ;; activate, otherwise, auto-complete will
     ;; (ac-set-trigger-key "TAB")
     ;; (ac-set-trigger-key "<tab>")

     ;; (setq ac-auto-start nil)
     (custom-set-variables
      '(ac-quick-help-delay 0.5))

     (declare-function global-auto-complete-mode "auto-complete")
     (global-auto-complete-mode t)
     (add-hook 'c-mode-common-hook
               (lambda ()
                 (when (and semantic-mode (boundp 'ac-sources))
                   (setq ac-sources '(ac-source-semantic ac-source-semantic-raw)))))
     ;; (define-key ac-mode-map [(control tab)] 'auto-complete)
     ;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
     ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
     )))
(provide '50auto-complete)
;;; 50auto-complete ends here
