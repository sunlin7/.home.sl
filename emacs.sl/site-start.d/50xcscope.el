;;; xcscope --- settings
;;; Commentary:
;;; Code:

(defvar sl-packages-list)
(add-to-list 'sl-packages-list 'xcscope)

;; (add-hook
;;  'after-init-hook
;;  (lambda ()
;;    (when (fboundp 'cscope-setup)
;;      (cscope-setup)
;;      (custom-set-variables
;;       '(cscope-option-do-not-update-database t)
;;       '(cscope-display-cscope-buffer nil))
;;      (defvar cscope-minor-mode-keymap)
;;      (define-key cscope-minor-mode-keymap "\e]" 'cscope-find-global-definition-no-prompting)
;;      (define-key cscope-minor-mode-keymap "\e[" 'cscope-pop-mark)
;;      (define-key cscope-minor-mode-keymap "\en" 'cscope-history-forward-line-current-result)
;;      (define-key cscope-minor-mode-keymap "\ep" 'cscope-history-backward-line-current-result)
;;      (define-key cscope-minor-mode-keymap "\eN" 'cscope-history-forward-line)
;;      (define-key cscope-minor-mode-keymap "\eP" 'cscope-history-backward-line)
;;      )))

(provide '50xcscope)
;;; 50xcscope ends here
