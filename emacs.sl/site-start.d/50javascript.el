;;; 50web.el -- setup for python
;;; Commentary:
;;; Code:
;; use tern for javascript

(defvar sl-packages-list)
;; (add-to-list 'sl-packages-list 'tern)
;; (add-to-list 'sl-packages-list 'company-tern)

;; (cond ((fboundp 'tern-mode)
;;        (add-hook 'js-mode-hook (lambda () (tern-mode t)))
;;        (when (fboundp 'company-tern)
;;          (defvar company-backends)
;;          (eval-after-load 'company
;;            '(add-to-list 'company-backends 'company-tern))))
;;       ((featurep 'prelude-core) (require 'prelude-js))) ; the js2 mode maybe very slow

;; ;; js2-mode with flycheck very slow https://github.com/flycheck/flycheck/issues/1129
;; (with-eval-after-load 'flycheck
;;   ;; (semantic-mode 0)
;;   (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(add-to-list 'sl-packages-list 'tide)

(autoload 'tide-setup "tide")
(autoload 'tide-hl-identifier-mode "tide")
(autoload 'flycheck-mode "flycheck")
(defun sl-setup-tide-mode ()
  "Set up the tide mode."
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (eldoc-mode +1)
  ;; (flycheck-mode +1)
  ;; (defvar flycheck-check-syntax-automatically)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

(when (featurep 'tide)
  ;; aligns annotation to the right hand side
  (when (featurep 'company)
    (defvar company-tooltip-align-annotations)
    (setq company-tooltip-align-annotations t))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'js2-mode-hook #'sl-setup-tide-mode)
  (add-hook 'typescript-mode-hook #'sl-setup-tide-mode)
  ;; configure javascript-tide checker to run after your default javascript checker
  (with-eval-after-load 'tide
    (when (functionp 'flycheck-add-next-checker)
      (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))
  )

(provide '50javascript)
;;; 50javascript ends here
