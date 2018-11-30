;;; CEDET --- cedet configure
;;; Commentary:
;;; Code:
;;; for emacs-lisp-modle
;; make emacs-lisp-mode support.

(autoload 'rtags-buffer-status "rtags")
(autoload 'rtags-executable-find "rtags")
(autoload 'rtags-location-stack-back "rtags")
(autoload 'rtags-find-symbol-at-point "rtags")
(autoload 'rtags-enable-standard-keybindings "rtags")
(autoload 'rtags-start-process-unless-running "rtags")
(autoload 'semantic-goto-definition "50cedet")
(autoload 'semantic-mru-bookmark-mode "semantic/mru-bookmark")
(autoload 'semantic-idle-summary-mode "semantic/idle")
(defvar rtags-last-request-not-indexed)

;; helper variables
(defvar sl-rtags-available (and (executable-find "rc")) "The rtags is available or not.")
(defvar sl-rtags-running nil "The rtags is running for buffer or not.")

(defun sl-tags-find-symbol-at-point (&optional prefix)
  "Find the symbox at point, PREFIX is optional."
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
      (semantic-goto-definition)))

(defun sl-rtags-setup ()
  "Setup the rtags for new buffer."
  ;; try to start the rdm server
  (defvar rtags-rdm-process)
  (when (not (and (processp rtags-rdm-process)
                  (eq (process-status rtags-rdm-process) 'run)))
    (rtags-start-process-unless-running))

  (set (make-local-variable 'sl-rtags-running) (and (rtags-buffer-status)))
  (when sl-rtags-running
    ;; (semantic-mru-bookmark-mode nil) ;; disable cedet
    ;; (semantic-idle-summary-mode nil)
    ;; enable then rtags backend for company
    (defvar company-backends)
    (add-to-list 'company-backends 'company-rtags)

    (local-set-key (kbd "M-.") #'sl-tags-find-symbol-at-point)
    (local-set-key (kbd "M-,") #'rtags-location-stack-back))
)

(when (fboundp 'rtags-find-symbol)
  (rtags-enable-standard-keybindings)
  (add-hook 'c-mode-common-hook #'sl-rtags-setup))

(eval-after-load "flycheck"
  '(when (featurep 'flycheck-rtags)
     (require 'flycheck-rtags)))


(provide '50rtags)
;;; 50rtags ends here
