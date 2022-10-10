;;; cc-mode --- settings for cc-mode
;;; Commentary:
;;; Code:

(defun sl-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8."
  (interactive)
  (setq-local tab-width (pcase tab-width (2 4) (4 8) (_ 2)))
  (setq-local c-basic-offset tab-width)
  (redraw-display)
  (message "tab-width is %s now" tab-width))

;; don't punctuation characters such as ‘;’ or ‘{’
;; (setq-default c-electric-flag nil)

(with-eval-after-load 'hideshow
  (declare-function 'hs-already-hidden-p "hideshow")
  (declare-function 'hs-show-all "hideshow")
  (declare-function 'hs-hide-all "hideshow")
  (defun sl-toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (hs-life-goes-on
     (if (hs-already-hidden-p)
         (hs-show-all)
       (hs-hide-all))))

  (define-key-after
    hs-minor-mode-menu
    [sl-toggle-hideshow-all]
    '(menu-item "(SL)Toggle Show/Hide all..." sl-toggle-hideshow-all
                :help "Toggle Show/Hide all in current buffer..")
    'Toggle\ Hiding)

  (define-key hs-minor-mode-map (kbd "C-M-;") 'sl-toggle-hideshow-all)
  (define-key hs-minor-mode-map (kbd "C-;") 'hs-toggle-hiding))


(add-hook 'c-mode-common-hook
          (lambda ()
            ;; c/c++ common settings
            (declare-function c-toggle-hungry-state "cc-cmds")
            (c-toggle-hungry-state 1)
            ;; (c-toggle-auto-hungry-state 1) ; hungry-delete and auto-newline
            ;; (c-set-offset 'case-label '+) ; indent the case
            ))


(provide '50cc-mode)
;;; 50cc-mode ends here
