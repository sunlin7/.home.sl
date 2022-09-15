;;; cc-mode --- settings for cc-mode
;;; Commentary:
;;; Code:

(defun sl-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8."
  (interactive)
  (setq-local tab-width (pcase tab-width (2 4) (4 8) (t 2)))
  (setq-local c-basic-offset tab-width)
  (redraw-display)
  (message "tab-width is %s now" tab-width))

;; don't punctuation characters such as ‘;’ or ‘{’
;; (setq-default c-electric-flag nil)
(declare-function 'ede-current-project "ede")
(declare-function 'ede-compile-target  "ede")
(defun sl-compile-project-or-file ()
  "Compile current file just smart."
  (interactive)
  (cond ((and (boundp 'ede-minor-mode) ede-minor-mode (ede-current-project))
         (ede-compile-target))
        ((file-readable-p "Makefile")
         (compile compile-command))
        ((file-readable-p "makefile")
         (compile compile-command))
        ((string= "c-mode" major-mode)
         (let ((default-directory temporary-file-directory))
           (shell-command-on-region (point-min) (point-max)
                                    "gcc -g -O0 -x c -std=gnu11 -o a - && ./a")))
        ((string= "c++-mode" major-mode)
         (let ((default-directory temporary-file-directory))
           (shell-command-on-region (point-min) (point-max)
                                    "g++ -g -O0 -x c++ -std=c++11 -o a - && ./a")))
        ((call-interactively 'compile))))

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
    [\(SL\)Toggle\ Show/Hide\ all]
    '(menu-item "(SL)Toggle Show/Hide all..." sl-toggle-hideshow-all
                :help "Toggle Show/Hide all in current buffer..")
    'Toggle\ Hiding)

  (define-key hs-minor-mode-map (kbd "C-M-;") 'sl-toggle-hideshow-all)
  (define-key hs-minor-mode-map (kbd "C-;") 'hs-toggle-hiding))


(with-eval-after-load 'cc-mode
  (defvar c-mode-base-map)
  (define-key c-mode-base-map [(f9)] 'sl-compile-project-or-file)
  (define-key c-mode-base-map [(f11)] 'gdb))


(add-hook 'c-mode-common-hook
          (lambda ()
            ;; c/c++ common settings
            (declare-function c-toggle-hungry-state "cc-cmds")
            (c-toggle-hungry-state 1)
            ;; (c-toggle-auto-hungry-state 1) ; hungry-delete and auto-newline
            ;; (c-set-offset 'case-label '+) ; indent the case

            ;; for c++11 as default
            (when (eq major-mode 'c++-mode)
              (when (boundp 'company-clang-arguments)
                (setq-local company-clang-arguments (add-to-list 'company-clang-arguments "--std=c++11"))))
            ))


(provide '50cc-mode)
;;; 50cc-mode ends here
