;;; cc-mode --- settings for cc-mode
;;; Commentary:
;;; Code:

(defun sl-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8."
  (interactive)
  (setq-local tab-width (case tab-width (2 4) (4 8) (8 2)))
  (setq-local c-basic-offset tab-width)
  (redraw-display)
  (message "tab-width is %s now" tab-width))

;; don't punctuation characters such as ‘;’ or ‘{’
;; (setq-default c-electric-flag nil)
(autoload 'ede-current-project "ede")
(autoload 'ede-compile-target  "ede")
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
                                    "gcc -x c -o a - && ./a")))
        ((string= "c++-mode" major-mode)
         (let ((default-directory temporary-file-directory))
           (shell-command-on-region (point-min) (point-max)
                                    "g++ -x c++ -std=c++11 -o a - && ./a")))
        ((call-interactively 'compile))))

(eval-after-load 'hideshow
  '(progn
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
     (define-key hs-minor-mode-map (kbd "C-M-;") 'sl-toggle-hideshow-all)
     (define-key hs-minor-mode-map (kbd "C-;") 'hs-toggle-hiding)))


(eval-after-load 'cc-mode
  '(progn
     (defvar c-mode-base-map)
     (define-key c-mode-base-map [(f9)] 'sl-compile-project-or-file)
     (define-key c-mode-base-map [(f11)] 'gdb)))


(add-hook 'c-mode-common-hook
          (lambda ()
            ;; c/c++ common settings
            (declare-function c-toggle-hungry-state "cc-cmds")
            (c-toggle-auto-hungry-state 1) ; hungry-delete and auto-newline
            ;; (c-set-offset 'case-label '+) ; indent the case

            ;; for c++11 as default
            (when (eq major-mode 'c++-mode)
              (when (boundp 'company-clang-arguments)
                (setq-local company-clang-arguments (add-to-list 'company-clang-arguments "--std=c++11"))))
            ;; if rtags available, make sure it's the first of company-backend
            (when (and (boundp 'company-backends)
                       (member 'company-rtags company-backends)
                       (functionp 'rtags-is-indexed) (rtags-is-indexed))
              (setq-local company-backends (push 'company-rtags company-backends)))))


(provide '50cc-mode)
;;; 50cc-mode ends here
