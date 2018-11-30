;;; cc-mode --- settings for cc-mode
;;; Commentary:
;;; Code:

(custom-set-variables
 '(c-default-style "linux")
 ;; the follow variables will auto becomes buffer-local,
 ;; use `custom-set-variables' or `setq-default' for global
 '(c-basic-offset 4)
 '(indent-tabs-mode nil))

(defun toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8."
  (interactive)
  (setq tab-width (if (= tab-width 8) 4 8))
  (setq-default c-basic-offset tab-width)
  (redraw-display))

(if (functionp 'c-or-c++-mode)
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))
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
         (start-file-process "compile"
                             (get-buffer-create "Compile") "make"))
        ((string= "c-mode" major-mode)
         (let ((default-directory temporary-file-directory))
           (shell-command-on-region (point-min) (point-max)
                                    "gcc -x c -o a - && ./a")))
        ((string= "c++-mode" major-mode)
         (let ((default-directory temporary-file-directory))
           (shell-command-on-region (point-min) (point-max)
                                    "g++ -x c++ -std=c++0x -o a - && ./a")))
        ((call-interactively 'compile))))
;;;;; c/c++ common settings
(add-hook 'c-mode-common-hook
          (lambda ()
            (require 'cc-mode)
            ;; hungry-delete and auto-newline
            ;; (c-toggle-auto-hungry-state t)
            (declare-function c-toggle-hungry-state "cc-cmds")
            (c-toggle-hungry-state t)
            (defvar c-mode-base-map)
            ;; (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
            (define-key c-mode-base-map [(f9)] 'sl-compile-project-or-file)
            (define-key c-mode-base-map [(f11)] 'gdb)
            ;; (c-set-offset 'case-label '+) ; indent the case
            ;; c preprocessor
            (custom-set-variables
             '(c-macro-shrink-window-flag t)
             ;; (setq c-macro-preprocessor "cpp") ; the default value enough
             ;; (setq c-macro-cppflags " ")
             '(c-macro-prompt-flag t))))

;; when use press the key map, the package will autoload, and function applied.
(autoload 'hs-already-hidden-p "hideshow"
  "Return non-nil if point is in an already-hidden block, otherwise nil." nil nil)
(defvar sl-hs-hideshow-all nil "Current state of hideshow for toggling all.")
(defun sl-toggle-hideshow-all ()
  "Toggle hideshow all."
  (interactive)
  (require 'hideshow)
  (declare-function hs-show-all "hideshow")
  (declare-function hs-hide-all "hideshow")
  (setq sl-hs-hideshow-all
        (cond ((boundp 'sl-hs-hideshow-all) (not sl-hs-hideshow-all))
              (t (hs-already-hidden-p)))) ; use hs-already-hidden-p as initial value
  (if sl-hs-hideshow-all
      (hs-hide-all)
    (hs-show-all)))
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; the hideshow will be loaded by calling any function in it, then
            ;; the hs-minor-mode will on by variable hs-minor-mode.
            (hs-minor-mode t)      ; (setq hs-minor-mode t) seems not work
            (local-set-key (kbd "C-M-;") 'sl-toggle-hideshow-all)
            (local-set-key [?\C-\;] 'hs-toggle-hiding)))
(provide '50cc-mode)
;;; 50cc-mode ends here
