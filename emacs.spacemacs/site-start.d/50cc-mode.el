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
                                    "gcc -g -O0 -x c -std=gnu11 -o a - && ./a")))
        ((string= "c++-mode" major-mode)
         (let ((default-directory temporary-file-directory))
           (shell-command-on-region (point-min) (point-max)
                                    "g++ -g -O0 -x c++ -std=c++11 -o a - && ./a")))
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

     (define-key-after
       hs-minor-mode-menu
       [\(SL\)Toggle\ Show/Hide\ all]
       '(menu-item "(SL)Toggle Show/Hide all..." sl-toggle-hideshow-all
                   :help "Toggle Show/Hide all in current buffer..")
       'Toggle\ Hiding)

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
            (c-toggle-hungry-state 1)
            ;; (c-toggle-auto-hungry-state 1) ; hungry-delete and auto-newline
            ;; (c-set-offset 'case-label '+) ; indent the case

            ;; for c++11 as default
            (when (eq major-mode 'c++-mode)
              (when (boundp 'company-clang-arguments)
                (setq-local company-clang-arguments (add-to-list 'company-clang-arguments "--std=c++11"))))
            ))


(defun sl-setup-short-keys-for-jumper ()
  "Setup the short keys for jumping."
  (let ((mode-map (intern (format "spacemacs-%s-map-root-map" major-mode))))
    (when (and (boundp 'spacemacs-jump-handlers) (boundp mode-map))
      (define-key (symbol-value mode-map) (kbd "C-.") 'xref-find-definitions)
      (define-key (symbol-value mode-map) (kbd "C->") 'xref-find-references)
      (define-key (symbol-value mode-map) (kbd "C-,") 'xref-pop-marker-stack))))

(add-hook 'after-change-major-mode-hook
          #'sl-setup-short-keys-for-jumper)

(eval-after-load 'python
  '(progn
     (unless (fboundp 'python-shell-send-statement)
       (declare-function 'python-shell-send-region "python")
       (defun python-shell-send-statement (&optional beg end)
         "This function should exist after emacs27 and later."
         (interactive)
         (if (region-active-p)
             (call-interactively #'python-shell-send-region)
           (python-shell-send-region
            (save-excursion (python-nav-beginning-of-statement))
            (save-excursion (python-nav-end-of-statement)))))
       (defvar python-mode-map)
       (define-key python-mode-map "\C-c\C-e" 'python-shell-send-statement)
       (define-key-after
         (lookup-key python-mode-map [menu-bar Python])
         [Eval\ statement]
         '(menu-item "Eval statement/region" python-shell-send-statement
                     :help "Eval statement or region in inferior Python session.")
         'Eval\ Statement))))

(provide '50cc-mode)
;;; 50cc-mode ends here
