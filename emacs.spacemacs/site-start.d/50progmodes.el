;;; progmodes --- settings for the program modes -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:

(defun sl-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8."
  (interactive)
  (setq-local tab-width (pcase tab-width (2 4) (4 8) (_ 2)))
  (setq-local c-basic-offset tab-width)
  (redraw-display)
  (message "tab-width is %s now" tab-width))

(defvar sl-packages-list)

;; The dtrt-indent guess tab-width based on the syntax, but not support emacs-lisp
(add-to-list 'sl-packages-list 'dtrt-indent)
(use-package dtrt-indent
  :hook (lua-mode-local-vars
         . (lambda () (unless (local-variable-p 'tab-width) (dtrt-indent-mode)))))

(add-to-list 'sl-packages-list
             '(guess-style :location (recipe :fetcher github :repo "nschum/guess-style")))
(use-package guess-style
  :commands (guess-style-guess-variable)
  :hook (emacs-lisp-mode-local-vars
         . (lambda () (unless (local-variable-p 'tab-width) (guess-style-guess-variable 'tab-width)))))

(use-package cc-mode
  :hook ((c-mode-local-vars c++-mode-local-vars)
         . (lambda () (unless (local-variable-p 'tab-width) (c-guess)))))

(with-eval-after-load 'hideshow
  (eval-and-compile (require 'hideshow))
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


(with-eval-after-load 'cc-vars
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; c/c++ common settings
              (declare-function c-toggle-hungry-state "cc-cmds")
              (c-toggle-hungry-state 1)
              ;; (c-toggle-auto-hungry-state 1) ; hungry-delete and auto-newline
              ;; (c-set-offset 'case-label '+) ; indent the case
              ;; don't punctuation characters such as ‘;’ or ‘{’
              ;; (c-toggle-electric-state -1)
              )))

(defcustom sl-preferred-c++-std nil
  "The default c++ standard.  Example: c++11."
  :type 'string
  :group 'c)

(with-eval-after-load 'quickrun
  (defvar quickrun--language-alist)
  (setf (alist-get ':command (alist-get "python" quickrun--language-alist nil nil #'string=)) "python3")
  ;;; gcc < 6.1 uses -std=gnu++98 by default, clang < 6.0 uses -std=gnu++98 by default
  (when sl-preferred-c++-std
    (dolist (lang '("c++/g++" "c++/clang++"))
      (if-let* ((cmd_list (alist-get lang quickrun--language-alist nil nil #'string=))
                (exec (alist-get ':exec cmd_list))
                (compile (car exec)))
          (unless (string-match "-std=" compile)
            (setf (car exec) (concat compile " -std=" sl-preferred-c++-std)))))))

(with-eval-after-load 'menu-bar
  (declare-function spacemacs/quickrun "")
  (easy-menu-add-item
   nil '("Tools")
   '["Quick Run..." spacemacs/quickrun :help "Quick run the code"]
   "Compile...")
  (global-set-key [(f9)] #'spacemacs/quickrun))


;;; flycheck -- settings for flycheck

(defvar flycheck-clang-args)
(defvar flycheck-clang-language-standard)

(defun sl-flycheck-c++-std ()
  "Change the g++/clang++ language standards param -std to c++11."
  (when (eq major-mode 'c++-mode)
    (dolist (x '(flycheck-clang-language-standard flycheck-gcc-language-standard))
      (when (string-empty-p (or (symbol-value x) ""))
        (set (make-local-variable x) sl-preferred-c++-std)))))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'sl-flycheck-c++-std)
  ;; FIXME: disable clang warning on struct init syntax "struct a = {0}".
  (add-to-list 'flycheck-clang-args "-Wno-missing-field-initializers"))

;; fix that the pipenv only find pylint in virtual evnvironment
;; (with-eval-after-load 'pipenv
;;   (define-advice pipenv-executable-find (:around (ORIG executable))
;;     (when (funcall ORIG executable)
;;       (message "Warrning: %s not found in pipenv, try global one" executable)
;;       (executable-find executable))))

;;; gdb interface
(with-eval-after-load 'gdb-mi
  (eval-and-compile (require 'gdb-mi))
  (define-key gud-minor-mode-map [(f5)] 'gud-go)
  (define-key gud-minor-mode-map [(f6)] 'gud-print)
  (define-key gud-minor-mode-map [(S+f6)] 'gud-pstar)
  (define-key gud-minor-mode-map [(f7)] 'gud-step)
  (define-key gud-minor-mode-map [(f8)] 'gud-next)
  (define-key gud-minor-mode-map [(S-f8)] 'gud-finish)
  (define-key gud-minor-mode-map [(C-f8)] 'gud-until)
  (define-key gud-minor-mode-map [(f9)] 'gud-break)
  (define-key gud-minor-mode-map [(S-f9)] 'gud-remove)
  (define-key gud-minor-mode-map [(C-f9)] 'gud-tbreak))

(with-eval-after-load 'menu-bar
  (define-key menu-bar-tools-menu [(f11)] #'gdb))

;; (define-advice gud-display-line (:after (true-file line) display-line-centered)
;;   "Center the line in the source frame"
;;   (when (and gud-overlay-arrow-position gdb-source-window)
;;     (with-selected-window gdb-source-window
;;       ;; (marker-buffer gud-overlay-arrow-position)
;;       (save-restriction
;;         ;; (forward-line (ad-get-arg 1))
;;         (recenter)))))


(provide '50progmodes)
;;; 50progmodes.el ends here
