;;; progmodes --- settings for the program modes
;;; Commentary:
;;; Code:

(defun sl-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8."
  (interactive)
  (setq-local tab-width (pcase tab-width (2 4) (4 8) (_ 2)))
  (setq-local c-basic-offset tab-width)
  (redraw-display)
  (message "tab-width is %s now" tab-width))


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
            ;; don't punctuation characters such as ‘;’ or ‘{’
            ;; (c-toggle-electric-state -1)
            ))


;;; CEDET configure
(add-hook 'after-init-hook
          (lambda ()
            (when (locate-library "cedet/semantic")
              (global-ede-mode t)
              ;; (ede-enable-generic-projects)
              (semantic-mode t)
              ;; (semantic-add-system-include "/usr/include/c++/4.6/bits" 'c++-mode)
              ;; (semantic-c-reset-preprocessor-symbol-map)
              ;;;; optimize the search speed
              ;; (setq-mode-local c-mode semanticdb-find-default-throttle
              ;;                  '(project unloaded system recursive))
              ;; (add-to-list 'semantic-default-submodes
              ;;              'global-semantic-mru-bookmark-mode)
              ;;;; FIXME: disable semantic-mode in js2-mode for it's extremly slow
              (with-eval-after-load 'js2-mode
                (setq-mode-local js2-mode
                                 semantic-mode nil
                                 forward-sexp-function nil))
              (with-eval-after-load 'json-mode
                (setq-mode-local json-mode semantic-mode nil))
              ;; (require 'srecode)
              ;; (global-srecode-minor-mode t)
              )))

(with-eval-after-load 'semantic
  (define-key semantic-mode-map (kbd "C-c , t") #'spacemacs/helm-jump-in-buffer)
  (define-key-after
    (lookup-key cedet-menu-map [navigate-menu])
    [semantic-select-local-tags]
    '(menu-item "(SL)Find Local Tags..." spacemacs/helm-jump-in-buffer
                :enable (and (semantic-active-p))
                :help "Find tags in current buffer..")
    'semantic-symref-symbol)
  ;;;; to setup the emacs-lisp-mode
  ;; (defvar semantic-new-buffer-setup-functions)
  ;; (add-to-list 'semantic-new-buffer-setup-functions
  ;;              '(emacs-lisp-mode . semantic-default-elisp-setup))
  )

(with-eval-after-load 'ede
  (define-key ede-minor-mode-map [(f9)] #'quickrun)
  (define-key cedet-menu-map [ede-quick-run]
              '(menu-item "Quick Run" quickrun
	                        :visible global-ede-mode)))

(define-advice cedet-directory-name-to-file-name (:around (orig-fun file) sl-adv)
  "Check the return value, if it longer than 255, generate an MD5 value instead.
ORIG-FUN is the original function.
FILE is the filename.

For many file system, the file name (without dir) should less than 255.
Please refer http://wikipedia.org/wiki/Comparison_of_file_systems for detail."
  (defvar semanticdb-default-file-name)
  (let ((ret (funcall orig-fun file))
        (flen (length semanticdb-default-file-name)))
    (if (< (+ flen (length ret)) 255)
        ret
      (concat (md5 (file-name-directory file)) "!" (file-name-nondirectory file)))))


;;; flycheck -- settings for flycheck
(declare-function 'oref "eieio")
(declare-function 'class-p "eieio-core")
(declare-function 'object-of-class-p "eieio")
(declare-function 'ede-current-project "ede")
(declare-function 'ede-project-root-directory "ede/auto")

(defvar flycheck-clang-args)
(defvar flycheck-clang-blocks)
(defvar flycheck-clang-definitions)
(defvar flycheck-clang-include-path)
(defvar flycheck-clang-includes)
(defvar flycheck-clang-language-standard)
(defvar flycheck-clang-ms-extensions)
(defvar flycheck-clang-no-exceptions)
(defvar flycheck-clang-no-rtti)
(defvar flycheck-clang-standard-library)

(defun sl-ede-cpp-root-project-flycheck-init ()
  "Setup the flycheck for ede-cpp-root-project."
  (let* ((cur-proj (ede-current-project))
         (project-root (when cur-proj (ede-project-root-directory cur-proj))))
    (when (and cur-proj
               project-root
               (object-of-class-p cur-proj 'ede-cpp-root-project))
      (setq-local flycheck-clang-include-path
                  (append flycheck-clang-include-path
                          (oref cur-proj system-include-path)
                          (mapcar
                           (lambda (prj-inc)
                             (if (string-prefix-p "/" prj-inc)
                                 ;; drop the "/" from an :include-path
                                 (expand-file-name (substring prj-inc 1) project-root)
                               prj-inc))
                           (oref cur-proj include-path))))

      (setq-local flycheck-clang-definitions
                  (mapcar (lambda(defs)
                            (cond ((zerop (length (cdr defs))) (car defs))
                                  (t (concat (car defs) "=" (cdr defs)))))
                          (oref cur-proj spp-table))))))

(declare-function 'get-command-line "ede-compdb")
(declare-function 'get-defines "ede-compdb")
(declare-function 'get-includes "ede-compdb")
(declare-function 'get-include-path "ede-compdb")
(defun sl-ede-compdb-flycheck-init ()
  "Setup the flycheck for ede-compdb."
  (defvar ede-object)
  (when (and ede-object
             (class-p 'ede-compdb-project)
             (object-of-class-p ede-object 'ede-compdb-project))
    (let* ((comp (oref ede-object compilation))
           (cmd (get-command-line comp)))
      ;; Configure flycheck clang checker.
      ;; TODO: configure gcc checker also
      (when (string-match " \\(-O[0-9]\\) " cmd)
        (add-to-list 'flycheck-clang-args (match-string 1 cmd)))
      (when (string-match " \\(-fPIC\\) " cmd)
        (add-to-list 'flycheck-clang-args (match-string 1 cmd)))
      (when (string-match " -std=\\([^ ]+\\)" cmd)
        (setq-local flycheck-clang-language-standard (match-string 1 cmd)))
      (when (string-match " -stdlib=\\([^ ]+\\)" cmd)
        (setq-local flycheck-clang-standard-library (match-string 1 cmd)))
      (when (string-match " -fms-extensions " cmd)
        (setq-local flycheck-clang-ms-extensions t))
      (when (string-match " -fno-exceptions " cmd)
        (setq-local flycheck-clang-no-exceptions t))
      (when (string-match " -fno-rtti " cmd)
        (setq-local flycheck-clang-no-rtti t))
      (when (string-match " -fblocks " cmd)
        (setq-local flycheck-clang-blocks t))
      (setq-local flycheck-clang-includes (get-includes comp))
      (setq-local flycheck-clang-definitions (get-defines comp))
      (setq-local flycheck-clang-include-path (get-include-path comp t)))))

(defun sl-ede-flycheck-init ()
  "Setup the flycheck for ede projects."
  ;; FIXME: disable clang warning on struct init syntax "struct a = {0}".
  (add-to-list 'flycheck-clang-args "-Wno-missing-field-initializers")

  (when (equal major-mode 'c++-mode)
    (when (and (boundp 'flycheck-clang-language-standard)
               (not (string-empty-p flycheck-clang-language-standard)))
      (setq-local flycheck-clang-language-standard "c++11"))
    (when (and (boundp 'flycheck-gcc-language-standard)
               (not (string-empty-p flycheck-gcc-language-standard)))
      (setq-local flycheck-gcc-language-standard "c++11")))

  (let ((cur-proj (ede-current-project)))
    (when cur-proj
      (when (and (class-p 'ede-cpp-root-project)
                 (object-of-class-p cur-proj 'ede-cpp-root-project))
        (sl-ede-cpp-root-project-flycheck-init))
      (when (and (class-p 'ede-compdb-project)
                 (object-of-class-p cur-proj 'ede-compdb-project))
        (sl-ede-compdb-flycheck-init)))))

(with-eval-after-load 'flycheck
  (add-hook 'ede-compdb-project-rescan-hook #'sl-ede-compdb-flycheck-init)
  (add-hook 'ede-minor-mode-hook #'sl-ede-flycheck-init)
  (add-hook 'flycheck-mode-hook #'sl-ede-flycheck-init)

  (when (functionp 'flycheck-pos-tip-mode)
    (flycheck-pos-tip-mode)))

;; fix the pycompile-checker executable
(add-hook 'python-mode-hook
          (lambda ()
            (when (and (boundp flycheck-python-pycompile-executable)
                       (not flycheck-python-pycompile-executable))
              (setq-local flycheck-python-pycompile-executable
                          (if (string= (file-name-base python-shell-interpreter) "ipython")
                              (or (executable-find "python3") (executable-find "python2") "python")
                            python-shell-interpreter)))))

;; fix that the pipenv only find pylint in virtual evnvironment
;; (with-eval-after-load 'pipenv
;;   (define-advice pipenv-executable-find (:around (ORIG executable))
;;     (when (funcall ORIG executable)
;;       (progn
;;         (message "Warrning: %s not found in pipenv, try global one", executable)
;;         (executable-find executable)))))

;;; gdb interface
(add-hook 'gdb-mode-hook
          #'(lambda ()
              (define-key gud-minor-mode-map [(f5)] 'gud-go)
              (define-key gud-minor-mode-map [(f6)] 'gud-print)
              (define-key gud-minor-mode-map [(S+f6)] 'gud-pstar)
              (define-key gud-minor-mode-map [(f7)] 'gud-step)
              (define-key gud-minor-mode-map [(f8)] 'gud-next)
              (define-key gud-minor-mode-map [(S-f8)] 'gud-finish)
              (define-key gud-minor-mode-map [(C-f8)] 'gud-until)
              (define-key gud-minor-mode-map [(f9)] 'gud-break)
              (define-key gud-minor-mode-map [(S-f9)] 'gud-remove)
              (define-key gud-minor-mode-map [(C-f9)] 'gud-tbreak)))

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
;;; 50progmodes ends here