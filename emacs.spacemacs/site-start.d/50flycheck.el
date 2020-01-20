;;; flycheck -- settings for flycheck
;;; Commentary:
;;; Code:
(autoload 'oref "eieio")
(autoload 'class-p "eieio-core")
(autoload 'object-of-class-p "eieio")
(autoload 'ede-current-project "ede/cpp-root")
(autoload 'ede-project-root-directory "ede/auto")
(autoload 'get-command-line "ede-compdb")
(autoload 'get-defines "ede-compdb")
(autoload 'get-includes "ede-compdb")
(autoload 'get-include-path "ede-compdb")

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
                          (oref cur-proj :system-include-path)
                          (mapcar
                           (lambda (prj-inc)
                             (if (string-prefix-p "/" prj-inc)
                                 ;; drop the "/" from an :include-path
                                 (expand-file-name (substring prj-inc 1) project-root)
                               prj-inc))
                           (oref cur-proj :include-path))))

      (setq-local flycheck-clang-definitions
                  (mapcar (lambda(defs)
                            (cond ((zerop (length (cdr defs))) (car defs))
                                  (t (concat (car defs) "=" (cdr defs)))))
                          (oref cur-proj :spp-table))))))

(defun sl-ede-compdb-flycheck-init ()
  "Setup the flycheck for ede-compdb."
  (defvar ede-object)
  (when (and ede-object (oref ede-object :compilation))
    (let* ((comp (oref ede-object :compilation))
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
                              ;; (string-trim (shell-command-to-string (concat python-shell-interpreter " -c 'from __future__ import print_function; import sys; print(sys.executable)'")))
                              (or (executable-find "python3") (executable-find "python2") "python")
                            python-shell-interpreter))
              )))

;; fix that the pipenv only find pylint in virtual evnvironment
;; (eval-after-load 'pipenv
;;   (advice-add 'pipenv-executable-find :around
;;               #'(lambda(ORIG executable)
;;                   (let ((res (funcall ORIG executable)))
;;                     (or res
;;                         (progn
;;                           (message "Warrning: %s not found in pipenv, try global one", executable)
;;                           (executable-find executable)))))))

(provide '50flycheck)
;;; 50flycheck ends here
