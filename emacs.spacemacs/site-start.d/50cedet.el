;;; CEDET --- cedet configure
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook
          (lambda ()
            (when (locate-library "cedet/semantic")
              (global-ede-mode t)
              ;; (ede-enable-generic-projects)
              (semantic-mode  t)
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

(provide '50cedet)
;;; 50cedet ends here
