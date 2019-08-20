;;; CEDET --- cedet configure
;;; Commentary:
;;; Code:
(autoload 'xref-pop-marker-stack "xref")
(autoload 'xref-push-marker-stack "xref")
(autoload 'semantic-tag-p "semantic/tag")
(autoload 'semantic-tag-name "semantic/tag")
(autoload 'semantic-tag-type "semantic/tag")
(autoload 'semantic-tag-class "semantic/tag")
(autoload 'semantic-tag-get-attribute "semantic/tag")
(autoload 'semantic-fetch-tags "semantic/tag")
(autoload 'semantic-ia-fast-jump "semantic/ia")
(autoload 'semantic-ia--fast-jump-helper "semantic/ia")
(autoload 'global-semantic-mru-bookmark-mode "semantic/mru-bookmark")
;; (autoload 'global-semantic-idle-summary-mode "semantic/idle")
;; (autoload 'global-semantic-stickyfunc-mode   "semantic/util-modes")

(add-hook 'after-init-hook
          (lambda ()
            (semantic-mode  t)
            (global-ede-mode t)))

(eval-after-load 'semantic
  '(progn
     (global-semantic-mru-bookmark-mode t) ;; (semantic-mru-bookmark-mode)
     ;; (global-semantic-idle-summary-mode t)
     ;; (global-semantic-stickyfunc-mode   t)
     ;; (ede-enable-generic-projects)

     ;; make emacs-lisp-mode support.
     ;; (defvar semantic-new-buffer-setup-functions)
     ;; (add-to-list 'semantic-new-buffer-setup-functions
     ;;              '(emacs-lisp-mode . semantic-default-elisp-setup))
     ;; (add-to-list 'semantic-new-buffer-setup-functions
     ;;              '(lisp-mode . semantic-default-elisp-setup))
     ))

;; FIXME: the semantic-mode with js2-mode will extremly slow, so disable it
(add-hook 'js2-mode-hook (lambda ()
                           (message "disable semantic-mode in js2-mode.")
                           (setq forward-sexp-function nil)
                           (set (make-local-variable 'semantic-mode) nil)))
(add-hook 'json-mode-hook (lambda ()
                            (message "disable semantic-mode in json-mode")
                            (set (make-local-variable 'semantic-mode) nil)))

;; (require 'srecode/loaddefs) ; the `global-srecode-minor-mode` not loaded by default.
;; (global-srecode-minor-mode t)

;; (semantic-add-system-include "/usr/include/c++/4.6/bits" 'c++-mode)
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "/usr/include/sys/cdefs.h")
;; (semantic-c-reset-preprocessor-symbol-map)

;;(when (cedet-gnu-global-version-check t) ; follow function is autoload in bounded cedet
;; WARRING: the semantic search :include-dir recursive, that really slow in large project
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

;;(defalias 'cedet-called-interactively-p 'called-interactively-p)
;;(mapc 'load (directory-files "~/.emacs.sl/site-lisp/cedet-bzr/lisp/cedet/semantic/ectags/" t "^[0-9].*.el$"))
;;(when (semantic-ectags-test-version) ;; (cedet-ectag-version-check)
;;   (cons (semantic-load-enable-primary-ectags-support)
;;         (message "Exuberent CTags %s  - Good enough for CEDET." (car (semantic-ectags-version)))))

;; follow setting can optimize the search speed.
;; (setq-mode-local c-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))

(defun sl-semantic-go-to-tag-adv (orig tag &optional parent)
  "Work with xref marker, and Center the tag after jumping.
ORIG is the original function.
TAG, PARENT is the param."
  (condition-case err
      (progn
        (xref-push-marker-stack)
        (apply orig tag parent)
        (recenter find-function-recenter-line)
        (run-hooks 'find-function-after-hook))
    (error ;;if not found remove the tag saved in the ring
     (xref-pop-marker-stack)
     (signal (car err) (cdr err)))))
(advice-add 'semantic-go-to-tag :around #'sl-semantic-go-to-tag-adv)

(defun sl-semantic-get-tags (prefix tags)
  "Construct candidates from the list inside of tags.
PREFIX is for namespace or class.
TAGS is the tag from semantic."
  (require 'semantic/tag)
  (let ((ret nil))
    (mapc (lambda (tag)
            (when (listp tag)
              (let ((name (semantic-tag-name tag))
                    (type (semantic-tag-type tag))
                    (class (semantic-tag-class tag)))
                (cond ((and (stringp type)
                            (or (string= type "class")
                                (string= type "namespace")))
                       (setq ret
                             (append ret (sl-semantic-get-tags
                                      (concat prefix name "::")
                                      (semantic-tag-get-attribute tag :members)))))
                      ((or (eq class 'function) (eq class 'variable))
                       (let* ((parent (semantic-tag-get-attribute tag :parent))
                              (prefix (if parent (concat prefix parent "::") prefix))
                              (postfix (if (semantic-tag-get-attribute tag :prototype-flag)
                                           "@" "")))
                         (add-to-list 'ret (cons (concat prefix name postfix) tag))))))))
          tags)
    ret))

(defun sl-select-local-tags ()
  "Select the local tags."
  (interactive)
  (let* ((tag-list (sl-semantic-get-tags "" (semantic-fetch-tags))))
    (when tag-list
      (let* ((tag-name (completing-read "Tags: " (mapcar 'car tag-list)))
             ;; (tag (semantic-complete-read-tag-buffer-deep
             ;;       "Jump to symbol: " (assoc tag-name tag-list)))
             (tag (cdr (assoc tag-name tag-list))))
        (when (semantic-tag-p tag)
          (semantic-ia--fast-jump-helper tag))))))

(eval-after-load 'semantic
  '(progn
     (defvar semantic-mode-map)
     (define-key semantic-mode-map (kbd "C-c , t") 'sl-select-local-tags)
     (define-key-after
       (lookup-key cedet-menu-map [navigate-menu])
       [semantic-select-local-tags]
       '(menu-item "(SL)Find Local Tags..." sl-select-local-tags
                   :enable (and (semantic-active-p))
                   :help "Find tags in current buffer..")
       'semantic-symref-symbol)
     ))

(defun sl-cedet-directory-name-to-file-name (orig-fun file)
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

(advice-add 'cedet-directory-name-to-file-name :around #'sl-cedet-directory-name-to-file-name)

(provide '50cedet)
;;; 50cedet ends here
