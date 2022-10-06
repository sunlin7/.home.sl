;;; CEDET --- cedet configure
;;; Commentary:
;;; Code:
(declare-function 'xref-pop-marker-stack "xref")
(declare-function 'xref-push-marker-stack "xref")

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

(define-advice semantic-go-to-tag (:around (orig tag &optional parent) sl-adv)
  "Work with xref marker, and Center the tag after jumping.
ORIG is the original function.
TAG, PARENT is the param."
  (if sl-jump-from-user-interactive     ; the `semantic-change-function' will trigger `semantic-go-to-tag' also
      (condition-case err
          (progn
            (xref-push-marker-stack)
            (apply orig tag parent)
            (recenter find-function-recenter-line)
            (run-hooks 'find-function-after-hook))
        (error ;;if not found remove the tag saved in the ring
         (xref-pop-marker-stack)
         (signal (car err) (cdr err))))
    (apply orig tag parent)))
(declare-function 'semantic-tag-p "semantic/tag")
(declare-function 'semantic-tag-name "semantic/tag")
(declare-function 'semantic-tag-type "semantic/tag")
(declare-function 'semantic-tag-class "semantic/tag")
(declare-function 'semantic-tag-get-attribute "semantic/tag")
(declare-function 'semantic-fetch-tags "semantic/tag")
(autoload 'semantic-ia--fast-jump-helper "semantic/ia")

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

(with-eval-after-load 'semantic
  (defvar semantic-mode-map)
  (define-key semantic-mode-map (kbd "C-c , t") 'sl-select-local-tags)
  (define-key-after
    (lookup-key cedet-menu-map [navigate-menu])
    [semantic-select-local-tags]
    '(menu-item "(SL)Find Local Tags..." sl-select-local-tags
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
