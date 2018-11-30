;;; 50eim --- emacs imput method
;;; Commentary:
;;; Code:
;;;; no package site for eim, just check it manual
;; (package--add-to-archive-contents
;;  '(eim .
;;        [(1 5)
;; 	nil "Emacs Input method" tar
;; 	((:keywords "tools" "processes" "convenience")
;; 	 (:url . "https://github.com/wenbinye/emacs-eim"))])
;;  "gnu")
(eval-when-compile (defvar sl-site-lisp-dir))
(defvar sl-eim-path (expand-file-name "eim" sl-site-lisp-dir))
(defmacro sl-set-eim-insert-ascii-key(arg)
      `(local-set-key ";" (if ,arg 'eim-insert-ascii nil)))
(when (file-exists-p sl-eim-path)
  (add-to-list 'load-path sl-eim-path)
  (autoload 'eim-use-package "eim" "Another emacs input method")
  ;; Tooltip 暂时还不好用
  (eval-after-load "eim"
    '(setq-default eim-use-tooltip nil))

  (register-input-method
   "eim-wb" "euc-cn" 'eim-use-package
   "五笔" "极点五笔输入法" "wb-jidian.txt")
  (register-input-method
   "eim-py" "euc-cn" 'eim-use-package
   "拼音" "汉字拼音输入法" "py.txt")

  ;; 用 ; 暂时输入英文
  (autoload 'eim-insert-ascii "eim-extra" "Insert the ascii char")
  (add-hook 'eim-active-hook
            (lambda () (sl-set-eim-insert-ascii-key t)))
  (advice-add 'eim-inactivate :after #'(lambda (&rest r) (sl-set-eim-insert-ascii-key nil)))

  (add-hook
   'set-language-environment-hook
   (lambda ()
     (setq default-input-method "eim-wb")))
  (custom-set-variables '(default-input-method "eim-wb")))

(provide '50eim)
;;; 50eim ends here
