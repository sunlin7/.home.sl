;;; yasnippet --- settings  -*-emacs-lisp-*-
;;; Commentary:
;;; Code:
(defvar sl-packages-list)
(add-to-list 'sl-packages-list 'yasnippet 'yasnippet-snippets)

(autoload 'yas-load-directory "yasnippet")
(add-hook
 'after-init-hook
 (lambda ()
   (when (fboundp 'yas-global-mode)
     ;; set the sinippets-dirs then enable yas-global-mode
     (yas-global-mode t)
     (defvar sl-savefile-dir)
     (let ((snippets-dir (expand-file-name "snippets/" sl-savefile-dir)))
       (when (file-exists-p snippets-dir)
         (yas-load-directory snippets-dir)))
     (defun sl-disable-yas-minor-mode ()
       "Disable the `yas-minor-mode' for not well unsupport major-mode."
       (when (fboundp 'yas-minor-mode)
         (yas-minor-mode -1)))
     (add-hook 'term-mode-hook 'sl-disable-yas-minor-mode)      ; TAB key conflict, turn yas off
     (add-hook 'inferior-python-mode-hook 'sl-disable-yas-minor-mode))))

(provide '50yasnippet)
;;; 50yasnippet ends here
