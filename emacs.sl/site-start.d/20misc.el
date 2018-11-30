;;; misc --- the misc settings
;;; Commentary:
;;; Code:

;;(setq inhibit-default-init 1)

;; prefer chinese-gbk
(set-locale-environment "zh_CN.UTF-8")

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(recentf-mode t)

;;;;; use WindMove for easy navigate in multi frames
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(when (fboundp 'winner-mode)
  (winner-mode t))

;; desktop
(custom-set-variables
 '(desktop-load-locked-desktop t)
 '(desktop-buffers-not-to-save
   (concat "\\("
           "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|\\.org"
           "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
           "\\|\\*.*\\|\\*ECB.*"
           "\\|\\.pdf|\\)$")))
(eval-after-load 'desktop
  '(progn
     (defvar desktop-modes-not-to-save)
     (mapc (lambda (mode) (add-to-list 'desktop-modes-not-to-save mode))
           '(dired-mode Info-mode info-lookup-mode fundamental-mode))))

(when (and (fboundp 'desktop-save-mode) (not (daemonp)))
  (desktop-save-mode t))

(custom-set-variables
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
 '(dired-omit-size-limit nil))          ; don't limit this size

(add-hook 'dired-mode-hook
          (lambda ()
            (declare-function dired-omit-mode "dired-x.el")
            (dired-omit-mode t)))

(defun my-find-file-for-large-file ()
  "If file very large, turn off the follow minor-mode."
  (when (> (buffer-size) (* 512 1024))
    (when (fboundp 'global-hl-line-mode)
      (global-hl-line-mode -1))
    (when (fboundp 'whitespace-mode)
      (whitespace-mode -1))))
(add-hook 'find-file-hook 'my-find-file-for-large-file)

(defun sl-toggle-color ()
  "Toggle the background/foreground color."
  (interactive)
  (let* ((fgcolor (assoc 'foreground-color default-frame-alist))
         (bgcolor (assoc 'background-color default-frame-alist)))
    (cl-rotatef (cdr fgcolor) (cdr bgcolor))
    (set-foreground-color (cdr fgcolor))
    (set-background-color (cdr bgcolor))))

(defun sl-copy-full-path(&optional dir_only)
  (interactive "P")
  (let ((full-file-name (file-truename
                         (cond ((and dir_only buffer-file-name)
                                (file-name-directory (buffer-file-name)))
                               (buffer-file-name)
                               (default-directory)))))
    (message full-file-name)
    (kill-new full-file-name)))

(global-set-key (kbd "C-c . l k") 'sl-copy-full-path)

(provide '20misc)
;;; 20misc ends here
