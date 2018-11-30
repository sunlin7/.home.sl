;;; open-with --- setings
;;; Commentary:
;;; Code:

(declare-function dired-get-file-for-visit "dired" nil)
(defun sl-dired-open-explorer-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (if (fboundp 'w32-shell-execute)
      (w32-shell-execute
       "open" (replace-regexp-in-string "/" "\\"
                                        (dired-get-file-for-visit) t t))
    (let ((process-connection-type nil))
      (call-process (or (executable-find "gvfs-open")
                        (executable-find "xdg-open")
                        (error "Can't find external tools"))
                    nil nil nil (dired-get-file-for-visit)))))

(eval-after-load 'dired
  '(progn
     (defvar dired-mode-map)
     (define-key dired-mode-map "E" 'sl-dired-open-explorer-file)
     (define-key dired-mode-map [menu-bar immediate open-explorer-file]
       '(menu-item "Open with explorer" sl-dired-open-explorer-file
                   :help "Open the file with explorer"))))

(provide '50openwith)
;;; 50openwith ends here
