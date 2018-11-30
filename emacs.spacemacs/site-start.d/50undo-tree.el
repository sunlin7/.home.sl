;;; 50undo-tree -- confige the undo-tree
;;; Commentary:
;;; Code:
(defun sl-undo-tree-make-history-save-file-name (orig-fun file)
  "Check the return value, if it longer than 255, generate an MD5 value instead.
ORIG-FUN is the original function.
FILE is the filename.

For many file system, the file name (without dir) should less than 255.
Please refer http://wikipedia.org/wiki/Comparison_of_file_systems for detail."
  (let ((ret (funcall orig-fun file)))
    (if (< (length ret) 255)
        ret
      (funcall orig-fun (concat (md5 (file-name-directory file)) "/" (file-name-nondirectory file))))))

(advice-add 'undo-tree-make-history-save-file-name :around #'sl-undo-tree-make-history-save-file-name)
;; (advice-remove 'undo-tree-make-history-save-file-name #'sl-undo-tree-make-history-save-file-name)

(provide '50undo-tree)
;;; 50undo-tree ends here
