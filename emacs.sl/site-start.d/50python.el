;;; 50python.el -- setup for python
;;; Commentary:
;;; Code:
;; send current line to *Python
(autoload 'cl-every "cl")
(autoload 'project-roots "project")
(autoload 'python-shell-send-region "python")

(defvar sl-last-python-shell-pythonpaths nil "The PYTHONPATH variable.")

(defvar python-shell-extra-pythonpaths)
(defun sl-adv-python-anaconda-mode-need-restart (ret)
  "Advice the anaconda-mode-need-restart function for PYTHONPATH changed.
RET is the original return value."
  (let ((ret2 (and sl-last-python-shell-pythonpaths ; not nil
                   (not (cl-every 'string= sl-last-python-shell-pythonpaths python-shell-extra-pythonpaths)))))
    (when (or ret2 (not sl-last-python-shell-pythonpaths))
      (setq sl-last-python-shell-pythonpaths (copy-tree python-shell-extra-pythonpaths)))
    (or ret ret2)))

(defun sl-python-send-region (&optional beg end)
  "Support send current line.
BEG for begine,
END for end."
  (interactive)
  (let ((beg (cond (beg beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (line-beginning-position))))
        (end (cond (end end)
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (line-end-position)))))
    (python-shell-send-region beg end)))

(eval-after-load 'python
  '(progn
     (defvar python-mode-map)
     (define-key python-mode-map [(control return)] 'sl-python-send-region)
     (define-key python-mode-map [menu-bar Python sl-send-line]
       '(menu-item "Send line/region" sl-python-send-region
                   :help "Send current line or region."))

     (advice-add 'anaconda-mode-need-restart :filter-return #'sl-adv-python-anaconda-mode-need-restart)))

;; add project dir to pythonpaths that python-mode/anaconda-mode will use it.
(add-hook 'python-mode-hook
          (lambda ()
            (when (buffer-file-name)
              (add-to-list (make-local-variable 'python-shell-extra-pythonpaths)
                           (file-name-directory (buffer-file-name))))

            (when (project-current)
              (let* ((roots (project-roots (project-current)))
                     (vpython (or
                               (car (file-expand-wildcards (expand-file-name "*/bin/python" (car roots)) t))
                               (car (file-expand-wildcards (expand-file-name "*/bin/python3" (car roots)) t)))))
                (setq-local python-shell-extra-pythonpaths
                            (cl-remove-duplicates
                             (append python-shell-extra-pythonpaths
                                     (mapcar 'file-truename roots))))
                (when vpython
                  (let ((vroot (expand-file-name "../.." vpython)))
                    (defvar python-shell-virtualenv-root)
                    (setq-local python-shell-virtualenv-root vroot)

                    (defvar python-shell-interpreter)
                    (when (and (string= (default-value 'python-shell-interpreter)
                                        python-shell-interpreter) ; didn't manually changed
                               (file-exists-p vpython)
                               (not (string= vpython python-shell-interpreter))) ; it's a new value
                      (setq sl-last-python-shell-pythonpaths
                            (append sl-last-python-shell-pythonpaths
                                    '("not-exist-path-make-anoconda-mode-restart"))))))))))

(provide '50python)
;;; 50python ends here
