;;; project-config --- project settings.
;;; Commentary:
;;; Code:

(defvar sl-ede-project-file-basename ".sl-ede-project" "The base name for project file.")
(defvar sl-ede-project-file (concat sl-ede-project-file-basename ".el") "The file for project.")
(defvar sl-ede-project-list-all (concat sl-ede-project-file-basename ".all") "The files list for project.")
(defvar sl-ede-project-xtags (concat sl-ede-project-file-basename ".xtags") "The files list for project.")
(autoload 'oref "eieio")
(autoload 'object-of-class-p "eieio")
(autoload 'ede-name "ede/base")
(autoload 'ede-current-project "ede/cpp-root")
(autoload 'ede-project-root-directory "ede/auto")
(autoload 'hash-table-keys "subr-x")
(eieio-defclass-autoload 'sl-ede-cpp-root-project '(ede-cpp-root-project)
                         "sl-cpp-root"
                         "SL EDE cpp-root project class,
Each directory needs a project file to control it.")

(defun sl-ede-read-file-lines (filename)
  "Return the list of lines of a file in `filename', FILENAME should no be nil."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(defun sl-get-uniq-path (path-list)
  "Get the unique path for the PATH-LIST."
  (let* ((path-common nil)
         (path-seq-map ; assoc path with it's split, '(("/a/b" . '(a b)) ...)
          (cl-mapcar (lambda (str)
                     (let ((str-sp (split-string str "/" str)))
                       (setq path-common
                             (if path-common
                                 (cl-nintersection path-common str-sp :test 'equal)
                               str-sp))
                       (cons str str-sp)))
                   path-list)))
    (cl-mapcar (lambda (item)
               (cons (mapconcat 'identity
                                (cl-set-difference (cdr item) path-common :test 'equal) "/")
                     (car item)))
             path-seq-map)))

(defun sl-ede-uniq-names (names-hash)
  "Generate unique names from NAMES-HASH."
  ;; for each item in name-table, generate unique name and store to name-uniq
  (let ((name-uniq  (make-hash-table :test 'equal)))
    (maphash (lambda (key val)
               (if (> (length val) 1)
                   (mapc (lambda (v)
                           (puthash (concat key "<" (car v) ">") (cdr v) name-uniq))
                         (sl-get-uniq-path val))
                 (puthash key (car val) name-uniq)))
             names-hash)
    name-uniq))

(defun sl-ede-project-files-hash ()
  "Return the files in project."
  (require 'ede)
  (let ((cur-proj (ede-current-project)))
    (when (not cur-proj)
      (signal 'error '("ede-project not exist!")))
    (let* ((project-root (ede-project-root-directory cur-proj))
           (files-list   (expand-file-name sl-ede-project-list-all project-root))
           (files-el     (expand-file-name sl-ede-project-file project-root)))
      (when (not (file-exists-p files-list))
        (signal 'file-error '("tags file not exist!")))
      (when (file-newer-than-file-p files-list files-el)
        (with-temp-buffer
          (let ((lines (sl-ede-read-file-lines files-list))
                (name-table (make-hash-table :test 'equal)))
            ;; store lines with it's basename as key
            (mapc (lambda (full-path)
                    (let* ((basename (file-name-nondirectory (directory-file-name full-path)))
                           (old (gethash basename name-table)))
                      (if old
                          (add-to-list 'old full-path)
                        (setq old (list full-path)))
                      (puthash basename old name-table)))
                  lines)
            ;; put to the buffer
            (print (sl-ede-uniq-names name-table) (current-buffer)))

          (condition-case nil
              (write-region (point-min) (point-max) files-el)
            (file-error (message "Can't write %s" files-el)))))
      (with-temp-buffer
        (insert-file-contents files-el)
        (read (current-buffer))))))

(defun sl-ede-find-file ()
  "Find the file from `ede-cpp-root-project."
  (interactive)
  (let* ((name-uniq (sl-ede-project-files-hash))
         (choice
          (completing-read "Select file: " (hash-table-keys name-uniq) nil t)))
    (let ((default-directory (ede-project-root-directory (ede-current-project))))
      (find-file (gethash choice name-uniq)))))

(defun sl-ede-workspace-switch ()
  "Switch the workspace for `sl-ede-cpp-project'."
  (interactive)
  (require 'ede/cpp-root)
  (defvar ede-projects)
  (let ((name-table (make-hash-table :test 'equal))
        name-uniq)
    ;; orgnize the projects via project name and root-directory into name-table
    (mapc (lambda (proj)
            (let* ((full-path (ede-project-root-directory proj))
                   (basename (if (string= (ede-name proj) "Untitled")
                                 (file-name-nondirectory (directory-file-name full-path))
                               (ede-name proj)))
                   (old (gethash basename name-table)))
              (if old
                  (add-to-list 'old full-path)
                (setq old (list full-path)))
              (puthash basename old name-table)))
          ede-projects)
    (setq name-uniq (sl-ede-uniq-names name-table))
    ;; try to select it
    (let ((choice
           (completing-read
            "Select workspace:"
            (hash-table-keys name-uniq) nil t)))
      (find-file (gethash choice name-uniq)))))

(eval-after-load "ede"
  '(progn
     (define-key ede-minor-mode-map (kbd "C-c . j") 'sl-ede-find-file)
     (define-key ede-minor-mode-map (kbd "C-c . w") 'sl-ede-workspace-switch)))

(eval-after-load "projectile" ; FIXME: workaround for projectile extremly slow on NFS
  '(progn
     (defvar projectile-project-root-files)
     (add-to-list 'projectile-project-root-files sl-ede-project-file)))

(autoload 'rgrep-default-command "grep")
(defun sl-ede-tags-create()
  "Create the cscope & gnu global tags from `ede-cpp-root-project' settings.
NOTICE: the \"/\" in :include-path and :source-path mean the project root
directory, it will be ignored for optimizing tags file, use \"/.\" if you
want add all files in project to tags file."
  (interactive)
  (require 'ede)
  (require 'grep)
  (let ((cur-proj (ede-current-project)))
    (when (not cur-proj)
      (signal 'error '("Invalid project")))

    (let* ((project-root (ede-project-root-directory cur-proj))
           (proj-name-tmp  (concat sl-ede-project-file-basename ".tmp"));;tags-file-tmp (shell-command-to-string "mktemp")
           (rpath-all (mapconcat
                      (lambda (prj-inc)
                        (when (string-prefix-p "/" prj-inc) ; drop the "/", then append to path
                          (substring prj-inc 1)))
                      (append (oref cur-proj :include-path)
                              (when (object-of-class-p cur-proj 'sl-ede-cpp-root-project)
                                (oref cur-proj :source-path)))
                      " ")))
      ;; (setq rpath-all (concat rpath-all " " (mapconcat 'identity (oref (ede-current-project) :system-include-path) " "))) ;; use GTAGSLIBPATH instead
      (defvar grep-find-ignored-directories)
      (defvar grep-find-ignored-files)
      (let ((grep-find-ignored-directories ;; prepare the 'exclude-path'
             (append grep-find-ignored-directories
                     (when (object-of-class-p cur-proj 'sl-ede-cpp-root-project)
                       (oref cur-proj :exclude-path)) ))
            (grep-find-ignored-files grep-find-ignored-files))
        (grep-compute-defaults)
        (let ((grep-find-template (format "find <D> <X> -type f <F>  -print > %s" proj-name-tmp)))
          (let ((default-directory project-root)
                (str-command (rgrep-default-command "" "* .*" rpath-all))) ; cd to project-root
            ;; use find | sort | (cscope && global) step by step for esase debug
            (shell-command-to-string str-command)
            (shell-command-to-string (format "sort -u %s > %s" proj-name-tmp sl-ede-project-list-all))
            (shell-command-to-string (format "grep -E '\\.[cChH](pp)?$|\\.cc$|\\.hh$' %s > %s" sl-ede-project-list-all sl-ede-project-xtags))
            ;; (call-process-shell-command (concat "time cscope -b -f cscope.out -i " sl-ede-project-xtags) nil 0) 
            (setenv "GTAGSLIBPATH" (mapconcat 'identity (oref (ede-current-project) :system-include-path) ":"))
            (call-process-shell-command (concat "time gtags -f " sl-ede-project-xtags) nil 0)
            (delete-file proj-name-tmp)))))))

(defvar sl-ede-project-reload-hook nil "Hook called for reload project.")

(defun sl-ede-project-reload()
  "Load the projects."
  (interactive)
  (defvar ede-cpp-root-project-list)
  (setq ede-cpp-root-project-list
        (cl-remove-if-not
         (lambda (proj) (file-exists-p (ede-project-root-directory proj)))
         ede-cpp-root-project-list))
  (run-hooks 'sl-ede-project-reload-hook))

(eval-after-load "ede"
  '(add-hook 'after-init-hook 'sl-ede-project-reload))

(defun sl-compilation-start (OLDFUN command &optional mode name-function highlight-regexp)
  "Create tag with sl-ede-project-xtags if it avaliable.
OLDFUN is the original function, COMMAND MODE NAME-FUNCTION HIGHLIGHT-REGEXP are bypass"
  (let* ((project-root (and (string= command "global -u")
                            (ggtags-process-string "global" "-p")))
         (project-xtags (expand-file-name sl-ede-project-xtags project-root)))
    (if (and project-root
             (file-exists-p project-xtags))
        (let ((default-directory project-root))
          (apply OLDFUN (concat "gtags -i -f " sl-ede-project-xtags) mode name-function highlight-regexp))
      (apply OLDFUN command mode name-function highlight-regexp))))

(advice-add 'compilation-start :around #'sl-compilation-start)


;;; A sample for ede-cpp-root-project
;; (add-hook 'sl-ede-project-reload-hook
;;  (lambda ()
;;    (when (file-exists-p "~/project/ede-test-prj-simple/README")
;;      (ede-cpp-root-project "ede-test"
;;                            :name "ede-test-prj-simple"
;;                            :file "~/project/ede-test-prj-simple/README"))))

;;; A template for projects which have same structure
;; (defvar sl-prj-inc '("/include" "/inc") "Projects include paths.")
;; (defvar sl-sys-inc '("/usr/include" "/usr/inc") "System include paths.")
;; (defvar sl-spp-table '(("CONST" . "const")) "C Preprocessor macros for project.")
;; (defun sl-make-ede-project (project-root)
;;   "Make ede projects.  PROJECT-ROOT is the root of project.."
;;   (when (file-exists-p project-root)
;;     (let* ((project-name (file-name-base project-root))
;;            (project-file (expand-file-name "cscope.out" project-root)))
;;       (shell-command (concat "touch " (shell-quote-argument project-file)))
;;       (when (file-exists-p project-file)
;;         (ede-cpp-root-project project-name
;;                               :name project-name
;;                               :file project-file
;;                               :include-path sl-prj-inc
;;                               :system-include-path sl-sys-inc
;;                               :spp-table sl-spp-table)))))
;;
;; (add-hook 'sl-ede-project-reload-hook
;;  (lambda ()
;;    (mapc 'make-ede-project
;;          (split-string (shell-command-to-string "find ~/template-project -type d -maxdepth 1 2>/dev/null")))))

(provide '50project)
;;; 50project.el ends here
