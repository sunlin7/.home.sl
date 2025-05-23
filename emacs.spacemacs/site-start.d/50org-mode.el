;;; 50org-mode --- org settings      -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:
;; Various preferences

(defmacro org-publish-dir() "Concat pub dir."
          (list 'expand-file-name "publish" 'org-directory))

(with-eval-after-load 'org
  (custom-set-variables
   '(org-archive-reversed-order t)
   '(org-reverse-note-order t)
   '(org-directory (expand-file-name "org/" sl-savefile-dir))
   '(org-default-notes-file (expand-file-name "note.org" org-directory))
   '(org-fast-tag-selection-single-key 'expert)
   '(org-tags-column 80)
   ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
   '(org-todo-keywords
     '((sequence "TODO(t)" "DOING(i)" "PENDING(p)" "WAIT(w)" "|" "DONE(d)" "CANCEL(l)")
       (sequence "OPEN(o)" "|" "CLOSED(c)")
       (type "NOTE")))
   '(org-todo-keyword-faces '(("NOTE" . '(:foreground "#875f00"))))
   '(org-src-fontify-natively t)
   '(org-agenda-files '("personal.org"  ; file path relate to org-directory
                        "todo.org"
                        "memory.org"
                        "note.org"))
   '(org-agenda-start-on-weekday nil)
   '(org-agenda-span 14)
   '(org-agenda-include-diary t)
   '(org-agenda-window-setup 'current-window)
   ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
   '(org-refile-targets  '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
   ;; Targets start with the file name - allows creating level 1 tasks
   '(org-refile-use-outline-path 'file)
   '(org-capture-templates
     '(("t" "Todo" entry (file+headline "todo.org" "Tasks") ; file path relate to org-directory
        "* TODO %?\n  %i\n  %a" :prepend t)
       ("n" "Note" entry (file+headline "note.org" "Note")
        "* NOTE %?\n  %i\n  %a" :prepend t)
       ("j" "Journal" entry (file+olp+datetree "journal.org")
        "* %?\nEntered on %U\n  %i\n  %a" :prepend t)))
   '(org-confirm-babel-evaluate nil)
   ;; '(org-babel-load-languages
   ;;   '((shell . t)
   ;;     (js . t)
   ;;     (emacs-lisp .t)
   ;;     (python . t)))
   '(org-clock-persist t)
   '(org-clock-in-resume t)
   ;; Change task state to STARTED when clocking in
   '(org-clock-in-switch-to-state "STARTED")
   ;; Save clock data and notes in the LOGBOOK drawer
   '(org-clock-into-drawer t)
   ;; Removes clocked tasks with 0:00 duration
   '(org-clock-out-remove-zero-time-clocks t)
   '(org-clock-persist-file (expand-file-name "org-clock-save.el" sl-savefile-dir))
   ;; for publish
   '(org-publish-project-alist
     '(("orgfiles"
        :base-directory org-directory
        :base-extension "org"
        :publishing-directory (org-publish-dir)
        :publishing-function org-publish-org-to-html
        :exclude "private\\|person.*"   ;; regexp
        :headline-levels 3
        :section-numbers nil
        :table-of-contents nil
        :style "<link rel=\"stylesheet\"
                       href=\"../other/mystyle.css\" type=\"text/css\"/>"
        :html-preamble t)

       ("source"
        :base-directory (concat org-directory "/source")
        :base-extension "c\\|cpp"
        :publishing-directory (concat (org-publish-dir) "/source")
        :publishing-function org-publish-org-to-org
        :htmlized-source t
        )

       ("images"
        :base-directory (concat org-directory "/images")
        :base-extension "jpg\\|gif\\|png"
        :publishing-directory (concat (org-publish-dir) "/images")
        :publishing-function org-publish-attachment)

       ("other"
        :base-directory (concat org-directory "/other/")
        :base-extension "css\\|el"
        :publishing-directory (concat (org-publish-dir) "/other/")
        :publishing-function org-publish-attachment)
       ("website" :components ("orgfiles" "images" "other")))))
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-persistence-insinuate))

(with-eval-after-load 'ox-html
  (declare-function org-html--format-image "ox-html")
  (declare-function org-html-export-to-html "ox-html")
  (declare-function org-html-close-tag "ox-html")
  (declare-function org-combine-plists "org-macs")
  (declare-function org-html--make-attribute-string "ox-html")
  (declare-function org-export-define-derived-backend "ox")
  ;; https://niklasfasching.de/posts/org-html-export-inline-images/
  (defun org-html-export-to-mhtml (&optional async subtree visible body)
    (cl-letf (((symbol-function 'org-html--format-image) 'format-image-inline))
      (org-html-export-to-html async subtree visible body)))

  (defun format-image-inline (source attributes info)
    (let* ((ext (file-name-extension source))
           (prefix (if (string= "svg" ext) "data:image/svg+xml;base64," "data:;base64,"))
           (data (with-temp-buffer
                   (if (file-exists-p source)
                       (insert-file-contents source)
                     (url-insert-file-contents source))
                   (buffer-string)))
           (data-url (concat prefix (base64-encode-string data)))
           (attributes (org-combine-plists `(:src ,data-url) attributes)))
      (org-html-close-tag "img" (org-html--make-attribute-string attributes) info)))
  (org-export-define-derived-backend
      'html-inline-images 'html
    :menu-entry '(?h "Export to HTML" ((?m "As MHTML file and open" org-html-export-to-mhtml)))))

(defvar org-directory)
(defun gtd ()
  "The gtd function."
  (interactive)
  (defvar org-agenda-files)
  (let ((default-directory org-directory))
    (mapc #'find-file org-agenda-files)))

(provide '50org-mode)
;;; 50org-mode.el ends here
