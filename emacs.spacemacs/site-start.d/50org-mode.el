;;; 50org-mode --- org settings
;;; Commentary:
;;; Code:
;; -*- coding: utf-8 -*-
;; Various preferences
(custom-set-variables
 '(org-directory (expand-file-name "org/" sl-savefile-dir))
 '(org-agenda-files
   (mapcar (lambda (str) (expand-file-name str org-directory))
           '("personal.org"
             "action.org"
             "memory.org"
             "sched.org"
             "todo.org"
             "note.org"
             "wait.org")))
 '(org-default-notes-file (expand-file-name "note.org" org-directory))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-span 14)
 '(org-agenda-include-diary t)
 '(org-agenda-window-setup 'current-window)
 '(org-fast-tag-selection-single-key 'expert)
 '(org-tags-column 80)
 ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
 '(org-refile-targets  '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
 ;; Targets start with the file name - allows creating level 1 tasks
 '(org-refile-use-outline-path 'file)
 ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
 '(org-todo-keywords
   '((sequence "TODO(t)" "DOING(i)" "PENDING(p)" "WAIT(w)" "|" "DONE(d)" "CANCEL(l)")
     (sequence "OPEN(o)" "|" "CLOSED(c)")))
 '(org-capture-templates
   '(("t" "Todo" entry (file+headline "todo.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Note" entry (file+headline "note.org" "Note")
      "* NOTE %?\n  %i\n  %a")
     ("j" "Journal" entry (file+datetree "journal.org")
      "* %?\nEntered on %U\n  %i\n  %a")))
 '(org-confirm-babel-evaluate nil)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
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
 '(org-clock-persist-file (expand-file-name "org-clock-save.el" sl-savefile-dir)))

(defmacro org-publish-dir() (list 'expand-file-name "publish" 'org-directory))

(custom-set-variables
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

(eval-after-load 'org
  '(progn
     ;; in a future version of org, use if to avoid errors
     ;; Change .pdf association directly within the alist
     (defvar org-file-apps)
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")
     ;; Save the running clock and all clock history when exiting Emacs, load it on startup
     (org-clock-persistence-insinuate)))

(eval-after-load 'ox-html
  '(progn
     (eval-when-compile (require 'ox-html))
     ;; https://niklasfasching.de/posts/org-html-export-inline-images/
     (defun org-html-export-to-mhtml (async subtree visible body)
       (cl-letf (((symbol-function 'org-html--format-image) 'format-image-inline))
         (org-html-export-to-html nil subtree visible body)))

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
      :menu-entry '(?h "Export to HTML" ((?m "As MHTML file and open" org-html-export-to-mhtml))))))

(defun gtd ()
  "The gtd function."
  (interactive)
  (require 'org)
  (defvar org-agenda-files)
  (dolist (file org-agenda-files)
    (find-file file)))

(provide '50org-mode)
;;; 50org-mode ends here
