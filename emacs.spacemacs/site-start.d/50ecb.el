;;; 50ecb --- ecb settings
;;; Commentary:
;;; Code:

(defvar sl-packages-list)
(add-to-list 'sl-packages-list 'ecb)

(defvar ecb-dir-path
  (if (fboundp 'ecb-activate)
      (file-name-directory (find-lisp-object-file-name 'ecb-activate (symbol-function 'ecb-activate)))
    (concat (locate-user-emacs-file "ecb") "/"))
  "The ecb directory.")

(custom-set-variables
 '(ecb-layout-name "left9")
 '(ecb-source-path '(("~" "~"))) ; the `ecb-source-path' and `ecb-options-version'
 '(ecb-options-version "2.50")  ; will prevent the ecb-first-time page
 '(ecb-windows-width 0.22)      ; for most 16:10 or 16:9 screen
 '(ecb-help-html-path (concat ecb-dir-path "ecb.html" )) ; avoid the ecb first run tips
 ;; the path maybe not exist, not `expand-file-name'
 '(ecb-help-info-path (concat ecb-dir-path "ecb.info"))
 '(ecb-tip-of-the-day nil))

(eval-after-load "ecb-autoloads.el"
  '(progn
     ;; ecb features
     (setq ecb-dir-path (file-name-directory (find-lisp-object-file-name
                                              'ecb-activate (symbol-function 'ecb-activate))))
     (global-set-key (kbd "C-c . l w") 'ecb-activate)
     (add-hook 'c-mode-common-hook        ; re-map "C-c ." keyboard
               (lambda ()
                 (local-unset-key (kbd "C-c ."))
                 (local-set-key (kbd "C-c , .") 'c-set-style)))))

(eval-after-load 'ecb
  '(progn
     (defvar ecb-mode-map)
     (declare-function ecb-disable-advices "ecb-common-browser")
     (add-hook 'ecb-deactivate-hook
               (lambda () (ecb-disable-advices 'ecb-winman-not-supported-function-advices t)))
     (add-hook 'ecb-activate-hook
               (lambda () (define-key ecb-mode-map (kbd "C-c . l w") 'ecb-deactivate)))))

(eval-after-load 'ecb-layout
  '(with-no-warnings
     (require 'ecb-util)
     (ecb-layout-define "sl-leftright-analyse" left-right
       "This function creates the following layout:

   --------------------------------------------------------------
   |             |                               |              |
   |  Methods    |                               |  Directories |
   |             |                               |              |
   |             |                               |              |
   |             |                               |              |
   |             |                               |              |
   |             |                               |              |
   |             |             Edit              |--------------|
   |             |                               |              |
   |             |                               |  Sources     |
   |             |                               |              |
   |-------------|                               |--------------|
   |             |                               |              |
   |  Analyse    |                               |  History     |
   |             |                               |              |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
       (ecb-set-methods-buffer)
       (ecb-split-ver 0.7)
       (ecb-set-analyse-buffer)
       (select-window (next-window (next-window)))
       (ecb-set-directories-buffer)
       (ecb-split-ver 0.4)
       (ecb-set-sources-buffer)
       (ecb-split-ver 0.5)
       (ecb-set-history-buffer)
       (select-window (previous-window (previous-window (selected-window) 0) 0)))

     (ecb-layout-define "sl-left-analyse" left
       "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   |              |                 Edit                 |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |              |                                      |
   |  Analyse     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
       (ecb-set-methods-buffer)
       (ecb-split-ver 0.7)
       (ecb-set-analyse-buffer)
       (select-window (next-window)))))

;;(ecb-activate)
(provide '50ecb)
;;; 50ecb ends here
