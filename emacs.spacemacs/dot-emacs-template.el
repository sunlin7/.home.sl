;;; dot-emacs-template --- a template for .emacs -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; It's a good start for custom dot-emacs file.
;; And Emacs 28.1+ is required.
;;; Code:

(when (version<= emacs-version "28")
  (require 'subr-x))                       ; for when-let*/if-let*

(defvar portable-root-dir (file-name-parent-directory invocation-directory))
(defvar portable-home-dir
  (if (and (not (fboundp 'image-mask-p)) ; for noX build try .root[im]-*/.emacs
           (file-exists-p (expand-file-name ".emacs" portable-root-dir)))
      portable-root-dir
    (file-name-directory (or (buffer-file-name) load-file-name))))

;; async-compile will invoke "emacs --batch -l /tmp/xxx.el", then the libgccjit
;; will search the crtbegin*.o, change native-comp-driver-options to help
;; libgccjit to locate the essential files.
(when (native-comp-available-p)
  (custom-set-variables
   '(native-comp-async-jobs-number (1- (num-processors)))
   '(native-comp-async-env-modifier-form  ; driver or compiler options
     `(setq native-comp-driver-options '(,(concat "-B" (expand-file-name "../lib/" invocation-directory)))))))

(defvar sl-savefile-dir (if-let* ((save-dir (expand-file-name ".emacs.save/" (or (getenv "OHOME") (getenv "HOME"))))
                                  (_ (file-exists-p save-dir)))
                            save-dir
                          user-emacs-directory))

(load (expand-file-name ".home.sl/emacs.spacemacs/init" portable-home-dir))

;; assume that spacemacs was installed.
(when-let* ((spacemacs-dir (expand-file-name ".emacs.spacemacs/" portable-home-dir))
            (sl-spacemacs-init (locate-file "init" (list spacemacs-dir) load-suffixes)))

  (setq sl-packages-excluded
        '(chinese-conv
          evil-tutor
          fancy-battery
          gtags
          multi-term
          pangu-spacing
          dactyl-mode
          rainbow-delimiters
          tern
          tide
          vi-tilde-fringe
          xcscope))
  (setq sl-configuration-layers
        '(auto-completion
          better-defaults
          (emacs-lisp :variables emacs-lisp-format-on-save nil)
          helm
          ibuffer
          imenu-list
          (lua :variables
               lua-indent-level 4
               lua-lsp-server 'lua-language-server) ; if lsp enabled
          markdown
          nginx
          yaml
          shell
          vimscript))
  (pcase system-type
    ('windows-nt
     (nconc sl-configuration-layers
            '(csv
              git
              ietf
              (lsp :variables lsp-restart 'ignore)
              (multiple-cursors :variables multiple-cursors-backend 'mc)
              org
              (python :variables python-enable-tools '(pipenv))
              sql
              version-control
              windows-scripts))
     (setq exec-path ; remove Python-App from path to avoid issues for Emacs-Win32
           (cl-delete-if
            (apply-partially 'string-match-p "AppData/Local/Programs/Python")
            exec-path)))

    ((guard (or (fboundp 'image-mask-p) (native-comp-available-p))) ; not minimal
     (nconc sl-configuration-layers
            '((c-c++ :variables
                     ;; c-c++-enable-google-style t
                     ;; c-c++-enable-google-newline t
                     c-c++-backend 'lsp-clangd)
              (chinese :variables chinese-default-input-method t
                       chinese-enable-avy-pinyin nil)
              cmake
              csv
              django
              epub
              eww
              git
              imenu-list
              ietf
              (lsp :variables
                   lsp-restart 'ignore
                   ;; lsp-semantic-tokens-enable t
                   lsp-clients-lua-language-server-install-dir (expand-file-name "share/lua-language-server" portable-root-dir)
                   ;; lsp-lua-runtime-path ["?.lua" "?/init.lua" "?/?.lua" "../?/?.lua"]
                   lsp-lua-workspace-preload-file-size 500
                   lsp-copilot-applicable-fn nil)
              (multiple-cursors :variables multiple-cursors-backend 'mc)
              octave
              (org :variables
                   org-plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))
              (plantuml :variables plantuml-default-exec-mode 'jar
                        plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))
              (python :variables python-enable-tools '(pipenv))
              rust
              (shell :variables shell-default-shell 'multi-vterm)
              shell-scripts
              syntax-checking
              systemd
              (sql :variables sql-capitalize-keywords t
                   sql-capitalize-keywords-blacklist '("name" "varchar"))
              version-control
              yaml))
     (setq sl-packages-list (append sl-packages-list '(mermaid-mode math-preview)))
     (add-hook 'kill-buffer-hook
               #'(lambda ()
                   (when-let* (((boundp 'mermaid-tmp-dir))
                               (prefix (concat mermaid-tmp-dir "current-buffer"))
                               ((string-prefix-p prefix (buffer-file-name))))
                     (dolist (x (file-expand-wildcards (concat prefix "*")))
                       (delete-file x t)))))
     (delq 'shell sl-configuration-layers) ;delete the no-argument `shell' layer
     (when (fboundp 'image-mask-p)
       (add-to-list 'sl-packages-list 'org-pdftools)
       (nconc sl-configuration-layers
              '(graphviz
                pdf))
       (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)
       (custom-set-variables '(pdf-view-restore-filename (locate-user-emacs-file ".cache/pdf-view-restore"))))
     (when-let* ((default-directory portable-home-dir)
                 (paths (file-expand-wildcards ".local/LanguageTool*/languagetool-commandline.jar" t)))
       (add-to-list 'sl-configuration-layers `(languagetool :variables langtool-language-tool-jar ,(car paths))))
     (when (and (not (equal portable-home-dir portable-root-dir))
                (file-exists-p (expand-file-name ".wl" portable-home-dir)))
       (add-to-list 'sl-packages-list 'wanderlust)))

    (_ ;; terminal without X11, a minimum config
     (nconc sl-packages-excluded '(pdf-tools
                                   org-pdftools
                                   yasnippet
                                   yasnippet-snippets))
     (nconc sl-configuration-layers '())
     (with-eval-after-load "files" (delete '("\\.org\\'" . org-mode) auto-mode-alist))))

  (define-advice recentf-load-list (:around (ofun &rest args) ADV)
    (let ((file-name-handler-alist
           (cl-remove-if (lambda (x) (string-prefix-p "tramp-" (symbol-name (cdr x))))
                         file-name-handler-alist)))
      (apply ofun args)))

  (with-eval-after-load 'core-configuration-layer
    (setq configuration-layer--elpa-root-directory (concat portable-home-dir ".emacs.d/elpa")))
  (define-advice dotspacemacs/layers (:after ())
    (setq-default dotspacemacs-configuration-layers sl-configuration-layers
                  dotspacemacs-additional-packages sl-packages-list
                  dotspacemacs-excluded-packages sl-packages-excluded
									dotspacemacs-enable-lazy-installation nil))
  (define-advice dotspacemacs/init (:after ())
    (setq-default dotspacemacs-editing-style 'hybrid
                  dotspacemacs-enable-load-hints t
                  dotspacemacs-loading-progress-bar nil
                  dotspacemacs-maximized-at-startup nil
                  dotspacemacs-line-numbers '(:disabled-for-modes org-mode)))
  ;; load the spacemacs
  (load-file sl-spacemacs-init)
  (when-let* ((OHOME (getenv "OHOME")))
    ;; set `default-directory' to full path to avoid "~/bin" is invalid after change HOME
    (setq default-directory (file-truename default-directory))
    (setenv "HOME" OHOME)
    (setenv "OHOME" nil))
  (setq dotspacemacs-frame-title-format "%b@%S")
  ;; post-config for spacemacs
  (when (fboundp 'pyim-activate)
    (custom-set-variables '(pyim-default-scheme 'wubi))
    (define-advice pyim-activate (:before (&optional _) mydicts)
      (advice-remove 'pyim-activate 'pyim-activate@mydicts)
      (dolist (x '("share/pyim-wbdict-rime.rime")) ;"share/pyim-wbdict-v86.pyim"
        (pyim-extra-dicts-add-dict
         `(:name ,(file-name-base x) :file ,(expand-file-name x portable-root-dir))))))

  (add-to-list 'after-init-hook 'menu-bar-mode t) ;; glitch
  (when (string-match "X11" system-configuration-features)
    (use-package org-pdftools ; make sure the function org-pdftools-setup-link exists
      :defer t :after org :config (org-pdftools-setup-link)))
  ;; (or (file-exists-p plantuml-jar-path) (plantuml-download-jar)); download plantuml.jar
  (with-eval-after-load 'plantuml-mode
    (when (fboundp 'image-mask-p)
      (plantuml-set-output-type "png"))) ; default SVG is hard to see under dark theme

  ;; disable img resize for window size is changed by HELM windows
  (custom-set-variables '(image-auto-resize-on-window-resize nil)
                        '(image-auto-resize ; resize image for HiDPI
                          (if-let* ((scale (getenv "GDK_SCALE")))
                              (string-to-number scale)
                            t)))

  (with-eval-after-load 'helm-files
    (setq helm-ff-use-notify (and file-notify--library t)))

  ;;;; fix the c-basic-offset for google-c-style
  ;; (with-eval-after-load 'google-c-style
  ;;   '(dolist (v google-c-style)
  ;;     (when (and (listp v) (eq (car v) 'c-basic-offset))
  ;;       (setcdr v 4))))
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda () (setq-local tab-width c-basic-offset)))

  ;; (when (daemonp)
  ;;   (with-temp-buffer (helm-mode)) ;; preload heavy packages
  ;;   (with-temp-buffer (org-mode)))

  (with-eval-after-load 'multi-term
    (nconc term-bind-key-alist '(("<M-backspace>" . term-send-backward-kill-word))))

  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "C-y") 'vterm--self-insert)
    (define-key vterm-mode-map (kbd "C-S-<insert>")
                #'(lambda () "`kill-ring-save' then `yank'"
                    (interactive)
                    (call-interactively 'kill-ring-save)
                    (call-interactively 'vterm-yank))))

  (let ((template-file (or (buffer-file-name) load-file-name)))
    (defun open-dot-emacs-template ()
      (interactive)
      (find-file-existing template-file))
    (spacemacs/set-leader-keys
      "fet" #'open-dot-emacs-template)) ; function NAME displayed on transaction menu
  )

(defun sl-term-kdb-patch (frame)
  "Update key binding in terminal for FRAME, `$showkey -a` for key sequence."
  (when (terminal-live-p (frame-terminal frame))
    (with-selected-frame frame
      (define-key input-decode-map "[;5~" [C-backspace])
      (define-key input-decode-map "[;6~" [C-S-backspace])
      (define-key input-decode-map "[1;8u" [?\C-\M-%]))))

(add-hook 'after-make-frame-functions #'sl-term-kdb-patch)
(sl-term-kdb-patch (selected-frame)) ; patch 'after-make-frame-functions for the initialed term

;; (add-to-list 'after-init-hook (apply-partially 'xterm-mouse-mode 0))
(add-to-list 'after-init-hook #'global-hungry-delete-mode)

;; dired-quick-sort integration with dired.
;; https://github.com/syl20bnr/spacemacs/pull/16814
(define-advice dired-sort-toggle (:before ())
  "Recover `dired-actual-switches' with `dired-listing-switches' when the long
option \"--sort=...\" exists, and convert \"--sort=time\" to \"-t\"."
  (when-let* (((string-match-p "--sort=" dired-actual-switches))
              (switches dired-listing-switches))
    ;; ignore "-t" option first, determines it from actually switches later
    (when (string-match "\\(\\`\\| \\)-\\([^t]*\\)\\(t\\)\\([^ ]*\\)"
                        switches)
      (let ((alone (and (equal (match-string 2 switches) "")
                        (equal (match-string 4 switches) ""))))
        ;; the 2nd and 4th are empty indicate it's standalone "-t"
        ;; otherwise the option is in the "-XtY" format
        (setq switches (replace-match "" t t switches
                                      (unless alone 3)))))
    ;; determines the "--sort=time" option and converts it to "-t" now
    (let ((sort-time (string-match-p "--sort=time" dired-actual-switches)))
      (setq dired-actual-switches
            (concat switches (when sort-time " -t"))))))

;; Better copying file path
;; https://github.com/syl20bnr/spacemacs/pull/16883
(define-advice spacemacs/copy-file-path (:around (ofun &rest args))
  (cond ((derived-mode-p 'dired-mode)
         (let ((path (or (dired-get-filename nil t)
                         (spacemacs--directory-path))))
           (kill-new path)
           (message "%s" path)))
        ((derived-mode-p 'eww)
         (eww-copy-page-url))
        (t (apply ofun args))))
;; (custom-set-variables
;; (window-system-default-frame-alist
;;  '((x . ((left . 0) (top . 50) (width . 190) (height . 50))) ;NOTE: get values by (frame-position), (frame-height), (frame-width)
;;    (w32 . ((left . (+ -10)) (top . 1) (width . 190) (height . 52)))))

;; (when-let (my-project-init (locate-file "projects" (list sl-savefile-dir)))
;;   (add-hook 'after-init-hook (lambda () (load-file my-project-init)))
;;   (defun sl-ede-project-reload ()
;;     (interactive)
;;     (setq ede-cpp-root-project-list nil)
;;     (load-file my-project-init)))

;;; dot-emacs-template.el ends here
