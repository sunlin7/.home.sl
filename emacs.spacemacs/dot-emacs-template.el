;;; dot-emacs-template --- a template for .emacs -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; It's a good start for custom dot-emacs file.
;; And Emacs 28.1+ is required.
;;; Code:

(autoload 'if-let* "subr-x")
(autoload 'when-let* "subr-x")

(defvar portable-root-dir (file-name-parent-directory invocation-directory))
(defvar portable-home-dir
  (if (and (not (fboundp 'image-mask-p)) ; for noX build try .root[im]-*/.emacs
           (file-exists-p (expand-file-name ".emacs" portable-root-dir)))
      portable-root-dir
    (file-name-directory (or load-file-name (buffer-file-name)))))

;; async-compile will invoke "emacs --batch -l /tmp/xxx.el", then the libgccjit
;; will search the crtbegin*.o, change native-comp-driver-options to help
;; libgccjit to locate the essential files.
(when (native-comp-available-p)
  (custom-set-variables
   '(native-comp-async-jobs-number (1- (num-processors)))
   '(native-comp-async-env-modifier-form  ; dirver or compiler options
     `(setq native-comp-driver-options '(,(concat "-B" (expand-file-name "../lib64/" invocation-directory)))))))

(defvar sl-savefile-dir (if-let* ((save-dir (expand-file-name "~/.emacs.save/"))
                                  (_ (file-exists-p save-dir)))
                            save-dir
                          user-emacs-directory))

(load (expand-file-name ".home.sl/emacs.spacemacs/init" portable-home-dir))

;; assume the spacemaces was installed.
(when-let* ((spacemacs-dir (expand-file-name ".emacs.spacemacs/" portable-home-dir))
            (_ (file-exists-p spacemacs-dir))
            (sl-spacemacs-init (locate-file "init" (list spacemacs-dir) load-suffixes)))

  (setq sl-packages-excluded
        '(anaconda-mode
          ccls
          chinese-conv
          company-anaconda
          company-rtags
          company-ycmd
          flycheck-rtags
          flycheck-ycmd
          gtags
          helm-rtags
          lsp-pyright
          lsp-python-ms
          magit-svn
          rainbow-delimiters
          rtags
          tern
          tide
          xcscope
          ycmd))
  (setq sl-configuration-layers
        '(auto-completion
          better-defaults
          emacs-lisp
          helm
          ibuffer
          imenu-list
          (lua :variables
               lua-indent-offset 4
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
              javascript
              (multiple-cursors :variables multiple-cursors-backend 'mc)
              org
              python
              sql
              (version-control :variables version-control-diff-tool 'diff-hl) ; avoid depending the git-gutter
              windows-scripts))
     (setq exec-path ; remove Python-App from path to avoid issues for Emacs-Win32
           (cl-delete-if
            (apply-partially 'string-match-p "AppData/Local/Programs/Python")
            exec-path))

     (when (not (executable-find invocation-name))
       (warn "Emacs not in PATH, recommend '[...\\mingw64.exe] bash -lc runemacs'")))

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
              git
              html
              imenu-list
              ietf
              (javascript :variables
                          js2-basic-offset 2
                          javascript-backend 'lsp
                          javascript-lsp-linter nil)
              (lsp :variables
                   lsp-restart 'ignore
                   ;; lsp-semantic-tokens-enable t
                   lsp-clients-lua-language-server-install-dir (expand-file-name "share/lua-language-server" portable-root-dir)
                   ;; lsp-lua-runtime-path ["?.lua" "?/init.lua" "?/?.lua" "../?/?.lua"]
                   lsp-lua-workspace-preload-file-size 500)
              (multiple-cursors :variables multiple-cursors-backend 'mc)
              octave
              (org :variables
                   org-plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))
              (plantuml :variables plantuml-default-exec-mode 'jar
                        plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))
              python
              rust
              (shell :variables shell-default-shell 'vterm)
              syntax-checking
              systemd
              (sql :variables sql-capitalize-keywords t
                   sql-capitalize-keywords-blacklist '("name" "varchar"))
              typescript
              (version-control :variables version-control-diff-tool 'diff-hl) ; avoid depending the git-gutter
              (xclipboard :variables xclipboard-enable-cliphist t)
              yaml))
     (delq 'shell sl-configuration-layers) ;delete the no-argument `shell' layer
     (when (fboundp 'image-mask-p)
       (add-to-list 'sl-packages-list 'org-pdftools)
       (nconc sl-configuration-layers
              '(graphviz
                pdf))
       (custom-set-variables '(pdf-view-restore-filename "~/.emacs.d/.cache/pdf-view-restore")))
     (when-let* ((default-directory portable-home-dir)
                 (paths (file-expand-wildcards ".local/LanguageTool*/languagetool-commandline.jar" t)))
       (add-to-list 'sl-configuration-layers `(languagetool :variables langtool-language-tool-jar ,(car paths)))))

    (_ ;; terminal without X11, a minimum config
     (nconc sl-packages-excluded '(pdf-tools
                                   org-pdftools
                                   yasnippet
                                   yasnippet-snippets))
     (nconc sl-configuration-layers '())
     (with-eval-after-load "files" (delete '("\\.org\\'" . org-mode) auto-mode-alist))))

  (define-advice dotspacemacs/layers (:after ())
    (setq-default dotspacemacs-configuration-layers sl-configuration-layers
                  dotspacemacs-additional-packages sl-packages-list
                  dotspacemacs-excluded-packages sl-packages-excluded
                  dotspacemacs-editing-style 'emacs
                  dotspacemacs-maximized-at-startup nil
                  dotspacemacs-line-numbers '(:disabled-for-modes org-mode)))
  ;; load the spacemacs
  (load-file sl-spacemacs-init)
  (when-let (OHOME (getenv "OHOME"))
    ;; set `default-directory' to full path to avoid "~/bin" is invalid after change HOME
    (setq default-directory (file-truename default-directory))
    (setenv "HOME" OHOME)
    (setenv "OHOME" nil))
  (setq dotspacemacs-frame-title-format "%b@%S")
  ;; post-config for spacemacs
  (when (fboundp 'pyim-activate)
    (custom-set-variables '(pyim-default-scheme 'wubi))
    (declare-function 'pyim-extra-dicts-add-dict "pyim-dict")
    (define-advice pyim-activate (:before (&optional _) mydicts)
      (advice-remove 'pyim-activate 'pyim-activate@mydicts)
      (dolist (x '("share/pyim-wbdict-rime.rime")) ;"share/pyim-wbdict-v86.pyim"
        (pyim-extra-dicts-add-dict
         `(:name ,(file-name-base x) :file ,(expand-file-name x portable-root-dir))))))

  (menu-bar-mode t)
  (when (string-match "X11" system-configuration-features)
    (use-package org-pdftools ; make sure the function org-pdftools-setup-link exists
      :defer t :after org :config (org-pdftools-setup-link))
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (when (string-match-p "-dark" (format "%s" custom-enabled-themes))
                  (pdf-view-midnight-minor-mode t)))))
  ;; (or (file-exists-p plantuml-jar-path) (plantuml-download-jar)); download plantuml.jar
  (with-eval-after-load 'plantuml-mode
    (when (fboundp 'image-mask-p)
      (plantuml-set-output-type "png")) ; text in svg image hard to see in dark theme
    (define-advice plantuml-jar-output-type-opt (:around (ORIG output-type))
      (funcall ORIG (if (display-graphic-p) output-type "txt")))) ; "txt" for terminal

  ;; disable img resize for window size is changed by HELM windows
  (custom-set-variables '(image-auto-resize-on-window-resize nil)
                        '(image-auto-resize ; resize image for HiDPI
                          (if-let ((scale (getenv "GDK_DPI_SCALE")))
                              (string-to-number scale)
                            t)))

  (with-eval-after-load 'helm-files
    (require 'filenotify)
    (setq helm-ff-use-notify (and file-notify--library t)))

  ;;;; fix the c-basic-offset for google-c-style
  ;; (with-eval-after-load 'google-c-style
  ;;   '(dolist (v google-c-style)
  ;;     (when (and (listp v) (eq (car v) 'c-basic-offset))
  ;;       (setcdr v 4))))
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda () (setq-local tab-width c-basic-offset)))

  (when (daemonp)
    (with-temp-buffer (helm-mode)) ;; preload heavy packages
    (with-temp-buffer (org-mode)))

  (define-advice undo-tree-save-history-from-hook (:around (ORIG))
    (when (buffer-modified-p) (funcall ORIG)))

  (define-advice git-gutter-mode (:around (ORIG &optional ARG) large-file)
    (if (< (point-max) (* 512 1024))
        (funcall ORIG ARG)
      (message "disable git-gutter for large file")))

  (with-eval-after-load 'multi-term
    (nconc term-bind-key-alist '(("<M-backspace>" . term-send-backward-kill-word))))

  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "C-y") 'vterm--self-insert))

  (let ((template-file (or load-file-name (buffer-file-name))))
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
      (define-key input-decode-map "[;6~" [C-S-backspace]))))

(add-hook 'after-make-frame-functions #'sl-term-kdb-patch)
(sl-term-kdb-patch (selected-frame)) ; patch 'after-make-frame-functions for the initialed term

(add-to-list 'after-init-hook (apply-partially 'xterm-mouse-mode 0))
(add-to-list 'after-init-hook #'global-hungry-delete-mode)

;; (custom-set-variables
;; (window-system-default-frame-alist
;;  '((x . ((left . 0) (top . 50) (width . 190) (height . 50))) ;NOTE: get vlaues by (frame-position), (frame-height), (frame-width)
;;    (w32 . ((left . (+ -10)) (top . 1) (width . 190) (height . 52)))))

;; (when-let (my-project-init (locate-file "projects" (list sl-savefile-dir)))
;;   (add-hook 'after-init-hook (lambda () (load-file my-project-init)))
;;   (defun sl-ede-project-reload ()
;;     (interactive)
;;     (setq ede-cpp-root-project-list nil)
;;     (load-file my-project-init)))

;;; dot-emacs-template.el ends here
