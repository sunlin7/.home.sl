;;; dot-emacs-template --- a template for .emacs -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; It's a good start for custom dot-emacs file.
;; And Emacs 28.1+ is required.
;;; Code:

(autoload 'if-let* "subr-x")
(autoload 'when-let* "subr-x")

;; async-compile will invoke "emacs --batch -l /tmp/xxx.el", then the libgccjit
;; will search the crtbegin*.o, change native-comp-driver-options to help
;; libgccjit to locate the essential files.
(custom-set-variables
 '(native-comp-async-jobs-number (when (fboundp 'num-processors) (num-processors)))
 '(native-comp-async-env-modifier-form  ; dirver or compiler options
   `(setq native-comp-driver-options '(,(concat "-B" (expand-file-name "../lib64/" invocation-directory))))))

(defvar portable-root-dir (expand-file-name ".." invocation-directory))
(defvar portable-home-dir
  (if (and (null (fboundp 'image-mask-p)) ; try .rootm-*/.emacs for noX build
	         (locate-file ".emacs" (list portable-root-dir)))
      portable-root-dir)
  (file-name-directory (file-truename (or load-file-name buffer-file-name))))

(defvar sl-savefile-dir (if-let* ((save-dir (expand-file-name "~/.emacs.save/"))
                                  (_ (file-exists-p save-dir)))
                            save-dir
                          user-emacs-directory))

(load (expand-file-name ".home.sl/emacs.spacemacs/init" portable-home-dir))

;; remove windows Python from path which has issues for emacs-win32
(setq exec-path
      (seq-remove
       (apply-partially 'string-match-p "AppData/Local/Programs/Python")
       exec-path))

;; assume the spacemaces was installed.
(when-let* ((spacemacs-dir (expand-file-name ".emacs.spacemacs/" portable-home-dir))
            (_ (file-exists-p spacemacs-dir))
            (sl-spacemacs-init (locate-file "init" (list spacemacs-dir) load-suffixes)))

  (pcase system-type
    ('windows-nt
     (setq sl-packages-excluded
           '(anaconda-mode
             ccls
             rtags
             company-rtags
             company-ycmd
             flycheck-rtags
             flycheck-ycmd
             helm-rtags
             magit-svn
             tern
             tide)
           sl-configuration-layers
           '(auto-completion
             better-defaults
             csv
             emacs-lisp
             git
             helm
             ibuffer
             ietf
             javascript
             lua
             (multiple-cursors :variables multiple-cursors-backend 'mc)
             org
             python
             sql
             (version-control :variables version-control-diff-tool 'diff-hl) ; avoid depending the git-gutter
             windows-scripts)))
    ((guard (or (fboundp 'image-mask-p) (native-comp-available-p)))
     (setq sl-packages-excluded
           '(anaconda-mode
             ccls
             chinese-conv
             rainbow-delimiters
             rtags
             ycmd
             gtags
             company-rtags
             company-ycmd
             company-anaconda
             flycheck-rtags
             flycheck-ycmd
             helm-rtags
             lsp-pyright
             lsp-python-ms
             magit-svn
             tern
             tide
             xcscope)
           sl-configuration-layers
           '(auto-completion
             better-defaults
             (c-c++ :variables
                    ;; c-c++-enable-google-style t
                    ;; c-c++-enable-google-newline t
                    c-c++-backend 'lsp-clangd)
             (chinese :variables chinese-default-input-method t
                      chinese-enable-avy-pinyin nil)
             cmake
             csv
             dap
             django
             emacs-lisp
             epub
             git
             helm
             html
             ibuffer
             imenu-list
             ietf
             (javascript :variables
                         js2-basic-offset 2
                         javascript-backend 'lsp
                         javascript-lsp-linter nil)
             (lsp :variables
                  ;; lsp-semantic-tokens-enable t
                  lsp-clients-lua-language-server-install-dir (expand-file-name "share/lua-language-server" portable-root-dir)
                  ;; lsp-lua-runtime-path ["?.lua" "?/init.lua" "?/?.lua" "../?/?.lua"]
                  lsp-lua-workspace-preload-file-size 500)
             (lua :variables
                  lua-indent-offset 4
                  lua-lsp-server 'lua-language-server)
             markdown
             (multiple-cursors :variables multiple-cursors-backend 'mc)
             nginx
             (org :variables
                  org-plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))
             octave
             python
             rust
             (shell :variables shell-default-shell 'vterm)
             syntax-checking
             systemd
             (sql :variables sql-capitalize-keywords t
                  sql-capitalize-keywords-blacklist '("name" "varchar"))
             typescript
             vimscript
             (version-control :variables version-control-diff-tool 'diff-hl) ; avoid depending the git-gutter
             (xclipboard :variables xclipboard-enable-cliphist t)
             yaml
             windows-scripts)))
    (_ ;; terminal without X11, a minimum config
     (with-eval-after-load "files" (delete '("\\.org\\'" . org-mode) auto-mode-alist))
     (setq sl-packages-excluded
           '(anaconda-mode
             ccls
             rtags
             ycmd
             company-rtags
             company-ycmd
             company-anaconda
             flycheck-rtags
             flycheck-ycmd
             helm-rtags
             pdf-tools
             org-pdftools
             tern
             tide
             yasnippet
             yasnippet-snippets)
           sl-configuration-layers
           '(auto-completion
             better-defaults
             emacs-lisp
             helm
             (lua :variables lua-indent-offset 4)
             ibuffer
             imenu-list
             markdown
             nginx
             shell
             yaml
             vimscript))))

  (when (fboundp 'image-mask-p)
    (setq  sl-packages-list (append sl-packages-list '(org-pdftools))
           sl-configuration-layers
           (append sl-configuration-layers
                   '(graphviz
                     pdf
                     (plantuml :variables plantuml-default-exec-mode 'jar
                               plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))))))
  (when-let* ((jarpath "~/.local/LanguageTool-6.0-SNAPSHOT/languagetool-commandline.jar")
              (_ (file-exists-p jarpath)))
    (add-to-list 'sl-configuration-layers `(languagetool :variables langtool-language-tool-jar ,jarpath)))

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
    ;; set the default-directory to full path before change HOME (or "~/bin" is invalid after change HOME)
    (setq default-directory (file-truename default-directory))
    (setenv "HOME" OHOME)
    (setenv "OHOME" nil))
  (setq dotspacemacs-frame-title-format "%b@%S")
  ;; (setq dotspacemacs-line-numbers t) ;; not work here, onlywork in .spacemacs
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
    (declare-function plantuml-set-output-type "plantuml-mode")
    (plantuml-set-output-type "png")) ; text in svg image hard to see in dark theme

  ;; disable img resize for window size is changed by HELM windows
  (custom-set-variables '(image-auto-resize-on-window-resize nil)
                        '(image-auto-resize ; resize image for HiDPI
                          (if-let ((scale (getenv "GDK_DPI_SCALE")))
                              (string-to-number scale)
                            t)))
  ;;;; fix the c-basic-offset for google-c-style
  ;; (with-eval-after-load 'google-c-style
  ;;   '(dolist (v google-c-style)
  ;;     (when (and (listp v) (eq (car v) 'c-basic-offset))
  ;;       (setcdr v 4))))
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda () (setq-local tab-width c-basic-offset)))
  )

(when (daemonp)
  (with-temp-buffer (helm-mode)) ;; preload heavy packages
  (with-temp-buffer (org-mode)))

(when (and (eq system-type 'windows-nt) (not (executable-find invocation-name)))
  (warn "Emacs not in PATH, recommend '[...\\mingw64.exe] bash -lc runemacs'"))

(define-advice undo-tree-save-history-from-hook (:around (ORIG))
  (when (buffer-modified-p) (funcall ORIG)))

(define-advice semantic-find-file-noselect (:around (orig file &rest r))
  (let ((find-file-hook nil))
    (apply orig file r)))

(define-advice git-gutter-mode (:around (ORIG &optional ARG) large-file)
               (if (< (point-max) (* 512 1024))
                   (funcall ORIG ARG)
                 (message "disable git-gutter for large file")))

(defun sl-term-kdb-patch (frame)
  "Update key binding in terminal, `$showkey -a` for key sequence."
  (when (terminal-live-p (frame-terminal frame))
    (with-selected-frame frame
      (define-key input-decode-map "[;5~" [C-backspace])
      (define-key input-decode-map "[;6~" [C-S-backspace]))))

(add-hook 'after-make-frame-functions #'sl-term-kdb-patch)
(sl-term-kdb-patch (selected-frame)) ; patch 'after-make-frame-functions for the initialed term

(xterm-mouse-mode 0)

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
