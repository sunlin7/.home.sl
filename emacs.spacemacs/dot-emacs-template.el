;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This is template for dot-emacs.
;; It's a good start for custom dot-emacs file.

(defvar portable-root-dir (expand-file-name ".." invocation-directory))
(defvar portable-home-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defvar sl-savefile-dir (expand-file-name ".emacs.save/" portable-home-dir))
(let ((sl-init-file (expand-file-name ".home.sl/emacs.spacemacs/init.el" portable-home-dir)))
  (when (file-exists-p sl-init-file) (load-file sl-init-file)))

(defvar sl-x11-support (string-match-p " X11 " system-configuration-features))
;; (defvar PYTHON_VER_BIN "python3")

;; remove windows Python from path which has issues for emacs-win32
(setq exec-path
      (seq-remove
       (lambda (x) (string-match-p "AppData/Local/Programs/Python" x))
       exec-path))

;; assume the spacemaces was installed.
(setq spacemacs-start-directory (expand-file-name ".emacs.spacemacs/" portable-home-dir))
(defvar sl-spacemacs-init (expand-file-name "init.el" spacemacs-start-directory))
(when (file-exists-p sl-spacemacs-init)
  (let ((magit-exec (expand-file-name "bin/git" portable-root-dir)))
    (when (file-exists-p magit-exec) (setq-default magit-git-executable magit-exec)))
  (let ((gtags-el (expand-file-name "share/gtags/gtags.el" portable-root-dir)))
    (when (file-exists-p gtags-el) (load-file gtags-el)))
  ;; FIXME: the env PYTHONUSERBASE maybe incorrect in ~/.spacemacs.env, flushing it.
  ;; (when (executable-find PYTHON_VER_BIN) (setenv "PYTHONUSERBASE" portable-root-dir))

  (pcase system-type
    ('windows-nt
     (setq sl-packages-list (append sl-packages-list
                                    '(flycheck-popup-tip))
           sl-packages-excluded '(ccls
                                  rtags
                                  company-rtags
                                  company-ycmd
                                  flycheck-rtags
                                  flycheck-ycmd
                                  helm-rtags
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
             javascript
             lua
             multiple-cursors
             org
             python
             sql
             treemacs
             version-control
             windows-scripts
             )))
    ((guard sl-x11-support)
     (setq sl-packages-list (append sl-packages-list
                                    '(flycheck-popup-tip nov pdf-tools org-pdftools))
           sl-packages-excluded '(anaconda-mode
                                  ccls
                                  rtags
                                  ycmd
                                  company-rtags
                                  company-ycmd
                                  company-anaconda
                                  flycheck-rtags
                                  flycheck-ycmd
                                  helm-rtags
                                  tern
                                  tide
                                  xcscope)
           sl-configuration-layers
           '(auto-completion
             better-defaults
             csv
             chinese
             (c-c++ :variables
                    c-c++-enable-google-style t
                    c-c++-enable-google-newline t
                    ;; c-c++-enable-clang-support t
                    c-c++-backend 'lsp-clangd)
             dap
             django
             emacs-lisp
             git
             graphviz
             gtags
             helm
             html
             ibuffer
             imenu-list
             ietf
             (javascript :variables
                         js2-basic-offset 2
                         javascript-backend 'lsp
                         javascript-lsp-linter nil)
             lsp
             (lua :variables
                  lua-lsp-server 'lua-language-server
                  lsp-clients-lua-language-server-bin (expand-file-name "share/lua-language-server/bin/Linux/lua-language-server" portable-root-dir)
                  lsp-clients-lua-language-server-main-location (expand-file-name "share/lua-language-server/main.lua" portable-root-dir)
                  lua-indent-offset 4)
             markdown
             multiple-cursors
             nginx
             org
             octave
             (plantuml :variables
                       plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir)
                       org-plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))
             python
             php
             rust
             syntax-checking
             systemd
             smex
             (sql :variables
                  sql-capitalize-keywords t
                  sql-capitalize-keywords-blacklist '("name" "varchar"))
             ;; ivy
             (semantic :disabled-for emacs-lisp) ; company-backend for elisp has problem with semantic
             treemacs
             typescript
             vimscript
             version-control     ; depends on git-gutter
             (xclipboard :variables
                         xclipboard-enable-cliphist t)
             yaml
             windows-scripts)))
    (_ ;; terminal without X11, a minimum config
     (setq sl-packages-list (append sl-packages-list
                                    '(flycheck-popup-tip))
           sl-packages-excluded '(anaconda-mode
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
                                  yasnippet-snippets
                                  xcscope)
           sl-configuration-layers
           '(auto-completion
             better-defaults
             emacs-lisp
             helm
             ibuffer
             imenu-list
             markdown
             nginx
             smex
             ;; ivy
             (semantic :disabled-for emacs-lisp) ; company-backend for elisp has problem with semantic
             yaml
             vimscript))))

  (let ((dotspath (expand-file-name ".spacemacs" portable-home-dir)))
    (when (file-exists-p dotspath) (defvar dotspacemacs-filepath dotspath)))
  ;; load the spacemacs
  (load-file sl-spacemacs-init)
  (setq dotspacemacs-frame-title-format "%b@%S")
  ;; post-config for spacemacs
  (when (featurep 'pyim)
    (let ((file (expand-file-name "share/pyim-wbdict-v86.rime" portable-root-dir)))
      (pyim-extra-dicts-add-dict `(:name "wbdict-v86-rime" :file ,file)))
    (custom-set-variables '(pyim-default-scheme 'wubi)
                          '(default-input-method "pyim")
                          '(pyim-assistant-scheme-enable t)))

  (menu-bar-mode t)
  (when sl-x11-support
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
    (pdf-loader-install)
    (eval-after-load 'pdf-tools
      #'(push 'pdf-view-midnight-minor-mode pdf-tools-enabled-modes))
    (use-package org-pdftools
      :hook (org-load . org-pdftools-setup-link)))
  ;; (when (not (file-exists-p plantuml-jar-path)) ; download plantuml automatically
  ;;  (plantuml-download-jar))
  ;; (eval-after-load 'plantuml-mode #'(plantuml-set-output-type "png"))
  (custom-set-variables '(plantuml-default-exec-mode 'jar))

  ;;;; fix the c-basic-offset for google-c-style
  ;; (eval-after-load 'google-c-style
  ;;   (dolist (v google-c-style)
  ;;     (when (and (listp v) (eq (car v) 'c-basic-offset))
  ;;       (setcdr v 4))))
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda () (setq-local tab-width c-basic-offset)))
  ;; check the the checkers
  (unless (executable-find "gcc")
    (custom-set-variables '(flycheck-disabled-checkers '(c/c++-gcc))))
  (unless (executable-find "clang")
    (custom-set-variables '(flycheck-disabled-checkers '(c/c++-clang)))))

(when (daemonp)
  (add-hook 'after-init-hook
            (lambda () (cd "~")
              ;; try to preload these on mingw64/cygwin
              (with-temp-buffer
                (require 'helm-files)
                (require 'helm-external)
                (require 'helm-mode)
                (helm-mode t)))))

(unless (executable-find invocation-name)
  (warn "Emacs not in PATH, recommend '[...\\mingw64.exe] bash -lc runemacs'"))

(xterm-mouse-mode 0)

(advice-add 'pyvenv-activate :around
            #'(lambda (ORIG directory)
                (let ((sl-calling-pyvenv-activate t))
                  (funcall ORIG directory))))
(advice-add 'pyvenv-deactivate :around
            #'(lambda (ORIG)
                (if (and (boundp 'sl-calling-pyvenv-activate)
                         sl-calling-pyvenv-activate)
                    (ignore-errors (funcall ORIG))
                  (funcall ORIG))))

(defun sl-adv-gud-basic-call (orig command)
  "addvice for `gud-basic-call'"
  (with-current-buffer (current-buffer)
    (save-excursion
      (save-restriction
        (funcall orig command)))))
(advice-add 'gud-basic-call :around #'sl-adv-gud-basic-call)

(global-set-key (kbd "\e[127;5u") [C-backspace])
(global-set-key (kbd "\e[127:5u") [C-backspace])
(global-set-key (kbd "\e[127;6u") [C-S-backspace])
(global-set-key (kbd "\e[127:6u") [C-S-backspace])

;; (custom-set-variables
;;  '(default-frame-alist
;;     '((top . 1)
;;       (left . 0)
;;       (width . 149) ; (message "%s %s" (frame-width) (frame-height))
;;       (height . 37))))

;; (let ((my-project-init (expand-file-name "projects.el" sl-savefile-dir)))
;;   (when (file-exists-p my-project-init)
;;     (add-hook 'after-init-hook (lambda () (load-file my-project-init)))
;;     (defun sl-ede-project-reload ()
;;       (interactive)
;;       (setq ede-cpp-root-project-list nil)
;;       (load-file my-project-init))))
