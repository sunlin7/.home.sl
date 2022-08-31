;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-
;; This is template for dot-emacs.
;; It's a good start for custom dot-emacs file.
(autoload 'when-let "subr-x")
(autoload 'if-let* "subr-x")
(when-let (OHOME (getenv "OHOME"))
  ;; set the default-directory to full path before change HOME (or "~/bin" is invalid after change HOME)
  (setq default-directory (file-truename default-directory))
  (setq user-home-directory OHOME)
  (setenv "HOME" OHOME)
  (setenv "OHOME" nil))

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
  (file-name-directory (file-truename (or load-file-name (buffer-file-name)))))

(defvar sl-savefile-dir (if-let* ((save-dir (expand-file-name "~/.emacs.save/"))
                                  (_ (file-exists-p save-dir)))
                            save-dir
                          user-emacs-directory))

(load (expand-file-name ".home.sl/emacs.spacemacs/init" portable-home-dir))

;; remove windows Python from path which has issues for emacs-win32
(setq exec-path
      (seq-remove
       (lambda (x) (string-match-p "AppData/Local/Programs/Python" x))
       exec-path))

;; assume the spacemaces was installed.
(setq spacemacs-start-directory (expand-file-name ".emacs.spacemacs/" portable-home-dir))
(when-let (sl-spacemacs-init (locate-file "init" (list spacemacs-start-directory) load-suffixes))
  ;; FIXME: the env PYTHONUSERBASE maybe incorrect in ~/.spacemacs.env, flushing it.
  ;; (when (executable-find python3) (setenv "PYTHONUSERBASE" portable-root-dir))

  (pcase system-type
    ('windows-nt
     (setq sl-packages-excluded '(anaconda-mode
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
             (helm :variables helm-use-fuzzy 'source)
             ibuffer
             ietf
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
    ((guard (or (fboundp 'image-mask-p) (native-comp-available-p)))
     (setq sl-packages-excluded '(anaconda-mode
                                  ccls
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
             (chinese :variables chinese-default-input-method t)
             cmake
             csv
             dap
             django
             emacs-lisp
             epub
             git
             (helm :variables helm-use-fuzzy 'source)
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
                  lsp-clients-lua-language-server-bin (expand-file-name "share/lua-language-server/extension/server/bin/lua-language-server" portable-root-dir)
                  ;; lsp-clients-lua-language-server-main-location (expand-file-name "share/lua-language-server/main.lua" portable-root-dir)
                  ;; lsp-lua-runtime-path ["?.lua" "?/init.lua" "?/?.lua" "../?/?.lua"]
                  lsp-lua-workspace-preload-file-size 500)
             (lua :variables
                  lua-lsp-server 'lua-language-server
                  lua-indent-offset 4)
             markdown
             multiple-cursors
             nginx
             (org :variables
                  org-plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir))
             octave
             python
             rust
             shell
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
             version-control            ; depends on git-gutter
             (xclipboard :variables xclipboard-enable-cliphist t)
             yaml
             windows-scripts)))
    (_ ;; terminal without X11, a minimum config
     (with-eval-after-load "files" (delete '("\\.org\\'" . org-mode) auto-mode-alist))
     (setq sl-packages-excluded '(anaconda-mode
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
             (helm :variables helm-use-fuzzy 'source)
             (lua :variables lua-indent-offset 4)
             ibuffer
             imenu-list
             markdown
             nginx
             shell
             smex
             ;; ivy
             (semantic :disabled-for emacs-lisp) ; company-backend for elisp has problem with semantic
             yaml
             vimscript))))

  (when (fboundp 'image-mask-p)
    (setq  sl-packages-list (append sl-packages-list '(org-pdftools))
           sl-configuration-layers
           (append sl-configuration-layers
                   '(graphviz
                     pdf
                     (plantuml :variables
                               plantuml-jar-path (expand-file-name "share/plantuml.jar" portable-root-dir)
                               plantuml-default-exec-mode 'jar)))))

  (when-let (dotspath (locate-file ".spacemacs" (list portable-home-dir)))
    (defvar dotspacemacs-filepath dotspath))
  ;; load the spacemacs
  (load-file sl-spacemacs-init)
  (setq dotspacemacs-frame-title-format "%b@%S")
  ;; (setq dotspacemacs-line-numbers t) ;; not work here, onlywork in .spacemacs
  ;; post-config for spacemacs
  (when-let ((_ (fboundp 'pyim-activate))
             (file (expand-file-name "share/pyim-wbdict-v86.rime" portable-root-dir))
             (wubi-wait-initializing t))
    (custom-set-variables '(pyim-default-scheme 'wubi))
    (declare-function 'pyim-extra-dicts-add-dict "pyim-dict")
    (define-advice pyim-activate (:before (&optional _) add-wubi-dict)
                  (when wubi-wait-initializing
                    (pyim-extra-dicts-add-dict `(:name "wbdict-v86-rime" :file ,file))
                    (setq wubi-wait-initializing nil))))
  (menu-bar-mode t)
  (when (and (fboundp 'image-mask-p) (eq window-system 'x))
    (use-package org-pdftools ; make sure the function org-pdftools-setup-link exists
      :hook ((org-load . org-pdftools-setup-link)))
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (when (string-match-p "-dark" (format "%s" custom-enabled-themes))
                  (pdf-view-midnight-minor-mode t)))))
  ;; (unless (file-exists-p plantuml-jar-path) ; download plantuml automatically
  ;;  (plantuml-download-jar))
  (with-eval-after-load 'plantuml-mode
    (declare-function plantuml-set-output-type "plantuml-mode")
    (plantuml-set-output-type "png")) ; text in svg image hard to see in dark theme

  ;; disable img resize for window size is changed by HELM windows
  (custom-set-variables '(image-auto-resize-on-window-resize nil)
                        '(image-auto-resize ; resize image for HiDPI
                          (if-let ((scale (getenv "GDK_DPI_SCALE")))
                              (string-to-number scale)
                            t)))
  (with-eval-after-load 'quickrun
    (quickrun-add-command "c++11"
      '((:command . "g++")
        (:exec    . ("%c -std=c++11 %o -o %e %s"
		                 "%e %a"))
        (:remove  . ("%e")))
      :default "c++"))
  ;;;; fix the c-basic-offset for google-c-style
  ;; (with-eval-after-load 'google-c-style
  ;;   '(dolist (v google-c-style)
  ;;     (when (and (listp v) (eq (car v) 'c-basic-offset))
  ;;       (setcdr v 4))))
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda () (setq-local tab-width c-basic-offset)))
  ;; check the the checkers
  (with-eval-after-load "flycheck"
    (dolist (x '(("gcc" . c/c++-gcc) ("clang" . c/c++-clang)))
      (unless (executable-find (car x))
        (add-to-list 'flycheck-disabled-checkers (cdr x))))))

(when (daemonp)
  (add-hook 'after-init-hook
            (lambda () (cd "~")
              (with-temp-buffer ;; try to preload these on mingw64/cygwin
                (require 'helm-files)
                (require 'helm-external)
                (require 'helm-mode)
                (helm-mode t)))))

(when (and (eq system-type 'windows-nt) (not (executable-find invocation-name)))
  (warn "Emacs not in PATH, recommend '[...\\mingw64.exe] bash -lc runemacs'"))

(xterm-mouse-mode 0)

(define-advice git-gutter-mode (:around (ORIG &optional ARG) large-file)
               (if (< (point-max) (* 512 1024))
                   (funcall ORIG ARG)
                 (message "disable git-gutter for large file")))

(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              "Update key binding in terminal, `$showkey -a` for key sequence."
              (with-selected-frame frame
                (when (eq (terminal-live-p (frame-terminal frame)) t)
                  (define-key input-decode-map (kbd "\e[127:5u") [C-backspace])
                  (define-key input-decode-map (kbd "\e[127:6u") [C-S-backspace])))))

;; (custom-set-variables
;; '(default-frame-alist
;;    '(((top . 0) (left . 0) (width . 110) (height . 40) ; get by `(frame-height)', `(frame-width)' when frame is maximum
;;      (background-color . "black") (foreground-color . "white")))))

;; (when-let (my-project-init (locate-file "projects" (list sl-savefile-dir)))
;;   (add-hook 'after-init-hook (lambda () (load-file my-project-init)))
;;   (defun sl-ede-project-reload ()
;;     (interactive)
;;     (setq ede-cpp-root-project-list nil)
;;     (load-file my-project-init)))
