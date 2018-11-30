;;; cjk-font --- settings for the CJK fonts.
;;; Commentary:
;;; Code:
;; (set-default-font "Ubuntu Mono")
;; (dolist (charset '(kana han symbol cjk-misc gb18030 bopomofo))
;;   (set-fontset-font "fontset-default"
;; 		    charset
;; 		    '("WenQuanYi Zen Hei" . "unicode-bmp")))

(defun sl-font-exists-p (font)
  (not (null (x-list-fonts font))))

(defun sl-set-cjk-font (en-font en-font-size
                                ch-font &optional ch-font-size)
  "en-font-size could be set to \":pixelsize=10\" or a integer.
If set/leave ch-font-size to nil, it will follow en-font-size"

  (let ((str (if (and (stringp en-font-size)
                     ;; `font-size' begin with ":"
                     (char-equal (aref en-font-size 0) ?:))
                 "%s%s" "%s %s")))
    (when (and (sl-font-exists-p en-font) (sl-font-exists-p ch-font))
      (let ((en-font (format str en-font en-font-size))
            (zh-font (font-spec :family ch-font :size ch-font-size)))
        ;; set English font
        (set-face-attribute
         'default nil :font en-font)

        ;; Set Chinese font
        ;; Do NOT use Unicode font, it will cause the English font setting invalid
        (dolist (charset '(kana han symbol cjk-misc gb18030 bopomofo))
          ;; (set-fontset-font (frame-parameter nil 'font)
          (set-fontset-font "fontset-default" charset zh-font))))))

(defun sl-choose-fonts ()
  "set the fonts for cjk"
  (sl-set-cjk-font
   "Ubuntu Mono" ":pixelsize=16"
   "WenQuanYi Zen Hei Mono"))

(defun sl-set-frame-font(&optional frame)
  "set the frame fonts"
  (when (display-graphic-p frame)
    (select-frame frame)
    (sl-choose-fonts)))

(add-hook 'after-make-frame-functions 'sl-set-frame-font)
(when (display-graphic-p)
  (sl-choose-fonts))

(provide '20cjk-font)
;;; 20cjk-font ends here
