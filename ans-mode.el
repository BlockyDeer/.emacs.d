;;; ans-mode.el - 自定义 ans 文件模式

;;; Code:
(define-derived-mode ans-mode fundamental-mode "ans"
  "Major mode for .ans files which is visual novel script files for Ahoge Engine."
  (setq font-lock-defaults '(ans-mode-font-lock-keywords))
  (setq-local comment-start ";")
  (setq-local comment-end ""))

(defface ans-action-body-face
  '((t (:inherit font-lock-string-face :foreground "dark orange")))
  "Face for action body in ans-mode")

(defface ans-action-name-face
  '((t (:inherit font-lock-function-name-face :foreground "lime green" :slant italic)))
  "Face for action name in ans-mode")

(defface ans-macro-face
  '((t (:inherit font-lock-builtin-face :foreground "dark cyan" :slant italic)))
  "Face for macros in ans-mode.")

(defface ans-comment-face
  '((t (:inherit font-lock-comment-face :foreground "forest green")))
  "Face for comments in ans-mode.")

(defface ans-speaker-null-face
  '((t (:inherit font-lock-comment-face :foreground "gray" :slant italic)))
  "Face for speaker null")

(defface ans-speaker-face
  '((t (:inherit font-lock-constant-face :foreground "orange")))
  "Face for speakers")

;; Define the regex patterns and corresponding faces
(defvar ans-mode-font-lock-keywords
  '(
    ;; Comment: start with a ";" in a line
    ("^;[^\n]*" . 'ans-comment-face)
    ;; Action Name
    ("^\\(#[^ \t\n]+\\)" (1 'ans-action-name-face))
    ;; Action Body: start with # in a line
    ("^#[^\n]+" . 'ans-action-body-face)
    ;; Macro
    ("^@[^\n]+" . 'ans-macro-face)
    ;; Speaker
    ("^\\([^ \t;@#\n][^\n]*\\)$" 
     (1 (if (string-match-p "\\`null\\'" (match-string 1))
        'ans-speaker-null-face
      'ans-speaker-face))
    )))

;; Automatically activate ans-mode for .ans files
(add-to-list 'auto-mode-alist '("\\.ans\\'" . ans-mode))

(add-hook 'ans-mode-hook (lambda ()
                           (display-line-numbers-mode t)))

(provide 'ans-mode)
;;; ans-mode.el ends here
