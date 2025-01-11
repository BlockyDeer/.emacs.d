;;; ans-mode.el - 自定义 ans 文件模式

;;; Code:
(define-derived-mode ans-mode fundamental-mode "ANS"
  "Major mode for editing .ans files with special highlighting for #{xxx} and @{xxx}."
  (setq font-lock-defaults '((ans-mode-font-lock-keywords))))

(defface ans-inline-action-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Face for highlighting #{...} patterns in ans-mode.")

(defface ans-macro-face
  '((t (:foreground "SteelBlue" :weight bold)))
  "Face for highlighting @{...} patterns in ans-mode.")

;; Define the regex patterns and corresponding faces
(defvar ans-mode-font-lock-keywords
  '(("#{\\([^}]+\\)}" . font-lock-function-name-face)  ;; Highlight #{xxx}
    ("\n[^ ]+ " . font-lock-doc-face) ;; 高亮说话人
    ("#[^\n]*\n" . font-lock-comment-face)
    ("@{\\([^}]+\\)}" . font-lock-function-name-face) ;; Highlight @{xxx}
  ))

;; Automatically activate ans-mode for .ans files
(add-to-list 'auto-mode-alist '("\\.ans\\'" . ans-mode))

(provide 'ans-mode)
;;; ans-mode.el ends here
