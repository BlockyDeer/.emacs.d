(use-package comment-tags
  :ensure t)
(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
          ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil))
(add-hook 'prog-mode-hook 'comment-tags-mode)
(use-package rainbow-mode
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)

(provide 'colorful.el)
