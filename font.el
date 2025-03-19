(defun set-global-fonts (monospace-font-string chinese-font-name emoji-font-name)
  "Set the global fonts."
  (set-frame-font monospace-font-string nil t)
  (set-fontset-font t 'unicode (font-spec :family emoji-font-name :size 16))
  (set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family chinese-font-name :size 26)))

(provide 'font)
