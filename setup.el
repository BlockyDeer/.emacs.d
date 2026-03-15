(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-initialize)

(setq frame-title-format "%b - Ahoge Emacs")

(menu-bar-mode -1)
(tool-bar-mode -1)

(defun find-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun dired-config-file ()
  (interactive)
  (dired "~/.emacs.d/"))

(defun find-hypr-config-file ()
  (interactive)
  (find-file "~/.config/hypr/hyprland.conf"))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'setup-package)
