;;; 我的emacs配置 -- init.el

(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(menu-bar-mode -1)

;; 安装 use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 启用 use-package
(eval-when-compile
  (require 'use-package))

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp)
         (c++-mode . lsp))
  :commands lsp)

(defun company-go-mode-setup()
  "Create golang company backend."
  (setq-local company-backends '(
                                 company-capf
                                 company-dabbrev-code
                                 company-files)))
(use-package go-mode
  :ensure t
  :hook (
         (before-save . gofmt-before-save)
         (go-mode . company-go-mode-setup)))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; (global-display-line-numbers-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                   (progn (display-line-numbers-mode 1)))
      )
)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun my-c-c++-mode-hook ()
  "个人 C/C++ 模式下的缩进设置"
  (setq c-default-style "linux")    ;; 使用 Linux 风格的缩进（可选，可以改为 "bsd" 或 "gnu"）
  (setq c-basic-offset 8)           ;; 每层缩进使用 4 个空格
  (setq tab-width 8)                ;; Tab 键宽度设置为 4
  (setq indent-tabs-mode t))      ;; 禁止使用 Tab 字符，使用空格

(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)

(defun my-go-comment-line ()
  "注释当前行，如果该行已经注释，则取消注释。"
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\s-*//")  ;; 检查是否已经是注释
      (delete-char 2)         ;; 如果是注释，则删除 '//' 来取消注释
    (insert "//")))            ;; 如果不是注释，则插入 '//'

(defun my-go-mode-setup ()
  "Go-mode 配置"
  (local-set-key (kbd "C-c C-/") 'my-go-comment-line))

(add-hook 'go-mode-hook 'my-go-mode-setup)

(defvar my-move-beginning-of-line-count 0
  "用于跟踪 C-a 按下次数的变量。")

(defun my-move-beginning-of-line ()
  "第一次按下 C-a 时移动到行首，第二次按下时移动到第一个非空白字符位置。"
  (interactive)
  (if (= my-move-beginning-of-line-count 0)
      (progn
        (move-beginning-of-line 1)
        (setq my-move-beginning-of-line-count 1))  ;; 第一次按下，正常移动到行首
    (progn
      (move-beginning-of-line 2)  ;; 第二次按下，跳到第一个非空白字符
      (setq my-move-beginning-of-line-count 0))))  ;; 重置计数

;; 绑定新功能到 C-a
(global-set-key (kbd "C-a") 'my-move-beginning-of-line)

(global-set-key (kbd "<f6>") 'recompile)

(use-package neotree
  :ensure t
  :bind
  ("<f8>" . neotree-toggle)  ;; 使用 F8 打开/关闭侧边栏
  :config
  (setq neo-window-width 30)  ;; 设置侧边栏的宽度
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t))

(add-hook 'neo-enter-hook
            (lambda ()
              (display-line-numbers-mode)))

(electric-pair-mode 1)

(use-package rainbow-mode
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)

(load (expand-file-name "ans-mode.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
