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

(use-package which-key
  :ensure t)
(which-key-mode)

(setq
     backup-by-copying t ; 自动备份
     backup-directory-alist
     '(("." . "~/.emacs_backup")) ; 自动备份在目录"~/.em_backup"下
     delete-old-versions t ; 自动删除旧的备份文件
     kept-new-versions 3 ; 保留最近的3个备份文件
     kept-old-versions 1 ; 保留最早的1个备份文件
     version-control t) ; 多次备份

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp)
         (c++-mode . lsp))
  :commands lsp)

(use-package clang-format
  :ensure t)

(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "C-c f") 'clang-format-buffer)))

(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-c f") 'clang-format-buffer)))

(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c f") 'gofmt)))

(defun company-go-mode-setup()
  "Create golang company backend."
  (setq-local company-backends '(
                                 company-capf
                                 company-dabbrev-code
                                 company-files)))

(global-set-key (kbd "M-o") 'company-complete)

(use-package go-mode
  :ensure t
  :hook (
         (before-save . gofmt-before-save)
         (go-mode . company-go-mode-setup)))

(use-package go-gen-test
  :ensure t)

(use-package go-dlv
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; 配置 flycheck 使用 chktex
(setq-default flycheck-latex-chktexrc "~/.chktexrc")

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode 'display-line-numbers-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "DEL") 'backward-delete-char)

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

(global-set-key (kbd "<f7>") 'repeat-complex-command)

(use-package neotree
  :ensure t
  :bind
  ("<f8>" . neotree-toggle)  ;; 使用 F8 打开/关闭侧边栏
  :config
  (setq neo-window-width 30)  ;; 设置侧边栏的宽度
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t)
  (setq neo-show-updir-line nil))

(use-package magit
  :ensure t)

(add-hook 'magit-post-commit-hook 'neotree-refresh)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package conf-mode
  :ensure t
  :mode ("\\.ini\\'" . conf-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(load-theme 'tango-dark t)

(defvar executable-path "./main"
  "The path to the executable file.")

(defun set-executable-path ()
  "Prompt the user to set the path for the executable file."
  (interactive)
  (setq executable-path (read-file-name "")))

(defun set-compile-command-for-golang ()
  "Set the default command to compile for golang"
  (setq-local compile-command "go build"))

(require 'compile)
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (recompile)  ;; 进行构建
                  (when my-executable-path
                    (shell-command my-executable-path))  ;; 使用设置的路径运行可执行文件
                  ))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(clang-format yaml-mode which-key rainbow-mode rainbow-delimiters neotree magit lsp-mode json-mode go-gen-test go-dlv flycheck company auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
