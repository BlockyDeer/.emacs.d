;;; commentary: 我的emacs配置 -- init.el

;;; Code:
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-initialize)

(defun load-el (path)
  "Load single el file from user Emacs directory.  PATH: string, The file path."
  (load (expand-file-name path user-emacs-directory)))

(setq frame-title-format "%b - Ahoge Emacs")

(menu-bar-mode -1)
(tool-bar-mode -1)

;; 安装 use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 启用 use-package
(eval-when-compile
  (require 'use-package))

(use-package vlf
  :ensure t)

(use-package wc-mode
  :ensure t)
(add-hook 'wc-mode-hook (lambda () (local-set-key (kbd "C-c C-c") 'wc)))

(use-package mini-frame
  :ensure t)
(mini-frame-mode t)

(use-package rime
  :ensure t
  :custom
  (default-input-method "rime")
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  )

(use-package which-key
  :ensure t)
(which-key-mode)

(load-el "font.el")
;(set-global-fonts "FiraCode Nerd Font-16" "Noto Sans CJK SC" "Noto Color Emoji")
(set-frame-font "Maple Mono Normal NF CN-16" nil t)

(setq
     backup-by-copying t ; 自动备份
     backup-directory-alist
     '(("." . "~/.emacs_backup")) ; 自动备份在目录"~/.em_backup"下
     delete-old-versions t ; 自动删除旧的备份文件
     kept-new-versions 3 ; 保留最近的3个备份文件
     kept-old-versions 1 ; 保留最早的1个备份文件
     version-control t) ; 多次备份

(global-set-key (kbd "C-c o") 'ff-find-other-file)

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-mode . eglot-ensure))
  :bind
  (:map eglot-mode-map
        ("C-c C-r" . 'eglot-rename)
        ("C-c C-i" . 'eglot-code-actions))
  )

;(use-package lsp-mode
;  :ensure t
;  :hook ((go-mode . lsp)
;         (c++-mode . lsp)
;         (c-mode . lsp)
;         (python-mode . lsp)
;         (lua-mode . lsp))
;  :commands lsp)

(use-package clang-format
  :ensure t)

(use-package format-all
  :ensure t)
(global-set-key (kbd "C-c C-f") 'format-all-buffer)
(add-hook 'lua-mode-hook
          (lambda () (setq format-all-formatters
                           '(("Lua" (stylua))))))
;; Format the buffer when it to be saved.
(add-hook 'prog-mode-hook 'format-all-mode)

(add-hook 'c-mode-hook
          (lambda () (define-key c-mode-map (kbd "C-c C-f") 'clang-format-buffer)))
(add-hook 'c++-mode-hook
          (lambda () (define-key c++-mode-map (kbd "C-c C-f") 'clang-format-buffer)))
(add-hook 'go-mode-hook
          (lambda () (define-key go-mode-map (kbd "C-c C-f") 'gofmt)))

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
(use-package go-gen-test
  :ensure t)
(use-package go-dlv
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode))
(global-set-key (kbd "M-o") 'company-complete)

;; compile setting
(global-set-key (kbd "<f6>") 'recompile)
;; default compiling commands
;; Force the Emacs loading the compile package first
(message "compile-command 初始值: %s" compile-command)
(require 'compile)
(message "加载 compile 包后值: %s" compile-command)
(add-hook 'prog-mode-hook
  (lambda ()
    (setq-local compile-command
      (cond
        ((derived-mode-p 'lua-mode) "lua main.lua")
        ((derived-mode-p 'go-mode) "go build .")
        (t "make -k")))))
(defun set-compile-command-c-or-cpp ()
  "Set compile command in c/c++-mode."
    (let (file (buffer-file-name))
      (when file
        (set (make-local-variable 'compile-command)
          (if (project-current)
              "meson compile -C build"
            (let* ((is-c (string= (file-name-extension file) "c")))
              (format "%s %s -Wall -Wextra -I. -Iinclude -g -o "
                      (if is-c "gcc" "g++")
                      (shell-quote-argument (file-name-nondirectory file)))))))))
(add-hook 'c-mode #'set-compile-command-c-or-cpp)
(add-hook 'c++-mode #'set-compile-command-c-or-cpp)

;(use-package flycheck
;  :ensure t
;  :config
;  (setq truncate-lines nil)
;  :hook
;  (prog-mode . flycheck-mode))

(use-package yasnippet
  :ensure t)
(yas-global-mode 1)
(global-set-key (kbd "C-c i") 'yas-insert-snippet)
(global-set-key (kbd "M-p") 'yas-expand)

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode 'display-line-numbers-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "DEL") 'backward-delete-char)

(defun my-c-c++-mode-hook ()
  (setq c-default-style "linux")
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (setq indent-tabs-mode t))

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
  (local-set-key (kbd "C-c C-/") 'my-go-comment-line))

(add-hook 'go-mode-hook 'my-go-mode-setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-window-height 12)
 '(mini-frame-show-parameters '((top . 10) (width . 0.7) (left . 0.5)))
 '(package-selected-packages nil))

(use-package treemacs
  :ensure t
  :defer t)
(global-set-key (kbd "<f8>") 'treemacs)

(setq treemacs-show-hidden-files t)

(use-package magit
  :ensure t)
(global-set-key (kbd "<f5>") 'magit-status)

(electric-pair-mode 1)

(load-el "ans-mode.el")
(load-el "dashboard.el")
(load-el "liga.el")
(load-el "colorful.el")
(load-el "run.el")

(use-package lua-mode
  :ensure t
  :hook
  (lua-mode . (lambda ()
                (define-key lua-mode-map (kbd "C-c C-f") 'format-all-buffer))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package impatient-mode
  :ensure t
  :mode ("\\.md\\'" . impatient-mode))

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

;; Web 支持
(use-package web-mode
  :ensure t)
(use-package emmet-mode
  :ensure t)
(use-package js2-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))  ;; HTML 文件
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))    ;; JavaScript 文件
(add-hook 'web-mode-hook  (lambda () (emmet-mode t)))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "javascript")
              (js2-mode))))

(use-package cmake-mode
  :ensure t)

(use-package cmake-ide
  :ensure t)
(cmake-ide-setup)

(use-package xclip
  :ensure t)

(when (not (display-graphic-p))
  (require 'xclip)
  (xclip-mode 1))

(provide 'init)
;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
