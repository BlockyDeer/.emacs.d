;;; commentary: 我的emacs配置 -- init.el

;;; Code:
(defun load-el (path)
  "Load single el file from user Emacs directory.  PATH: string, The file path."
  (load (expand-file-name path user-emacs-directory)))

(load-el "setup.el")

;; General Configuration
;; Theme and font
(load-theme 'tango-dark t)
(load-el "font.el")
;(set-global-fonts "FiraCode Nerd Font-16" "Noto Sans CJK SC" "Noto Color Emoji")
(set-frame-font "Maple Mono Normal NF CN-16" nil t)

(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

(use-package emacs
  :ensure nil
  :custom
  (indent-tabs-mode nil)
  (tab-width 8))

(use-package menu-bar
  :ensure nil
  :bind
  ("C-c y" . clipboard-yank))

;; files
(use-package files
  :ensure nil
  :config (auto-save-visited-mode t)
  :custom
  (backup-by-copying t)
  (backup-directory-alist '(("." . "~/.emacs_backup/")))
  (delete-old-versions t)
  (kept-new-versions 3)
  (kept-old-version 1)
  (version-control t)
  (auto-save-visited-interval 1)
  )

(use-package vlf
  :ensure t
  :commands (vlf))

(use-package wc-mode
  :ensure t
  :bind ("C-c C-c" . wc))

(use-package mini-frame
  :ensure t)
(mini-frame-mode t)

(use-package rime
  :ensure t
  :custom
  (default-input-method "rime")
  :bind
  ("C-`" . 'rime-send-keybinding)
  )

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode t))

(load-el "ans-mode.el")
(load-el "dashboard.el")
(load-el "liga.el")
(load-el "colorful.el")
(load-el "run.el")

(use-package find-file
  :ensure nil
  :bind ("C-c o" . ff-find-other-file))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  :bind
  ("M-o" . company-complete)
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :bind
  ("C-c i" . yas-insert-snippet)
  ("M-p" . yas-expand)
  )

;; LSP
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

(use-package meson-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package clang-format
  :ensure t)

(use-package format-all
  :ensure t
  :bind ("C-c C-f" . format-all-buffer))

;(add-hook 'c-mode-hook
;          (lambda () (define-key c-mode-map (kbd "C-c C-f") 'clang-format-buffer)))
;(add-hook 'c++-mode-hook
;          (lambda () (define-key c++-mode-map (kbd "C-c C-f") 'clang-format-buffer)))
;(add-hook 'go-mode-hook
;          (lambda () (define-key go-mode-map (kbd "C-c C-f") 'gofmt)))

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-map
              ("C-c C-f" . 'clang-format-buffer)
              :map c++-mode-map
              ("C-c C-f" . 'clang-format-buffer)))

(use-package display-line-numbers
  :ensure nil
  :hook
  ((text-mode-hook prog-mode-hook conf-mode-hook) . display-line-numbers-mode))

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


(use-package lua-mode
  :ensure t
  :bind ("C-c C-f" . 'format-all-buffer)
  :custom
  (format-all-formatters '(("Lua" (stylua))))
  )

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

