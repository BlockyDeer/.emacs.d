;;; commentary: 我的emacs配置 -- init.el

;;; Code:
(defun load-el (path)
 "Load single el file from user Emacs directory.  PATH: string, The file path."
 (load (expand-file-name path user-emacs-directory)))

(add-to-list 'load-path "~/.emacs.d/holo-layer/")
(require 'holo-layer)
(setq holo-layer-enable-cursor-animation t)
;;(setq holo-layer-enable-indent-rainbow t)
;; This animation is too ugly for me.
;;(setq holo-layer-enable-type-animation t)
(setq holo-layer-python-command "python")
(holo-layer-enable)

(load-el "setup.el")

(defun switch-to-minibuffer ()
 "Switch to minibuffer window."
 (interactive)
 (if (active-minibuffer-window)
   (select-window (active-minibuffer-window))
  (error "Minibuffer is not active")))
(global-set-key (kbd "C-c C-o") 'switch-to-minibuffer)

;; General Configuration
;; Theme and font
(use-package dracula-theme
 :ensure t)
(load-theme 'dracula t)
(load-el "font.el")
;;(set-global-fonts "FiraCode Nerd Font-16" "Noto Sans CJK SC" "Noto Color Emoji")
(set-frame-font "Maple Mono Normal NF CN-14" nil t)

(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

(use-package emacs
 :ensure nil
 :custom
 (indent-tabs-mode nil)
 (tab-width 8))

(use-package compile
 :ensure nil
 :config (setq compilation-ask-about-save nil))

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
 (auto-save-visited-interval 1))

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
 ("C-`" . 'rime-send-keybinding))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode t))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . 'mc/edit-lines))

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map
        ("b" . dired-create-empty-file)))

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
  :bind
  ("M-o" . company-complete))
(global-company-mode)

(use-package yasnippet
  :ensure t
  :bind
  ("C-c i" . yas-insert-snippet)
  ("M-p" . yas-expand))
(yas-global-mode t)

;; LSP
(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (gdscript-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :config
  (setq eglot-semantic-token-faces
        '(("macro" . font-lock-macro-face)))
  :bind
  (:map eglot-mode-map
        ("C-c C-r" . 'eglot-rename)
        ("C-c C-i" . 'eglot-code-actions)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package meson-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package clang-format
  :ensure t)

(use-package format-all
  :ensure t
  :bind ("C-c C-f" . format-all-buffer))

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-map
              ("C-c C-f" . 'clang-format-buffer)
              :map c++-mode-map
              ("C-c C-f" . 'clang-format-buffer)))

(use-package gdscript-mode
  :ensure t)

(use-package display-line-numbers
  :ensure nil
  :hook
  ((text-mode-hook prog-mode-hook conf-mode-hook) . display-line-numbers-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-auto-character-face-perc 30)
 '(highlight-indent-guides-auto-even-face-perc 30)
 '(highlight-indent-guides-auto-odd-face-perc 25)
 '(highlight-indent-guides-method 'character))

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
  (format-all-formatters '(("Lua" (stylua)))))

(use-package slime
  :ensure t)
(setq inferior-lisp-program "sbcl")

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

(use-package autothemer
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
