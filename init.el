;;; -*- lexical-binding: t; -*-
;;; commentary: 我的emacs配置 -- init.el

;;; Code:
(defun load-el (path)
  "Load single el file from user Emacs directory.  PATH: string, The file path."
  (load (expand-file-name path user-emacs-directory)))

;;(add-to-list 'load-path "~/.emacs.d/holo-layer/")
;;(require 'holo-layer)
;;(setq holo-layer-enable-cursor-animation t)
;;(setq holo-layer-enable-indent-rainbow t)
;; This animation is too ugly for me.
;;(setq holo-layer-enable-type-animation t)
                                        ;(setq holo-layer-python-command "python")
                                        ;(holo-layer-enable)

(setq minibuffer-frame-alist nil)

(load-el "setup.el")

(use-package mouse
  :bind (("M-<down-mouse-1>" . nil)
         ("M-<mouse-1>" . nil)
         ("M-<drag-mouse-1>" . nil)))

(modify-syntax-entry ?_ "w")

                                        ; Source - https://stackoverflow.com/a/71785402
                                        ; Posted by Charles G
                                        ; Retrieved 2026-01-30, License - CC BY-SA 4.0

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

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
(set-frame-font "Maple Mono Normal NF CN-12" nil t)

(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))

(global-subword-mode 1)
(save-place-mode 1)

(use-package grep
  :ensure nil
  :bind ("C-c s" . rgrep))

(use-package emacs
  :ensure nil
  :custom
  (indent-tabs-mode nil)
  (tab-width 8))

(use-package elisp-mode
  :ensure nil
  :bind (("C-c C-f" . apheleia-format-buffer)))

(use-package compile
  :ensure nil
  :config (setq compilation-ask-about-save nil)
  :bind ("<f6>" . compile))

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
  (kept-new-versions 10)
  (kept-old-version 1)
  (version-control t)
  (auto-save-visited-interval 1))

(use-package vc-hooks
  :ensure nil
  :custom
  (vc-make-backend-files t))

(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :bind ("C-c p" . projectile-command-map))

(use-package diminish
  :ensure t
  :config
  (diminish 'ivy-mode)
  (diminish 'yas-global-mode)
  (diminish 'company-mode))

(use-package vlf
  :ensure t
  :commands (vlf))

(use-package wc-mode
  :ensure t
  :bind ("C-c C-c" . wc))

;;(use-package mini-frame
;; :ensure t)
;;(mini-frame-mode t)

(use-package rime
  :ensure t
  :custom
  (default-input-method "rime")
  :bind
  ("C-`" . 'rime-send-keybinding))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode t))

(use-package display-fill-column-indicator
  :ensure nil
  :config (setq-default fill-column 80)
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package whitespace
  :ensure nil
  :config
  (global-whitespace-mode 1))


(use-package pinyinlib
  :ensure t
  :config
  (defun ivy--regex-pinyin (str)
    (ivy--regex (pinyinlib-build-regexp-string str))))
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :config
  ;; 将 ivy--regex-pinyin 设置为 swiper 的正则表达式构建函数
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-pinyin))
  (setq ivy-re-builders-alist
        '((t . ivy--regex-pinyin))))
(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode t))

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
  :custom
  (dired-listing-switches "-alGhv --group-directories-first")
  :bind
  (:map dired-mode-map
        ("b" . dired-create-empty-file)))

(use-package dired-rainbow
  :ensure t)

(use-package dired-collapse
  :ensure t
  :config (global-dired-collapse-mode))

(use-package trashed
  :ensure t)

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
         (rust-mode . eglot-ensure)
         (js2-mode . eglot-ensure))
  :config
  (setq eglot-semantic-token-faces
        '(("macro" . font-lock-macro-face)))
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rust-analyzer" 
                                             :initializationOptions (:cargo (:buildScripts (:enable t))))))
  :bind
  (:map eglot-mode-map
        ("C-c C-r" . 'eglot-rename)
        ("C-c C-i" . 'eglot-code-actions)))

(use-package eldoc
  :init
  (setq eldoc-display-functions
        '(eldoc-display-in-buffer)))

(use-package eldoc-box
  :ensure t
  :after eldoc
  :commands (eldoc-box-help-at-point)
  :bind
  (("C-c d" . eldoc-box-help-at-point)))

;; (use-package treesit-auto
;;   :if (not (eq system-type 'windows-nt))
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (global-treesit-auto-mode))

(use-package meson-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package clang-format
  :ensure t)

;; (use-package format-all
;;   :ensure t
;;   :bind ("C-c C-f" . format-all-buffer)
;;   :config
;;   (setq-default format-all-formatters
;;                 '(("Rust" (rustfmt "--edition" "2024"))
;;                   ("JavaScript" (prettier))
;;                   ("C++" (clang-format))))
;;   :hook
;;   (js2-mode-hook . format-all-mode)
;;   (c++-mode-hook . format-all-mode))

(use-package apheleia
  :ensure t
  :config (apheleia-global-mode +1))

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-map
              ("C-c C-f" . 'apheleia-format-buffer)
              :map c++-mode-map
              ("C-c C-f" . 'apheleia-format-buffer)))

(use-package gdscript-mode
  :ensure t)

(use-package display-line-numbers
  :ensure nil
  :hook
  ((text-mode-hook . display-line-numbers-mode)
   (prog-mode-hook . display-line-numbers-mode)
   (text-mode-hook . display-line-numbers-mode)))

;; (use-package highlight-indent-guides
;;  :ensure t
;;  :hook (prog-mode . highlight-indent-guides-mode)
;;  :custom
;;  (highlight-indent-guides-auto-character-face-perc 30)
;;  (highlight-indent-guides-auto-even-face-perc 30)
;;  (highlight-indent-guides-auto-odd-face-perc 25)
;;  (highlight-indent-guides-method 'character))  

(use-package magit
  :ensure t)
(global-set-key (kbd "<f5>") 'magit-status)

(use-package magit-todos
  :after magit
  :ensure t
  :config (magit-todos-mode 1))

(defun asm-indent ()
  "Customize asm-mode indentation for 65816 assembly."
  (defun asm-calculate-indentation ()
    (or
     (and (looking-at "[.@_[:word:]]+:") 0)
     (and (looking-at "\\s<\\s<\\s<") 0)
     (and (looking-at "\\.[a-zA-Z]+") 0)
     (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
     (or (car tab-stop-list) tab-width))))

(use-package asm-mode
  :hook (asm-mode . asm-indent))

(use-package lua-mode
  :ensure t
  :bind ("C-c C-f" . 'apheleia-format-buffer)
  :custom
  (format-all-formatters '(("Lua" (stylua)))))

(use-package texfrag
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :hook (markdown-mode-hook . texfrag-mode))

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
(use-package css-mode
  :ensure nil
  :bind (("C-c C-f" . apheleia-format-buffer)))

(use-package web-mode
  :ensure t)
(load-el "vue-mode.el")

(use-package emmet-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'")
  :interpreter "node"
  :config
  (setq-default tab-width 2)
  (setq-default standard-indent 4)
  (setq-default indent-tabs-mode nil)
  :bind
  (:map js2-mode-map ("C-c C-f" . apheleia-format-buffer)))

(use-package jsdoc
  :ensure t)

(use-package xclip
  :ensure t)

(use-package autothemer
  :ensure t)

(when (not (display-graphic-p))
  (require 'xclip)
  (xclip-mode 1))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-interval 5 nil nil "Customized with use-package files")
 '(custom-safe-themes
   '("2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7" default))
 '(doc-view-resolution 600)
 '(js-indent-level 2)
 '(markdown-enable-math t)
 '(package-selected-packages
   '(ace-window all-the-icons apheleia autothemer cfrs clang-format comment-tags
                company dashboard diminish dired-collapse dired-rainbow
                dracula-theme eldoc-box emmet-mode expand-region format-all
                gdscript-mode glsl-mode gruber-darker-theme
                highlight-indent-guides ht htmlize hydra iscroll ivy
                ivy-prescient js2-mode jsdoc json-mode kotlin-mode levenshtein
                ligature lua-mode magit markdown-mode meson-mode mini-frame
                multiple-cursors pfuture pinyinlib projectile rainbow-delimiters
                rainbow-mode rime rust-mode simple-httpd srfi texfrag trashed
                typescript-mode vlf vue3-mode wc-mode wgsl-mode xclip yaml-mode
                yasnippet-snippets))
 '(scheme-mit-dialect nil)
 '(scheme-program-name "env LD_LIBRARY_PATH=/usr/local/lib/ chibi-scheme")
 '(sql-product 'sqlite)
 '(texfrag-setup-alist
   '((texfrag-html html-mode) (texfrag-eww eww-mode) (texfrag-sx sx-question-mode)
     (texfrag-prog prog-mode) (texfrag-trac-wiki trac-wiki-mode)
     (texfrag-markdown markdown-mode) (texfrag-org org-mode)
     (texfrag-adoc adoc-mode) (markdown-mode)))
 '(whitespace-style
   '(face trailing tabs spaces newline missing-newline-at-eof empty indentation
          space-after-tab space-before-tab space-mark tab-mark)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
