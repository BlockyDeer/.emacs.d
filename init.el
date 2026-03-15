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
  (kept-new-versions 3)
  (kept-old-version 1)
  (version-control t)
  (auto-save-visited-interval 1))

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

(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode t))

(use-package whitespace
  :ensure nil
  :config
  (global-whitespace-mode 1))


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
  :bind ("C-c C-f" . format-all-buffer)
  :config
  (setq-default format-all-formatters
                '(("Rust" (rustfmt "--edition" "2024")))))


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

;; (use-package highlight-indent-guides
;;  :ensure t
;;  :hook (prog-mode . highlight-indent-guides-mode)
;;  :custom
;;  (highlight-indent-guides-auto-character-face-perc 30)
;;  (highlight-indent-guides-auto-even-face-perc 30)
;;  (highlight-indent-guides-auto-odd-face-perc 25)
;;  (highlight-indent-guides-method 'character))  

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
  :mode "\\.md\\'")

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
(use-package emmet-mode
  :ensure t)
(use-package js2-mode
  :ensure t
  :config
  (setq-default tab-width 2)
  (setq-default standard-indent 4)
  (setq-default indent-tabs-mode nil))
(use-package jsdoc
  :ensure t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7"
     default))
 '(format-all-default-formatters
   '(("Assembly" asmfmt) ("ATS" atsfmt) ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex) ("C" clang-format) ("C#" csharpier)
     ("C++" clang-format) ("Cabal Config" cabal-fmt)
     ("Clojure" zprint) ("CMake" cmake-format) ("Crystal" crystal)
     ("CSS" prettier) ("Cuda" clang-format) ("D" dfmt)
     ("Dart" dart-format) ("Dhall" dhall) ("Dockerfile" dockfmt)
     ("Elixir" mix-format) ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp) ("Erlang" efmt) ("F#" fantomas)
     ("Fish" fish-indent) ("Fortran Free Form" fprettify)
     ("GLSL" clang-format) ("Go" gofmt) ("GraphQL" prettier)
     ("Haskell" brittany) ("HCL" hclfmt) ("HLSL" clang-format)
     ("HTML" html-tidy) ("HTML+EEX" mix-format)
     ("HTML+ERB" erb-format) ("Hy" emacs-hy)
     ("Java" google-java-format) ("JavaScript" prettier)
     ("JSON" prettier) ("JSON5" prettier) ("Jsonnet" jsonnetfmt)
     ("JSX" prettier) ("Kotlin" ktlint) ("LaTeX" latexindent)
     ("Less" prettier) ("Literate Haskell" brittany) ("Lua" lua-fmt)
     ("Markdown" prettier) ("Meson" muon-fmt) ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format) ("OCaml" ocp-indent)
     ("Perl" perltidy) ("PHP" prettier)
     ("Protocol Buffer" clang-format) ("PureScript" purty)
     ("Python" black) ("R" styler) ("Reason" bsrefmt)
     ("ReScript" rescript) ("Ruby" rufo)
     ("Rust" (rustfmt "--edition 2024" "--style-edition 2024"))
     ("Scala" scalafmt) ("SCSS" prettier) ("Shell" shfmt)
     ("Solidity" prettier) ("SQL" sqlformat) ("Svelte" prettier)
     ("Swift" swiftformat) ("Terraform" terraform-fmt)
     ("TOML" prettier) ("TSX" prettier) ("TypeScript" prettier)
     ("V" v-fmt) ("Verilog" istyle-verilog) ("Vue" prettier)
     ("XML" html-tidy) ("YAML" prettier) ("Zig" zig)
     ("_Angular" prettier) ("_AZSL" clang-format)
     ("_Beancount" bean-format) ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier) ("_Gleam" gleam) ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt) ("_Snakemake" snakefmt)))
 '(js-indent-level 2)
 '(markdown-enable-math t)
 '(package-selected-packages
   '(all-the-icons autothemer clang-format cmake-ide cmake-mode
                   comment-tags company dashboard diminish dirvish
                   dracula-theme eldoc-box emmet-mode format-all
                   gdscript-mode glsl-mode gruber-darker-theme
                   highlight-indent-guides impatient-mode ivy js2-mode
                   jsdoc json-mode ligature lua-mode magit
                   markdown-mode meson-mode mini-frame
                   multiple-cursors rainbow-delimiters rainbow-mode
                   rime rust-mode slime treemacs treesit-auto vlf
                   vue-mode vue3-mode wc-mode wgsl-mode xclip
                   yaml-mode yasnippet yasnippet-snippets))
 '(whitespace-style
   '(face trailing tabs spaces newline missing-newline-at-eof empty
          indentation space-after-tab space-before-tab space-mark
          tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
