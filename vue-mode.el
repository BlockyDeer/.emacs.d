;;; vue-mode.el --- Vue support via eglot + rassumfrassum -*- lexical-binding: t; -*-

;; @vue/language-server v3 no longer runs its own tsserver.  It expects
;; the client to broker its `tsserver/request' notifications to a
;; TypeScript server loaded with `@vue/typescript-plugin', which plain
;; eglot cannot do.  `rass vue' (see ~/.config/rassumfrassum/vue.py)
;; multiplexes vue-language-server with a shimmed raw tsserver,
;; brokers the notifications, and merges the responses for eglot.

(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(defun vue-mode-eglot-command ()
  "Return the `rass vue' command for eglot."
  (or (executable-find "rass")
      (expand-file-name "~/.local/venvs/rassumfrassum/bin/rass")))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(vue-mode . (,(vue-mode-eglot-command) "vue"))))

(add-hook 'vue-mode-hook #'eglot-ensure)

(provide 'vue-mode)
;;; vue-mode.el ends here
