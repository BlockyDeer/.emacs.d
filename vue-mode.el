;; web-mode setup
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(defun vue-eglot-init-options ()
  (let ((tsdk-path (expand-file-name
                    "lib"
                    (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
    `(:typescript (:tsdk ,tsdk-path
                         :languageFeatures (:completion
                                            (:defaultTagNameCase "both"
                                                                 :defaultAttrNameCase "kebabCase"
                                                                 :getDocumentNameCasesRequest nil
                                                                 :getDocumentSelectionRequest nil)
                                            :diagnostics
                                            (:getDocumentVersionRequest nil))
                         :documentFeatures (:documentFormatting
                                            (:defaultPrintWidth 100
                                                                :getDocumentPrintWidthRequest nil)
                                            :documentSymbol t
                                            :documentColor t)))))

;; Volar
(add-to-list 'eglot-server-programs
             `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
