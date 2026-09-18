;;; scheme-ts-mode.el --- Tree-sitter based major mode for Scheme -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)
(require 'scheme)

(eval-when-compile
  (require 'cl-lib))

(defgroup scheme-ts nil
  "Major mode for editing Scheme files with tree-sitter."
  :prefix "scheme-ts-"
  :group 'languages)

(defcustom scheme-ts-mode--grammar-dir
  (expand-file-name "tree-sitter" user-emacs-directory)
  "Directory to search for the tree-sitter Scheme grammar library."
  :type 'directory
  :group 'scheme-ts)

;; The syntax highlighting below only relies on node types and fields
;; that exist in the thchha/tree-sitter-scheme grammar.  Refer to
;; grammar.js of that grammar for the authoritative node inventory.

(defvar scheme-ts-mode--keywords
  ;; Core R7RS syntactic keywords, as recognised by the grammar.  These
  ;; show up as anonymous tokens within the corresponding constructs
  ;; (e.g. "if" inside `if_conditional'), so quoting them here is safe:
  ;; quoted data is parsed as `symbol' nodes, never as these tokens.
  '("and" "begin" "case" "case-lambda" "cond" "cond-expand"
    "define" "define-library" "define-record-type" "define-syntax"
    "define-values" "delay" "delay-force" "do" "else" "export"
    "guard" "if" "import" "include" "include-ci" "lambda" "let"
    "let*" "let-syntax" "let-values" "let-values*" "letrec"
    "letrec*" "letrec-syntax" "or" "parameterize" "quasiquote"
    "set!" "syntax-rules" "unless" "unquote" "unquote-splicing"
    "when"))

(defvar scheme-ts-mode--feature-list
  '((comment keyword)
    (string character number literal)
    (definition function variable boolean bracket))
  "`treesit-font-lock-feature-list' for `scheme-ts-mode'.")

(defvar scheme-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'scheme
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'scheme
   :feature 'keyword
   `([,@scheme-ts-mode--keywords] @font-lock-keyword-face)

   :language 'scheme
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'scheme
   :feature 'character
   '((character) @font-lock-constant-face)

   :language 'scheme
   :feature 'literal
   '((boolean) @font-lock-constant-face
     (keyword) @font-lock-keyword-face     ; gambit #!key #!optional #!rest
     (symbol) @font-lock-constant-face)    ; quoted datum

   :language 'scheme
   :feature 'number
   '((infnan) @font-lock-constant-face
     (decimal_number) @font-lock-constant-face
     (binary_number) @font-lock-constant-face
     (octal_number) @font-lock-constant-face
     (hexadecimal_number) @font-lock-constant-face)

   :language 'scheme
   :feature 'definition
   '((binding_variable name: (identifier) @font-lock-variable-name-face)
     (binding_procedure name: (identifier) @font-lock-function-name-face)
     (binding_syntax name: (identifier) @font-lock-function-name-face)
     (binding_record type: (identifier) @font-lock-type-face)
     (binding_record predicate: (identifier) @font-lock-function-name-face)
     (constructor name: (identifier) @font-lock-function-name-face)
     (field name: (identifier) @font-lock-variable-name-face)
     (field accessor: (identifier) @font-lock-function-name-face))

   :language 'scheme
   :feature 'function
   '((procedure_call name: (identifier) @font-lock-function-call-face))

   :language 'scheme
   :feature 'variable
   '((lambda (arguments (identifier) @font-lock-variable-name-face))
     (binding_procedure (arguments (identifier) @font-lock-variable-name-face))
     (binding_let (binding (identifier) @font-lock-variable-name-face))
     (binding_let label: (identifier) @font-lock-function-name-face)
     (iteration_step var: (identifier) @font-lock-variable-name-face)
     (multi_bindings formal: (identifier) @font-lock-variable-name-face)
     (guard value: (identifier) @font-lock-variable-name-face))

   :language 'scheme
   :feature 'boolean
   '((boolean) @font-lock-constant-face)

   :language 'scheme
   :feature 'bracket
   '((["(" ")" "[" "]"]) @font-lock-bracket-face))
  "Tree-sitter font-lock settings for `scheme-ts-mode'.")

(defun scheme-ts-mode--defun-name (node)
  "Return the name of the defun NODE as a string, or nil."
  (pcase (treesit-node-type node)
    ((or "binding_procedure" "binding_syntax"
         "binding_variable" "library")
     (let ((child (treesit-node-child-by-field-name node "name")))
       (and child (treesit-node-text child t))))
    ("binding_record"
     (let ((child (treesit-node-child-by-field-name node "type")))
       (and child (treesit-node-text child t))))
    (_ nil)))

;;;###autoload
(define-derived-mode scheme-ts-mode scheme-mode "Scheme"
  "Major mode for editing Scheme files, powered by tree-sitter.
Inherits indentation, sexp navigation and inferior-scheme commands
from `scheme-mode'; provides tree-sitter based syntax highlighting,
imenu, defun navigation and which-func."
  :group 'scheme-ts

  (add-to-list 'treesit-extra-load-path scheme-ts-mode--grammar-dir)

  (when (treesit-ready-p 'scheme)
    (treesit-parser-create 'scheme)

    (setq-local treesit-font-lock-settings
                scheme-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                scheme-ts-mode--feature-list)

    (setq-local treesit-defun-name-function
                #'scheme-ts-mode--defun-name)
    (setq-local treesit-defun-type-regexp
                (rx bos
                    (or "binding_procedure"
                        "binding_variable"
                        "binding_syntax"
                        "binding_values"
                        "binding_record"
                        "library")
                    eos))
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`binding_procedure\\'" nil nil)
                  ("Syntax" "\\`binding_syntax\\'" nil nil)
                  ("Record" "\\`binding_record\\'" nil nil)
                  ("Variable" "\\`binding_variable\\'" nil nil)
                  ("Library" "\\`library\\'" nil nil)))

    (treesit-major-mode-setup)

    ;; Prefer jumping between top-level definitions, like the classic
    ;; sexp based `beginning-of-defun' behaviour.
    (setq-local treesit-defun-prefer-top-level t)))

(provide 'scheme-ts-mode)
;;; scheme-ts-mode.el ends here
