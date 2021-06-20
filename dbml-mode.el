
(defconst dbml-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(defgroup dbml nil
  "Customization variables for dbml mode."
  :tag "dbml"
  :group 'languages)

(setq dbml-font-lock-keywords
      (let* ((x-types '("int" "integer" "char" "varchar" "bool" "boolean"
                        "string" "text" "timestamp" "date"))
             (x-fn-names  '("Table" "Ref" "ref" "Enum" "note"))
             (x-keywords '("not" "null" "pk" "primary key" "unique"))

             (x-types-regexp (regexp-opt x-types 'words))
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-fn-regexp (regexp-opt x-fn-names 'words)))

        `((,x-types-regexp . font-lock-type-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-fn-regexp . font-lock-function-name-face)
          (">" . font-lock-function-name-face))))

(define-derived-mode dbml-mode python-mode
  "dbml"
  :group 'dbml
  :syntax-table dbml-mode-syntax-table
  (setq font-lock-defaults '((dbml-font-lock-keywords)))
  (setq-local python-indent 2)
  (font-lock-fontify-buffer))

(add-to-list 'auto-mode-alist '("\\.dbd\\'" . dbml-mode))
(add-to-list 'auto-mode-alist '("\\.dbml\\'" . dbml-mode))

(provide 'dbml-mode)
