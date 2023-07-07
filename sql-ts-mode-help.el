(require 'treesit)
(treesit-ready-p 'sql)
(treesit-parser-create 'sql)

(treesit-node-at (point))

(treesit-parent-until
 (treesit-node-at (point))
 (lambda (node) (equal (treesit-node-type node) "join_clause"))
 t)

(treesit-node-top-level
 (treesit-node-at (point))
 (lambda (node) (equal (treesit-node-type node) "select_statement"))
 t)


(treesit-query-capture (treesit-parser-root-node (car (treesit-parser-list))) '(string))
(treesit-parser-root-node (car (treesit-parser-list)))

(treesit-query-capture 'sql '((string) @font-lock-string-face))


(treesit-query-capture 'sql `(([,@sql-pg-ts-mode--keywords]) @font-lock-keyword-face))

(treesit-query-p `(([,@sql-pg-ts-mode--keywords]) @font-lock-keyword-face))


(treesit-query-validate 'sql `(([,@sql-pg-ts-mode--keywords]) @font-lock-keyword-face))

(treesit-font-lock-fontify-region (point-min) (point-max) t)
