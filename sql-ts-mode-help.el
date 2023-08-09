(require 'treesit)
(treesit-ready-p 'sql)
(treesit-parser-create 'sql)

;; aller sur schema.sql
(setq test-data (sql-pg-ts-mode--extract-db-from-schema (current-buffer)))
(setq sql-pg-ts-mode--datastore
      (let* ((db (sql-pg-ts-mode-make-database "test"))
             (schema-db (gethash "test" db))
             (schema (gethash "public" schema-db)))
        (puthash "public"
                 `((tables . ,test-data)
                   (functions . ,(cdr (assoc 'functions schema))))
                 schema-db)
        db))
(defun sql-pg-ts-mode-current-database-name ()
  "test")


;; (49 52 (("password_recovery_token" ("used_at" "used_at" "password_recovery_token.used_at") ("created_at" "created_at" "password_recovery_token.created_at") ("token" "token" "password_recovery_token.token") ("profile_id" "profile_id" "password_recovery_token.profile_id"))))


(treesit-node-at (point))

(treesit-parent-until
 (treesit-node-at (point))
 (lambda (node) (equal (treesit-node-type node) "create_table_statement"))
 t)

(pcase toto
  ( sym))

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
