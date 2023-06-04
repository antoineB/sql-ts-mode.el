;;; sql-pg-ts-mode.el --- tree-sitter support for SQL  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Antoine

;; Author: Antoine <antoine597@gmail.com>
;; Keywords: languages sql tree-sitter

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on https://github.com/m-novikov/tree-sitter-sql grammar.

;;; Code:

(require 'treesit)


(defvar sql-pg-ts-mode--keywords
  '("ADD"
    "AFTER"
    "ALL"
    "ALTER"
    "ALTER_COLUMN"
    "AND"
    "AS"
    "ASC"
    "ATOMIC"
    "AT_TIME_ZONE"
    "AUTO_INCREMENT"
    "BEFORE"
    "BEGIN"
    "BETWEEN"
    "BY"
    "CACHE"
    "CALLED_ON_NULL_INPUT"
    "CASCADE"
    "CASCADED"
    "CASE"
    "CHECK"
    "CHECK_OPTION"
    "COLUMN"
    "COMMENT_ON"
    "COMMIT"
    "CONCURRENTLY"
    "CONSTRAINT"
    "CONTAINS_SQL"
    "COST"
    "CREATE"
    "CREATE_DOMAIN"
    "CREATE_EXTENSION"
    "CREATE_MATERIALIZED_VIEW"
    "CREATE_OR_REPLACE_FUNCTION"
    "CREATE_OR_REPLACE_PROCEDURE"
    "CREATE_ROLE"
    "CREATE_SCHEMA"
    "CREATE_TYPE"
    "CUBE"
    "CURRENT_ROLE"
    "CURRENT_ROW"
    "CURRENT_USER"
    "DATA"
    "DATABASE"
    "DECLARE"
    "DEFAULT"
    "DEFERRABLE"
    "DEFERRED"
    "DEFINER"
    "DELETE"
    "DESC"
    "DETERMINISTIC"
    "DISTINCT"
    "DISTINCT_FROM"
    "DROP"
    "EACH"
    "ELSE"
    "END"
    "ENUM"
    "EXCLUDE"
    "EXCLUDE_CURRENT_ROW"
    "EXCLUDE_GROUP"
    "EXCLUDE_NO_OTHERS"
    "EXCLUDE_TIES"
    "EXECUTE"
    "EXISTS"
    "EXTENSION"
    "EXTERNAL"
    "FALSE"
    "FETCH"
    "FILTER"
    "FIRST"
    "FOLLOWING"
    "FOLLOWS"
    "FOR"
    "FOREIGN_KEY"
    "FROM"
    "FULL"
    "FUNCTION"
    "GRANT"
    "GROUP"
    "GROUP_BY"
    "GROUPING_SETS"
    "GROUPS"
    "HAVING"
    "IF"
    "IF_EXISTS"
    "IF_NOT_EXISTS"
    "IMMEDIATE"
    "IMMUTABLE"
    "IN"
    "INCLUDE"
    "INCREMENT"
    "INDEX"
    "INITIALLY"
    "INNER"
    "INOUT"
    "INSERT"
    "INSTEAD_OF"
    ;; "INTERVAL" see `sql-pg-ts-mode--fontify-interval-keyword'
    "INTO"
    "INVOKER"
    "IS"
    "JOIN"
    "LANGUAGE"
    "LAST"
    "LATERAL"
    "LEAKPROOF"
    "LEFT"
    "LIMIT"
    "LOCAL"
    "MATERIALIZED"
    "MATERIALIZED_VIEW"
    "MAXVALUE"
    "MINVALUE"
    "MODIFIES_SQL_DATA"
    "NATURAL"
    "NEW"
    "NEXT"
    "NO"
    "NO_SQL"
    "NOT"
    "NOT_DEFERRABLE"
    "NULL"
    "NULLS"
    "OF"
    "OFFSET"
    "OLD"
    "ON"
    "ON_DELETE"
    "ONLY"
    "ON_UPDATE"
    "OR"
    "ORDER_BY"
    "OR_REPLACE"
    "OUT"
    "OUTER"
    "OVER"
    "OWNED_BY"
    "OWNER_TO"
    "PARALLEL"
    "PARTITION_BY"
    "PRECEDES"
    "PRECEDING"
    "PRECISION"
    "PRIMARY_KEY"
    "PRIVILEGES"
    "PROCEDURE"
    "PUBLIC"
    "RANGE"
    "READS_SQL_DATA"
    "RECURSIVE"
    "REFERENCES"
    "REFERENCING"
    "RENAME"
    "RENAME_TO"
    "REPEATABLE"
    "RESTRICT"
    "RESTRICTED"
    "RETURN"
    "RETURNS"
    "RETURNS_NULL_ON_NULL_INPUT"
    "RIGHT"
    "ROLLBACK"
    "ROLLUP"
    "ROW"
    "ROWS"
    "ROWS_FROM"
    "SAFE"
    "SCHEMA"
    "SECURITY_DEFINER"
    "SECURITY_INVOKER"
    "SELECT"
    "SEQUENCE"
    "SESSION"
    "SESSION_USER"
    "SET"
    "SET_DEFAULT"
    "SET_NULL"
    "SETOF"
    "SQL_SECURITY"
    "STABLE"
    "START"
    "STATEMENT"
    "STRICT"
    "SUPPORT"
    "TABLE"
    "TABLESAMPLE"
    "TABLESPACE"
    "TEMP"
    "TEMPORARY"
    "THEN"
    "TIME_ZONE"
    "TO"
    "TRANSACTION"
    "TRANSFORM_FOR_TYPE"
    "TRIGGER"
    "TRUE"
    "TRUNCATE"
    "UNBOUNDED_FOLLOWING"
    "UNBOUNDED_PRECEDING"
    "UNIQUE"
    "UNSAFE"
    "UPDATE"
    "USAGE"
    "USING"
    "VALUES"
    "VARIABLE"
    "VARIADIC"
    "VARYING"
    "VERSION"
    "VIEW"
    "VOLATILE"
    "WHEN"
    "WHERE"
    "WINDOW"
    "WITH"
    "WITH_GRANT_OPTION"
    "WITHIN_GROUP"
    "WITH_ORDINALITY"
    "WITHOUT"
    "WITHOUT_OIDS"
    "WORK")
  "Postgres SQL keywords")


(defun sql-pg-ts-mode--fontify-interval-keyword (node override start end &rest _)
  "Manualy fontify the keyword INTERVAL."
  (let ((node-start (treesit-node-start node)))
    (put-text-property node-start (+ node-start 8) 'face 'font-lock-keyword-face)
    (put-text-property node-start (treesit-node-end node) 'fontified t)))


(defvar sql-pg-ts-mode--operators
  '("->" "->>" "#>" "#>>" "::" "+" "-"
    "!!" "~" "@" "|" "#" "*" "/" "%" "<<"
    ">>" "&" "&&" "||" "<" "<=" "<>" "!="
    "=" ">" ">=" "~" "!~" "~*" "!~*")
  "Postgres SQL operators")


;; delimiter puntuaction
(defvar sql-pg-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'sql
   :override t
   :feature 'string
   '((string) @font-lock-string-face)
   :language 'sql
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face)
   :language 'sql
   :override t
   :feature 'delimiter
   '(([";" ","]) @font-lock-delimiter-face)
   :language 'sql
   :override t
   :feature 'keyword
   `(([,@sql-pg-ts-mode--keywords]) @font-lock-keyword-face
     (interval_expression) @sql-pg-ts-mode--fontify-interval-keyword)
   :language 'sql
   :override t
   :feature 'operator
   `(([,@sql-pg-ts-mode--operators]) @font-lock-operator-face)
   :language 'sql
   :override t
   :feature 'number
   '((number) @font-lock-number-face)
   :language 'sql
   :override t
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)
   :language 'sql
   :override t
   :feature 'type
   '((type) @font-lock-type-face)
   :language 'sql
   :override t
   :feature 'punctuation
   ;; TODO: this is not recognized
   '((["."]) @font-lock-punctuation-face))
  "Tree-sitter font-lock settings for `sql-pg-ts-mode'.")


(defvar sql-pg-ts-mode--statement-nodes
  '("begin_statement"
    "commit_statement"
    "rollback_statement"
    "select_statement"
    "update_statement"
    "insert_statement"
    "delete_statement"
    "set_statement"
    "grant_statement"
    "drop_statement"
    "create_statement"
    "alter_statement"
    "truncate_statement"
    "create_type_statement"
    "create_domain_statement"
    "create_index_statement"
    "create_table_statement"
    "create_schema_statement"
    "create_role_statement"
    "create_extension_statement"
    "create_trigger_statement"
    "create_function_statement"
    "comment_statement"
    "create_view_statement"
    "create_materialized_view_statement")
  "Treesitter parser's statement node names")


(defvar sql-pg-ts-mode--clause-nodes
  '("create_index_include_clause"
    "create_index_with_clause"
    "cube_clause"
    "default_clause"
    "fetch_clause"
    "filter_clause"
    "frame_clause"
    "from_clause"
    "group_by_clause"
    "grouping_sets_clause"
    "having_clause"
    "join_clause"
    "limit_clause"
    "offset_clause"
    "order_by_clause"
    "over_clause"
    "partition_by_clause"
    "repeatable_clause"
    "rollup_clause"
    "select_clause"
    "set_clause"
    "tablesample_clause"
    "using_clause"
    "values_clause"
    "where_clause"
    "window_clause"
    "with_clause"
    "within_group_clause")
  "Treesitter parser's clause node names")


;;;###autoload
(define-derived-mode sql-pg-ts-mode sql-mode "SQL"
  "Major mode for editing postgres SQL, powered by tree-sitter."
  :group 'sql

  (cond
   ((treesit-ready-p 'sql) (treesit-parser-create 'sql))
   (t (error "Tree-sitter for SQL isn't available")))

  (setq-local treesit-text-type-regexp
              (regexp-opt '("comment"
                            "string")))

  ;; Indent.
  ;; TODO:
  ;; (setq-local treesit-simple-indent-rules sql-pg-ts-mode--indent-rules)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (regexp-opt (cons "pg_command"
                                sql-pg-ts-mode--statement-nodes)))

  (setq-local treesit-sentence-type-regexp treesit-defun-type-regexp)

  (setq-local treesit-sexp-type-regexp
              (regexp-opt sql-pg-ts-mode--clause-nodes))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings sql-pg-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment )
                ( keyword string type )
                ( bracket delimiter operator)
                ( punctuation )))

  (sql-set-product 'postgres)
  (treesit-major-mode-setup))


;; TODO: The fontification of the derived mode still apply and give at leat one
;; error:
;;
;; CREATE TABLE table_name() ...
;;
;; table_name is fontified as a function name.


(provide 'sql-pg-ts-mode)
;;; sql-pg-ts-mode.el ends here
