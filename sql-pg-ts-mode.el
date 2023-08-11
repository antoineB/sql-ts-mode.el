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

(defvar sql-pg-ts-mode-completion t "Add the buildin completion")

(defvar sql-pg-ts-mode-keywords
  '("ABSOLUTE"
    "ACTION"
    "ADA"
    "ADD"
    "ADMIN"
    "AFTER"
    "ALWAYS"
    "ASC"
    "ASSERTION"
    "ASSIGNMENT"
    "ATTRIBUTE"
    "ATTRIBUTES"
    "BEFORE"
    "BERNOULLI"
    "BREADTH"
    "CASCADE"
    "CATALOG"
    "CATALOG_NAME"
    "CHAIN"
    "CHARACTER_SET_CATALOG"
    "CHARACTER_SET_NAME"
    "CHARACTER_SET_SCHEMA"
    "CHARACTERISTICS"
    "CHARACTERS"
    "CLASS_ORIGIN"
    "COBOL"
    "COLLATION"
    "COLLATION_CATALOG"
    "COLLATION_NAME"
    "COLLATION_SCHEMA"
    "COLUMN_NAME"
    "COMMAND_FUNCTION"
    "COMMAND_FUNCTION_CODE"
    "COMMITTED"
    "CONDITION_NUMBER"
    "CONNECTION"
    "CONNECTION_NAME"
    "CONSTRAINT_CATALOG"
    "CONSTRAINT_NAME"
    "CONSTRAINT_SCHEMA"
    "CONSTRAINTS"
    "CONSTRUCTOR"
    "CONTINUE"
    "CURSOR_NAME"
    "DATA"
    "DATETIME_INTERVAL_CODE"
    "DATETIME_INTERVAL_PRECISION"
    "DEFAULTS"
    "DEFERRABLE"
    "DEFERRED"
    "DEFINED"
    "DEFINER"
    "DEGREE"
    "DEPTH"
    "DERIVED"
    "DESC"
    "DESCRIPTOR"
    "DIAGNOSTICS"
    "DISPATCH"
    "DOMAIN"
    "DYNAMIC_FUNCTION"
    "DYNAMIC_FUNCTION_CODE"
    "ENFORCED"
    "EXCLUDE"
    "EXCLUDING"
    "EXPRESSION"
    "FINAL"
    "FIRST"
    "FLAG"
    "FOLLOWING"
    "FORTRAN"
    "FOUND"
    "GENERAL"
    "GENERATED"
    "GO"
    "GOTO"
    "GRANTED"
    "HIERARCHY"
    "IGNORE"
    "IMMEDIATE"
    "IMMEDIATELY"
    "IMPLEMENTATION"
    "INCLUDING"
    "INCREMENT"
    "INITIALLY"
    "INPUT"
    "INSTANCE"
    "INSTANTIABLE"
    "INSTEAD"
    "INVOKER"
    "ISOLATION"
    "KEY"
    "KEY_MEMBER"
    "KEY_TYPE"
    "LAST"
    "LENGTH"
    "LEVEL"
    "LOCATOR"
    "MAP"
    "MATCHED"
    "MAXVALUE"
    "MESSAGE_LENGTH"
    "MESSAGE_OCTET_LENGTH"
    "MESSAGE_TEXT"
    "MINVALUE"
    "MORE"
    "MUMPS"
    "NAME"
    "NAMES"
    "NESTING"
    "NEXT"
    "NFC"
    "NFD"
    "NFKC"
    "NFKD"
    "NORMALIZED"
    "NULLABLE"
    "NULLS"
    "NUMBER"
    "OBJECT"
    "OCTETS"
    "OPTION"
    "OPTIONS"
    "ORDERING"
    "ORDINALITY"
    "OTHERS"
    "OUTPUT"
    "OVERRIDING"
    "PAD"
    "PARAMETER_MODE"
    "PARAMETER_NAME"
    "PARAMETER_ORDINAL_POSITION"
    "PARAMETER_SPECIFIC_CATALOG"
    "PARAMETER_SPECIFIC_NAME"
    "PARAMETER_SPECIFIC_SCHEMA"
    "PARTIAL"
    "PASCAL"
    "PATH"
    "PLACING"
    "PLI"
    "PRECEDING"
    "PRESERVE"
    "PRIOR"
    "PRIVILEGES"
    "PUBLIC"
    "READ"
    "RELATIVE"
    "REPEATABLE"
    "RESPECT"
    "RESTART"
    "RESTRICT"
    "RETURNED_CARDINALITY"
    "RETURNED_LENGTH"
    "RETURNED_OCTET_LENGTH"
    "RETURNED_SQLSTATE"
    "ROLE"
    "ROUTINE"
    "ROUTINE_CATALOG"
    "ROUTINE_NAME"
    "ROUTINE_SCHEMA"
    "ROW_COUNT"
    "SCALE"
    "SCHEMA"
    "SCHEMA_NAME"
    "SCOPE_CATALOG"
    "SCOPE_NAME"
    "SCOPE_SCHEMA"
    "SECTION"
    "SECURITY"
    "SELF"
    "SEQUENCE"
    "SERIALIZABLE"
    "SERVER_NAME"
    "SESSION"
    "SETS"
    "SIMPLE"
    "SIZE"
    "SOURCE"
    "SPACE"
    "SPECIFIC_NAME"
    "STATE"
    "STATEMENT"
    "STRUCTURE"
    "STYLE"
    "SUBCLASS_ORIGIN"
    "TABLE_NAME"
    "TEMPORARY"
    "TIES"
    "TOP_LEVEL_COUNT"
    "TRANSACTION"
    "TRANSACTION_ACTIVE"
    "TRANSACTIONS_COMMITTED"
    "TRANSACTIONS_ROLLED_BACK"
    "TRANSFORM"
    "TRANSFORMS"
    "TRIGGER_CATALOG"
    "TRIGGER_NAME"
    "TRIGGER_SCHEMA"
    "TYPE"
    "UNBOUNDED"
    "UNCOMMITTED"
    "UNDER"
    "UNNAMED"
    "USAGE"
    "USER_DEFINED_TYPE_CATALOG"
    "USER_DEFINED_TYPE_CODE"
    "USER_DEFINED_TYPE_NAME"
    "USER_DEFINED_TYPE_SCHEMA"
    "VIEW"
    "WORK"
    "WRITE"
    "ZONE"
    "ABS"
    "ALL"
    "ALLOCATE"
    "ALTER"
    "AND"
    "ANY"
    "ARE"
    "ARRAY"
    "ARRAY_AGG"
    "ARRAY_MAX_CARDINALITY"
    "AS"
    "ASENSITIVE"
    "ASYMMETRIC"
    "AT"
    "ATOMIC"
    "AUTHORIZATION"
    "AVG"
    "BEGIN"
    "BEGIN_FRAME"
    "BEGIN_PARTITION"
    "BETWEEN"
    "BIGINT"
    "BINARY"
    "BLOB"
    "BOOLEAN"
    "BOTH"
    "BY"
    "CALL"
    "CALLED"
    "CARDINALITY"
    "CASCADED"
    "CASE"
    "CAST"
    "CEIL"
    "CEILING"
    "CHAR"
    "CHAR_LENGTH"
    "CHARACTER"
    "CHARACTER_LENGTH"
    "CHECK"
    "CLOB"
    "CLOSE"
    "COALESCE"
    "COLLATE"
    "COLLECT"
    "COLUMN"
    "COMMIT"
    "CONDITION"
    "CONNECT"
    "CONSTRAINT"
    "CONTAINS"
    "CONVERT"
    "CORR"
    "CORRESPONDING"
    "COUNT"
    "COVAR_POP"
    "COVAR_SAMP"
    "CREATE"
    "CROSS"
    "CUBE"
    "CUME_DIST"
    "CURRENT"
    "CURRENT_CATALOG"
    "CURRENT_DATE"
    "CURRENT_DEFAULT_TRANSFORM_GROUP"
    "CURRENT_PATH"
    "CURRENT_ROLE"
    "CURRENT_ROW"
    "CURRENT_SCHEMA"
    "CURRENT_TIME"
    "CURRENT_TIMESTAMP"
    "CURRENT_TRANSFORM_GROUP_FOR_TYPE"
    "CURRENT_USER"
    "CURSOR"
    "CYCLE"
    "DATE"
    "DAY"
    "DEALLOCATE"
    "DEC"
    "DECIMAL"
    "DECLARE"
    "DEFAULT"
    "DELETE"
    "DENSE_RANK"
    "DEREF"
    "DESCRIBE"
    "DETERMINISTIC"
    "DISCONNECT"
    "DISTINCT"
    "DOUBLE"
    "DROP"
    "DYNAMIC"
    "EACH"
    "ELEMENT"
    "ELSE"
    "END"
    "END_FRAME"
    "END_PARTITION"
    "END-EXEC"
    "EQUALS"
    "ESCAPE"
    "EVERY"
    "EXCEPT"
    "EXEC"
    "EXECUTE"
    "EXISTS"
    "EXP"
    "EXTERNAL"
    "EXTRACT"
    "FALSE"
    "FETCH"
    "FILTER"
    "FIRST_VALUE"
    "FLOAT"
    "FLOOR"
    "FOR"
    "FOREIGN"
    "FRAME_ROW"
    "FREE"
    "FROM"
    "FULL"
    "FUNCTION"
    "FUSION"
    "GET"
    "GLOBAL"
    "GRANT"
    "GROUP"
    "GROUPING"
    "GROUPS"
    "HAVING"
    "HOLD"
    "HOUR"
    "IDENTITY"
    "IN"
    "INDICATOR"
    "INNER"
    "INOUT"
    "INSENSITIVE"
    "INSERT"
    "INT"
    "INTEGER"
    "INTERSECT"
    "INTERSECTION"
    "INTERVAL"
    "INTO"
    "IS"
    "JOIN"
    "LAG"
    "LANGUAGE"
    "LARGE"
    "LAST_VALUE"
    "LATERAL"
    "LEAD"
    "LEADING"
    "LEFT"
    "LIKE"
    "LIKE_REGEX"
    "LN"
    "LOCAL"
    "LOCALTIME"
    "LOCALTIMESTAMP"
    "LOWER"
    "MATCH"
    "MAX"
    "MEMBER"
    "MERGE"
    "METHOD"
    "MIN"
    "MINUTE"
    "MOD"
    "MODIFIES"
    "MODULE"
    "MONTH"
    "MULTISET"
    "NATIONAL"
    "NATURAL"
    "NCHAR"
    "NCLOB"
    "NEW"
    "NO"
    "NONE"
    "NORMALIZE"
    "NOT"
    "NTH_VALUE"
    "NTILE"
    "NULL"
    "NULLIF"
    "NUMERIC"
    "OCTET_LENGTH"
    "OCCURRENCES_REGEX"
    "OF"
    "OFFSET"
    "OLD"
    "ON"
    "ONLY"
    "OPEN"
    "OR"
    "ORDER"
    "OUT"
    "OUTER"
    "OVER"
    "OVERLAPS"
    "OVERLAY"
    "PARAMETER"
    "PARTITION"
    "PERCENT"
    "PERCENT_RANK"
    "PERCENTILE_CONT"
    "PERCENTILE_DISC"
    "PERIOD"
    "PORTION"
    "POSITION"
    "POSITION_REGEX"
    "POWER"
    "PRECEDES"
    "PRECISION"
    "PREPARE"
    "PRIMARY"
    "PROCEDURE"
    "RANGE"
    "RANK"
    "READS"
    "REAL"
    "RECURSIVE"
    "REF"
    "REFERENCES"
    "REFERENCING"
    "REGR_AVGX"
    "REGR_AVGY"
    "REGR_COUNT"
    "REGR_INTERCEPT"
    "REGR_R2"
    "REGR_SLOPE"
    "REGR_SXX"
    "REGR_SXY"
    "REGR_SYY"
    "RELEASE"
    "RESULT"
    "RETURN"
    "RETURNS"
    "REVOKE"
    "RIGHT"
    "ROLLBACK"
    "ROLLUP"
    "ROW"
    "ROW_NUMBER"
    "ROWS"
    "SAVEPOINT"
    "SCOPE"
    "SCROLL"
    "SEARCH"
    "SECOND"
    "SELECT"
    "SENSITIVE"
    "SESSION_USER"
    "SET"
    "SIMILAR"
    "SMALLINT"
    "SOME"
    "SPECIFIC"
    "SPECIFICTYPE"
    "SQL"
    "SQLEXCEPTION"
    "SQLSTATE"
    "SQLWARNING"
    "SQRT"
    "START"
    "STATIC"
    "STDDEV_POP"
    "STDDEV_SAMP"
    "SUBMULTISET"
    "SUBSTRING"
    "SUBSTRING_REGEX"
    "SUCCEEDS"
    "SUM"
    "SYMMETRIC"
    "SYSTEM"
    "SYSTEM_TIME"
    "SYSTEM_USER"
    "TABLE"
    "TABLESAMPLE"
    "THEN"
    "TIME"
    "TIMESTAMP"
    "TIMEZONE_HOUR"
    "TIMEZONE_MINUTE"
    "TO"
    "TRAILING"
    "TRANSLATE"
    "TRANSLATE_REGEX"
    "TRANSLATION"
    "TREAT"
    "TRIGGER"
    "TRUNCATE"
    "TRIM"
    "TRIM_ARRAY"
    "TRUE"
    "UESCAPE"
    "UNION"
    "UNIQUE"
    "UNKNOWN"
    "UNNEST"
    "UPDATE"
    "UPPER"
    "USER"
    "USING"
    "VALUE"
    "VALUES"
    "VALUE_OF"
    "VAR_POP"
    "VAR_SAMP"
    "VARBINARY"
    "VARCHAR"
    "VARYING"
    "VERSIONING"
    "WHEN"
    "WHENEVER"
    "WHERE"
    "WIDTH_BUCKET"
    "WINDOW"
    "WITH"
    "WITHIN"
    "WITHOUT"
    "YEAR")
  "SQL keywords")

(defvar sql-pg-ts-mode-ts-keywords
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
  "Treesitter Postgres SQL keywords node identifiers")

(defvar sql-pg-ts-mode-types
  '("int4range" "int8range" "numrange" "tsrange" "tstzrange" "daterange"
    "any" "anyarray" "anyelement" "anyenum" "anynonarray" "cstring" "internal"
    "language_handler" "fdw_handler" "record" "trigger" "void" "opaque"
    "bit" "bit()" "bit varying" "bit varying()" "bigint" "int8" "bigserial"
    "serial8" "boolean" "bool" "box" "bytea" "character" "character()"
    "char" "char()" "character varying" "character varying()" "varchar()"
    "varchar" "cidr" "circle" "date" "double precision" "float8" "inet"
    "integer" "int" "int4"
    ;; TODO interval [ fields ] [ (p) ]
    "json" "jsonb" "line" "lseg" "macaddr" "money" "numeric" "numeric(p,s)"
    "decimal" "decimal(p,s)" "path" "pg_lsn" "point" "polygon" "real" "float4"
    "smallint" "int2" "smallserial" "serial2" "serial" "serial4" "text" "time"
    "time without time zone" "time with time zone" "time(0) without time zone"
    "time(0) with time zone" "timestamp without time zone" "timestamp with time zone"
    "timestamp(0) without time zone" "timestamp(0) with time zone" "timetz"
    "timestamptz" "tsquery" "tsvector" "txid_snapshot" "uuid" "xml")
  "Postgres Types")

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
   `(([,@sql-pg-ts-mode-ts-keywords]) @font-lock-keyword-face
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


(defun sql-pg-ts-mode-default-schema ()
  "public")


(defun sql-pg-ts-mode-current-database-name ()
  "")


(defun sql-pg-ts-mode--extract-join-clause (node)
  (let* ((children (treesit-node-children node))
         (child (car children))
         (schema1 (sql-pg-ts-mode-default-schema))
         (name1 nil)
         (alias1 nil)
         (schema2 (sql-pg-ts-mode-default-schema))
         (name2 nil)
         (alias2 nil)         (result '()))
    (while children
      (let ((child (car children)))
        (pcase (treesit-node-type child)
          ("join_clause"
           (setq result (sql-pg-ts-mode--extract-join-clause child)))
          ("dotted_name"
           (let ((dotted (treesit-node-children child)))
             (if name1
                 (progn
                   (setq schema2 (treesit-node-text (car dotted) t))
                   (setq name2 (treesit-node-text (caddr dotted) t)))
               (setq schema1 (treesit-node-text (car dotted) t))
               (setq name1 (treesit-node-text (caddr dotted) t)))))
          ("identifier"
           (if name1
               (setq name2 (treesit-node-text child t))
             (setq name1 (treesit-node-text child t))))
          ("alias"
           (if name2
               (setq alias2 (treesit-node-text child t))
             (setq alias1 (treesit-node-text child t))))))
      (setq children (cdr children)))
    (if name2
        (cons (list schema1 name1 alias1)
              (cons (list schema2 name2 alias2)
                    result))
        (cons (list schema1 name1 alias1) result))))
    

(defun sql-pg-ts-mode--extract-from (node)
  "extract info from the from node in the form of '((schema table-name alias-or-nil) ...)"
  ;; skip FROM node
  (let ((children (cdr (treesit-node-children node)))
        (result '()))
    (while children
      (let ((child (car children)))
        (pcase (treesit-node-type child)
          ("dotted_name"
           (let ((dotted (treesit-node-children child))
                 (schema nil)
                 (name nil)
                 (alias nil))
             (setq schema (treesit-node-text (car dotted) t))
             (setq name (treesit-node-text (caddr dotted) t))
             (setq children (cdr children))
             (when (and (equal (treesit-node-type (car children)) "AS")
                        (cadr children))
               (setq alias (treesit-node-text (cadr children) t))
               (setq children (cddr children)))
             (setq result (cons (list schema name alias) result))))
          ("identifier"
           (let ((name (treesit-node-text child t))
                 (alias nil))
             (setq children (cdr children))
             (when (and (equal (treesit-node-type (car children)) "AS")
                        (cadr children))
               (setq alias (treesit-node-text (cadr children) t))
               (setq children (cddr children)))
           (setq result (cons
                         (list (sql-pg-ts-mode-default-schema) name alias)
                         result))))
          ("join_clause"
           (setq result (append result
                                (sql-pg-ts-mode--extract-join-clause child)))
           (setq children (cdr children)))
          (_ (setq children (cdr children))))))
    result))


(defun sql-pg-ts-mode--extract-select (node) 
 nil)


(defvar sql-pg-ts-mode--datastore nil)


;; Database format :
;; (hashtable
;;  database_name -> (hashtable
;;                    (schema_name -> (alist
;;                                    ('tables .
;;                                             (hashtable string -> (alist ('name . string)
;;                                                                         ('position . filename offset-start offset-end)
;;                                                                         ('comment . string)
;;                                                                         ('columns . (hashtable string -> (alist ('name . string)
;;                                                                                                                 ('position . filename offset-start offset-end)
;;                                                                                                                 ('comment . string)
;;                                                                                                                 ('type . string)))))))
;;                                    ('functions . (hashtable string -> (alist ('name . string) ('position . filename offset-start offset-end)))))))))

(defun sql-pg-ts-mode-make-database (database-name)
  (let ((db (make-hash-table :test #'equal))
        (default-schema-db (make-hash-table :test #'equal))
        (functions-db (make-hash-table :test #'equal)))
    (puthash "array_agg" '((name . "array_agg")) functions-db)
    (puthash
     (sql-pg-ts-mode-default-schema)
     `((tables . ,(make-hash-table :test #'equal))
       (functions . ,functions-db))
     default-schema-db)
    (puthash database-name default-schema-db db)
    db))

;; TODO: where to put extensions ?
;; TODO: where to put default functions ? (does functions name resolution change if we change the default schema name ?)
;; TODO: does functions are schema dependant ?

(defun sql-pg-ts-mode--extract-db-from-schema (buffer)
  "Extract the database definition from schema buffer."
  (let* ((top-level-node (treesit-node-top-level
                         (treesit-node-at (point-min))
                         (lambda (x) (equal (treesit-node-type x) "source_file"))))
         (tables (make-hash-table :test #'equal))
         (children (treesit-node-children top-level-node)))
    (while children
      (let ((child (car children)))
        (setq children (cdr children))
        (pcase (treesit-node-type child)
          ("create_table_statement"
           (let ((table-store '())
                 (children2 (treesit-node-children child)))
             (while children2
               (let ((n (car children2)))
                 (setq children2 (cdr children2))
                 (pcase (treesit-node-type n)
                   ("identifier" (setq table-store (cons `(name . ,(treesit-node-text n t)) table-store)))
                   ("table_parameters"
                    (let ((children3 (treesit-node-children n))
                          (columns (make-hash-table :test #'equal)))
                      (while children3
                        (let ((n2 (car children3)))
                          (setq children3 (cdr children3))
                          (when (equal (treesit-node-type n2) "table_column")
                            (puthash (treesit-node-text (treesit-node-child-by-field-name n2 "name") t)
                                     `((name . ,(treesit-node-text (treesit-node-child-by-field-name n2 "name") t))
                                       (type . ,(treesit-node-text (treesit-node-child-by-field-name n2 "type") t)))
                                     columns))))
                      (setq table-store (cons `(columns . ,columns) table-store)))))))
             (puthash (cdr (assoc 'name table-store)) table-store tables)))
          ;; TODO: the create_extension_statement type because it can add functions (ajouter un hook avec les infos (database))
          ;; TODO: comment_statement usefull data to show to the user
          ;; TODO: create_function_statement
          )))
    tables))


(defun sql-pg-ts-mode--complete-tables (candidate-node)
  (when sql-pg-ts-mode--datastore
    (when-let* ((db (gethash (sql-pg-ts-mode-current-database-name)
                       sql-pg-ts-mode--datastore))
                (schema (if (equal (treesit-node-type candidate-node) "dotted_name")
                            (treesit-node-text (treesit-node-child candidate-node 0) t)
                          (sql-pg-ts-mode-default-schema)))
                (scheme-db (gethash schema db)))
      (list
       (treesit-node-start candidate-node)
       (treesit-node-end candidate-node)
       (cdr (assoc 'tables scheme-db))))))


(defun sql-pg-ts-mode--complete-identifiers (candidate-node start end)
  (let* ((select-node (treesit-parent-until
                       candidate-node
                       (lambda (node) (equal (treesit-node-type node) "select_statement"))))
         (from-node (treesit-search-subtree
                     select-node
                     (lambda (node) (equal (treesit-node-type node) "from_clause"))))
         (defined-tables (when from-node (sql-pg-ts-mode--extract-from from-node))))
      (list
       start
       end
       (when (and defined-tables sql-pg-ts-mode--datastore)
           (when-let ((db (gethash (sql-pg-ts-mode-current-database-name)
                                   sql-pg-ts-mode--datastore)))
             (cl-loop
              for item in defined-tables
              append (when-let* ((schema-db (gethash (car item) db))
                                  (table-name (cadr item))
                                  (table-db (gethash table-name (cdr (assoc 'tables schema-db)))))
                        (let ((alias (when (caddr item) (concat (caddr item) "."))))
                          (append
                           (if alias (list alias table-name) (list table-name))
                           (cl-loop
                            for column in (hash-table-keys (cdr (assoc 'columns table-db)))
                            append (list
                                     (if alias (concat alias column) column)
                                     column
                                     (concat table-name "." column))))))))))))

(defun sql-pg-ts-mode--treesit-node-prev (node)
  (let ((prev (treesit-node-prev-sibling node))
        (parent node))
      (while (and (not prev) parent)
        (setq parent (treesit-node-parent parent))
        (setq prev (treesit-node-prev-sibling parent)))
      prev))
       

;; TODO: The completion doesn't take into account the case.
;;
;; Solutions look at the first keyword (SELECT, INSERT, UPDATE ...) to know
;; the case of the user style and then upcase/lowercase all the completions

(defun sql-pg-ts-mode-completion-at-point ()
  (let ((node (treesit-node-at (point))))
    (pcase (treesit-node-type node)
      ("::" (list (treesit-node-end node)
                  (treesit-node-end node)
                  sql-pg-ts-mode-types))
      ("ERROR" (list (treesit-node-start node)
                     (treesit-node-end node)
                     sql-pg-ts-mode-keywords))
      ("."
       (let ((select-node (treesit-parent-until
                           node
                           (lambda (node) (equal (treesit-node-type node) "select_statement")))))
         (when select-node
           ;; TODO: si on ne trouve pas le from_clause
           ;; (let* ((text-start (buffer-substring-no-properties (treesit-node-start select-node) (treesit-node-start prev-node)))
           ;;        (text-end (buffer-substring-no-properties (treesit-node-end prev-node) (treesit-node-end select-node)))
           ;;        (new-node (treesit-parse-string (concat text-start " " text-end) 'sql)))
           (let ((prev-node (treesit-node-prev-sibling node)))
             (when (equal (treesit-node-type prev-node) "identifier")
               (sql-pg-ts-mode--complete-identifiers node (treesit-node-start prev-node) (treesit-node-end node)))))))
      ("identifier"
       ;; TODO: Do only the select case
       (let ((select-node (treesit-parent-until
                           node
                           (lambda (node) (equal (treesit-node-type node) "select_statement")))))
         (when select-node
           (let* ((parent-node (treesit-node-parent node))
                  (candidate-node (if (equal (treesit-node-type parent-node) "dotted_name") parent-node node))
                  (prev-node (sql-pg-ts-mode--treesit-node-prev candidate-node)))
             (pcase (treesit-node-type prev-node)
               ("::" (list (treesit-node-start node)
                           (treesit-node-end node)
                           sql-pg-ts-mode-types))
               ("AS" nil)
               ("," (if (treesit-parent-until
                         node
                         (lambda (node) (let ((type (treesit-node-type node)))
                                          (or (equal type "function_call") (equal type "select_clause")))))
                        (sql-pg-ts-mode--complete-identifiers candidate-node (treesit-node-start candidate-node) (treesit-node-end candidate-node))
                      (sql-pg-ts-mode--complete-tables candidate-node)))
               ((or "FROM" "JOIN")
                (sql-pg-ts-mode--complete-tables candidate-node))
               (_ (sql-pg-ts-mode--complete-identifiers candidate-node (treesit-node-start candidate-node) (treesit-node-end candidate-node)))))))))))


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

  ;; Completion
  (when sql-pg-ts-mode-completion
    (setq-local completion-ignore-case t)
    (setq-local completion-at-point-functions '(sql-pg-ts-mode-completion-at-point)))

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
