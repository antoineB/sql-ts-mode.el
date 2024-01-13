;;; sql-ts-mode.el --- treesitter mode for sql       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  antoine

;; Author: antoine <antoine@antoine-AB350>
;; Keywords:

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

;;

;;; Code:

;; TODO: les cte RECURSIVE ne sont pas bien parsé

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defvar sql-ts-mode--keywords
  '(keyword_select
    keyword_delete
    keyword_insert
    keyword_replace
    keyword_update
    keyword_truncate
    keyword_merge
    keyword_into
    keyword_overwrite
    keyword_values
    keyword_value
    keyword_matched
    keyword_set
    keyword_from
    keyword_left
    keyword_right
    keyword_inner
    keyword_full
    keyword_outer
    keyword_cross
    keyword_join
    keyword_lateral
    keyword_natural
    keyword_on
    keyword_off
    keyword_where
    keyword_order
    keyword_group
    keyword_partition
    keyword_by
    keyword_having
    keyword_desc
    keyword_asc
    keyword_limit
    keyword_offset
    keyword_primary
    keyword_create
    keyword_alter
    keyword_change
    keyword_analyze
    keyword_explain
    keyword_verbose
    keyword_modify
    keyword_drop
    keyword_add
    keyword_table
    keyword_tables
    keyword_view
    keyword_column
    keyword_columns
    keyword_materialized
    keyword_tablespace
    keyword_sequence
    keyword_increment
    keyword_minvalue
    keyword_maxvalue
    keyword_none
    keyword_owned
    keyword_start
    keyword_restart
    keyword_key
    keyword_as
    keyword_distinct
    keyword_constraint
    keyword_filter
    keyword_cast
    keyword_separator
    keyword_max
    keyword_min
    keyword_avg
    keyword_case
    keyword_when
    keyword_then
    keyword_else
    keyword_end
    keyword_in
    keyword_and
    keyword_or
    keyword_is
    keyword_not
    keyword_force
    keyword_ignore
    keyword_using
    keyword_use
    keyword_index
    keyword_for
    keyword_if
    keyword_exists
    keyword_auto_increment
    keyword_generated
    keyword_always
    keyword_collate
    keyword_character
    keyword_engine
    keyword_default
    keyword_cascade
    keyword_restrict
    keyword_with
    keyword_without
    keyword_no
    keyword_data
    keyword_type
    keyword_rename
    keyword_to
    keyword_database
    keyword_schema
    keyword_owner
    keyword_user
    keyword_admin
    keyword_password
    keyword_encrypted
    keyword_valid
    keyword_until
    keyword_connection
    keyword_role
    keyword_reset
    keyword_temp
    keyword_temporary
    keyword_unlogged
    keyword_logged
    keyword_cycle
    keyword_union
    keyword_all
    keyword_any
    keyword_some
    keyword_except
    keyword_intersect
    keyword_returning
    keyword_begin
    keyword_commit
    keyword_rollback
    keyword_transaction
    keyword_over
    keyword_nulls
    keyword_first
    keyword_after
    keyword_before
    keyword_last
    keyword_window
    keyword_range
    keyword_rows
    keyword_groups
    keyword_between
    keyword_unbounded
    keyword_preceding
    keyword_following
    keyword_exclude
    keyword_current
    keyword_row
    keyword_ties
    keyword_others
    keyword_only
    keyword_unique
    keyword_foreign
    keyword_references
    keyword_concurrently
    keyword_btree
    keyword_hash
    keyword_gist
    keyword_spgist
    keyword_gin
    keyword_brin
    keyword_like
    keyword_similar
    keyword_preserve
    keyword_unsigned
    keyword_zerofill
    keyword_conflict
    keyword_do
    keyword_nothing
    keyword_high_priority
    keyword_low_priority
    keyword_delayed
    keyword_recursive
    keyword_cascaded
    keyword_local
    keyword_current_timestamp
    keyword_check
    keyword_option
    keyword_vacuum
    keyword_wait
    keyword_nowait
    keyword_attribute
    keyword_authorization
    keyword_action
    keyword_extension
    keyword_trigger
    keyword_function
    keyword_returns
    keyword_return
    keyword_setof
    keyword_atomic
    keyword_declare
    keyword_language
    keyword_sql
    keyword_plpgsql
    keyword_immutable
    keyword_stable
    keyword_volatile
    keyword_leakproof
    keyword_parallel
    keyword_safe
    keyword_unsafe
    keyword_restricted
    keyword_called
    keyword_returns
    keyword_input
    keyword_strict
    keyword_cost
    keyword_rows
    keyword_support
    keyword_definer
    keyword_invoker
    keyword_security
    keyword_version
    keyword_extension
    keyword_out
    keyword_inout
    keyword_variadic
    keyword_session
    keyword_isolation
    keyword_level
    keyword_serializable
    keyword_repeatable
    keyword_read
    keyword_write
    keyword_committed
    keyword_uncommitted
    keyword_deferrable
    keyword_names
    keyword_zone
    keyword_immediate
    keyword_deferred
    keyword_constraints
    keyword_snapshot
    keyword_characteristics
    keyword_external
    keyword_stored
    keyword_virtual
    keyword_cached
    keyword_uncached
    keyword_replication
    keyword_tblproperties
    keyword_options
    keyword_compute
    keyword_stats
    keyword_statistics
    keyword_optimize
    keyword_rewrite
    keyword_bin_pack
    keyword_incremental
    keyword_location
    keyword_partitioned
    keyword_comment
    keyword_sort
    keyword_format
    keyword_delimited
    keyword_fields
    keyword_terminated
    keyword_escaped
    keyword_lines
    keyword_cache
    keyword_metadata
    keyword_noscan
    keyword_parquet
    keyword_rcfile
    keyword_csv
    keyword_textfile
    keyword_avro
    keyword_sequencefile
    keyword_orc
    keyword_avro
    keyword_jsonfile)
  "Treesit parser keyword nodes")

;; not implemented in parser "&&" "&" ">>" "<<" "#" "|" "!!" "~" "!~" "~*" "!~*"
;; "@"
(defvar sql-ts-mode--operators
  '("->" "->>" "#>" "#>>" "::" "+" "-"
    "*" "/" "%"
    "||" "<" "<=" "<>" "!="
    "=" ">" ">=")
  "Postgres SQL operators")

(defvar sql-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'sql
   :override t
   :feature 'string
   '((literal) @sql-ts-mode--fontify-string)
   :language 'sql
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face
     (marginalia) @font-lock-comment-face)
   :language 'sql
   :override t
   :feature 'delimiter
   '(([";" ","]) @font-lock-delimiter-face)
   :language 'sql
   :override t
   :feature 'keyword
   (let (result '())
     (dolist (keyword sql-ts-mode--keywords result)
       (setq result (cons (list keyword) (cons '@font-lock-keyword-face result))))
     result)
   :language 'sql
   :override t
   :feature 'operator
   `(([,@sql-ts-mode--operators]) @font-lock-operator-face)
   :language 'sql
   :override t
   :feature 'number
   '((literal) @sql-ts-mode--fontify-number)
   :language 'sql
   :override t
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)
   :language 'sql
   :override t
   :feature 'type
   '((column_definition type: (_) @font-lock-type-face)
     (cast (_) (_) @font-lock-type-face)
     (function_argument (_) @font-lock-type-face :anchor)
     (create_function (keyword_returns) :anchor (_) @font-lock-type-face))
   :language 'sql
   :override t
   :feature 'punctuation
   '((["."]) @font-lock-punctuation-face)
   :language 'sql
   :override t
   :feature 'error
   '((ERROR) @font-lock-warning-face
     (MISSING) @font-lock-warning-face
     (UNEXPECTED) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `sql-pg-ts-mode'.")

(defun sql-ts-mode--fontify-number (node override start end &rest _)
  "Fontify number from node literal."
  (let ((c (aref (treesit-node-text node) 0)))
    (when (or (eq c 43)
              (eq c 45)
              (eq c 46)
              (and (>= c 48) (<= c 57)))
      (let ((node-start (treesit-node-start node))
            (node-end (treesit-node-end node)))
        (put-text-property node-start node-end 'face 'font-lock-number-face)
        (put-text-property node-start node-end 'fontified t)))))

;; TODO: multiline
;; TODO: typed string (REAL '1.25')
(defun sql-ts-mode--fontify-string (node override start end &rest _)
  "Fontify string from node literal."
  (let* ((text (treesit-node-text node))
         (c (aref text 0)))
    (when (or
           ;; bit string
           (and
            (or (eq c 98)
                (eq c 66)
                (eq c 120)
                (eq c 88))
            (>= (length text) 3)
            (eq (aref text 1) 39))
           ;; unicode
           (and
            (eq c 117)
            (>= (length text) 4)
            (eq (aref text 1) 85)
            (eq (aref text 2) 38))
           (eq c 39)
           (eq c 36)
           (eq c 34))
      (let ((node-start (treesit-node-start node))
            (node-end (treesit-node-end node)))
        (put-text-property node-start node-end 'face 'font-lock-string-face)
        (put-text-property node-start node-end 'fontified t)))))

(defvar sql-ts-mode--indent-rules
  '((sql
    ((parent-is "program") column-0 0)
    ((parent-is "select_expression") parent 0)
    ((node-is "select") parent 0)
    ((node-is "from") parent 0)
    ((node-is "relation") prev-sibling 0)
    ((node-is "join") parent 0)
    ((and (node-is "keyword_on")
          (parent-is "join")) prev-sibling 0)
    ((node-is "where") standalone-parent 0)
    ((node-is "group_by") parent 0)
    ((node-is "order_by") parent 0)
    ((and (node-is "keyword_having")
          (parent-is "group_by")) parent 0)
    ((lambda (node parent bol)
       (let ((parent (treesit-node-parent node)))
         (and parent
              (equal (treesit-node-type node) "list")
              (equal (treesit-node-type parent) "insert")
              (not (equal (treesit-node-type (treesit-node-prev-sibling node)) "object_reference")))))
     prev-sibling 0)
    ((node-is "column_definitions") parent 0)
    ((node-is "column_definition") prev-sibling 0)
    (catch-all parent 0))))

(defun sql-ts-mode--imenu-name (node)
  (let ((captures (treesit-query-capture
                   node
                   '((statement (select) @type) @statement
                     (statement (insert) @type) @statement
                     (statement (update) @type) @statement
                     (statement (delete) @type) @statement
                     (statement (create_table) @type) @statement
                     (statement (create_view) @type) @statement
                     (statement (create_materialized_view) @type) @statement
                     (statement (create_index) @type) @statement
                     (statement (create_function) @type) @statement
                     (statement (create_type) @type) @statement
                     (statement (create_database) @type) @statement
                     (statement (create_role) @type) @statement
                     (statement (create_sequence) @type) @statement
                     (statement (create_extension) @type) @statement
                     (statement (create_trigger) @type) @statement
                     (statement (create_schema) @type) @statement))))
    ;; ((statement . #<treesit-node statement in 1-61>) (type . #<treesit-node insert in 1-61>))
    (if captures
      (concat
       (treesit-node-type (cdr (assoc 'type captures)))
       " @"
       (number-to-string (treesit-node-start (cdr (assoc 'statement captures)))))
      "Anonymous")))


;;;###autoload
(define-derived-mode sql-ts-mode sql-mode "SQL"
  "Major mode for editing postgres SQL, powered by tree-sitter."
  :group 'ts-sql

  (cond
   ((treesit-ready-p 'sql) (treesit-parser-create 'sql))
   (t (error "Tree-sitter for SQL isn't available")))

  ;; Remove this sql-mode pecularity
  (when (memq 'sql-indent-enable sql-mode-hook)
    (setq sql-mode-hook (remq 'sql-indent-enable sql-mode-hook)))

  (setq-local treesit-text-type-regexp
              (regexp-opt '("comment"
                            "marginalia")))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp "statement")

  (setq-local treesit-block-type-regexp "statement")

  (setq-local treesit-sentence-type-regexp (regexp-opt '("select"
                                                         "from"
                                                         "join"
                                                         "where"
                                                         "group_by"
                                                         "insert"
                                                         "update"
                                                         "delete"
                                                         "create_table"
                                                         "create_view"
                                                         "create_materialized_view"
                                                         "create_index"
                                                         "create_function"
                                                         "create_type"
                                                         "create_database"
                                                         "create_role"
                                                         "create_sequence"
                                                         "create_extension"
                                                         "create_trigger"
                                                         "create_schema")))

  (setq-local treesit-sexp-type-regexp
              (regexp-opt (append
                           '("identifier"
                             "object_reference"
                             "literal"
                             "field"
                             "parameter"
                             "list"
                             "case"
                             "array"
                             "between_expression"
                             "invocation"
                             "cast"
                             "binary_expression"
                             "unary_expression")
                           (mapcar #'symbol-name sql-ts-mode--keywords))))

  (setq-local treesit-simple-indent-rules sql-ts-mode--indent-rules)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings sql-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment )
                ( keyword string number type )
                ( bracket delimiter operator)
                ( punctuation )
                ( error )))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '((nil "\\`statement\\'" nil sql-ts-mode--imenu-name)))

  (treesit-major-mode-setup))

(provide 'sql-ts-mode)
;;; sql-ts-mode.el ends here
