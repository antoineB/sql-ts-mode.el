# sql-ts-mode.el
A treesitter mode for sql

Add the treesitter grammar:

```
(add-to-list 'treesit-language-source-alist '(sql "https://github.com/DerekStride/tree-sitter-sql/" "gh-pages"))
(treesit-install-language-grammar 'sql)
```

### add the completion

```
(sql-ts-mode--parse-schema-file "my-schema.sql")
(setq completion-at-point-functions (list #'sql-ts-mode-completion-at-point))
```
