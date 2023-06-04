# sql-pg-ts-mode.el
A treesitter mode for postgres sql

Add the treesitter grammar:

```
(add-to-list 'treesit-language-source-alist '(sql "https://github.com/m-novikov/tree-sitter-sql"))
(treesit-install-language-grammar 'sql)
```
