# sql-pg-ts-mode.el
A treesitter mode for sql

Add the treesitter grammar:

```
(add-to-list 'treesit-language-source-alist '(sql "https://github.com/DerekStride/tree-sitter-sql/" "gh-pages"))
(treesit-install-language-grammar 'sql)
```
