# Fixed Issues

## Tree-sitter JSON Grammar Missing (2026-03-15)

Emacs warns `language grammar for json is unavailable` because only bash grammar exists in `~/.emacs.d/tree-sitter/`.

**Fix**: `M-x treesit-install-language-grammar RET json` — or batch-install all:

```elisp
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```
