# happy-path

Emacs package that dims non-happy-path code (error handling, logging) using tree-sitter.

## Project Structure

- `happy-path.el` - Main package file
- `grammars/tree-sitter-rust/grammar.js` - Rust grammar reference for writing queries
- `tests/` - Test files for verifying tree-sitter queries

## Adding New Language Support

1. Add queries to a `happy-path-<lang>-queries` alist with named query groups
2. Add entry to `happy-path-dim-settings-alist` using `treesit-font-lock-rules`
3. Feature must be `'happy-path-dim`, face must be `'happy-path-dim`

## Writing Tree-sitter Queries

- Reference grammar files in `grammars/tree-sitter-<lang>/grammar.js`
- Use `:match` predicate for string matching: `(:match "\\`Err\\'" @_type)`
- Capture the entire construct (e.g., `match_arm`, `if_expression`) with `@happy-path-dim`

## Testing Queries

Run verification from project root:
```elisp
(require 'happy-path)
(load-file "tests/test-happy-path-rust.el")
(happy-path-rust-run-test "tests/test-err.rs" 'err-branch)
```

Or batch mode:
```bash
emacs --batch -Q -L . --eval '(progn (require (quote happy-path)) (load-file "tests/test-happy-path-rust.el") (happy-path-rust-run-test "tests/test-err.rs"))'
```
