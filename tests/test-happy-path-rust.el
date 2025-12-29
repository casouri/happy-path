;;; test-happy-path-rust.el --- Test happy-path Rust query  -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)
(require 'happy-path)

(defun happy-path-rust-run-test (rust-file &optional query-name)
  "Test the query on RUST-FILE and report captures.
QUERY-NAME is a key in `happy-path-rust-queries', defaults to `err-branch'."
  (interactive "fRust file: ")
  (let ((query (alist-get (or query-name 'err-branch) happy-path-rust-queries)))
    (with-temp-buffer
      (insert-file-contents rust-file)
      (let* ((parser (treesit-parser-create 'rust))
             (root (treesit-parser-root-node parser))
             (captures (treesit-query-capture root query)))
        (message "=== Happy-path Rust Query Test Results ===")
        (message "Query: %s" (or query-name 'err-branch))
        (message "Found %d total captures" (length captures))
        (let ((dim-count 0))
          (dolist (capture captures)
            (when (eq (car capture) 'happy-path-dim)
              (setq dim-count (1+ dim-count))
              (let ((node (cdr capture)))
                (message "Capture %d: lines %d-%d"
                         dim-count
                         (line-number-at-pos (treesit-node-start node))
                         (line-number-at-pos (treesit-node-end node))))))
          (message "=== Total Err match arms captured: %d ===" dim-count))
        captures))))

(provide 'test-happy-path-rust)
;;; test-happy-path-rust.el ends here
