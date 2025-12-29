;;; happy-path.el --- Happy-path view of program source  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package provides ‘happy-path-mode’, which grays out error
;; handling, logs, and other non-happy-path code, allowing reader to
;; grasp the main flow of the code easily.

;;; Code:

(defgroup happy-path nil
  "Dims non-main-flow code for better focus."
  :group 'utility)

(defface happy-path-dim '((t . (:inherit shadow)))
  "Face for dimmed code."
  :group 'happy-path)

(defvar happy-path-rust-queries
  '((err-branch
     . (;; Simple Err(e) pattern
        ((match_arm
          pattern: (match_pattern
                    (tuple_struct_pattern
                     type: (identifier) @_type)))
         @happy-path-dim
         (:match "\\`Err\\'" @_type))

        ;; Scoped Result::Err(e) pattern
        ((match_arm
          pattern: (match_pattern
                    (tuple_struct_pattern
                     type: (scoped_identifier
                            name: (identifier) @_type))))
         @happy-path-dim
         (:match "\\`Err\\'" @_type))))

    (if-let-err
     . (;; Simple without else: if let Err(e) = xxx { ... }
        ((if_expression
          "if" @happy-path-dim
          condition: (let_condition
                      "let" @happy-path-dim
                      pattern: (tuple_struct_pattern
                                type: (identifier) @_type) @happy-path-dim
                      "=" @happy-path-dim)
          consequence: (_) @happy-path-dim
          !alternative)
         (:match "\\`Err\\'" @_type))

        ;; Scoped without else: if let Result::Err(e) = xxx { ... }
        ((if_expression
          "if" @happy-path-dim
          condition: (let_condition
                      "let" @happy-path-dim
                      pattern: (tuple_struct_pattern
                                type: (scoped_identifier
                                       name: (identifier) @_type)) @happy-path-dim
                      "=" @happy-path-dim)
          consequence: (_) @happy-path-dim
          !alternative)
         (:match "\\`Err\\'" @_type))

        ;; Simple with else: capture only consequence
        ((if_expression
          "if" @happy-path-dim
          condition: (let_condition
                      "let" @happy-path-dim
                      pattern: (tuple_struct_pattern
                                type: (identifier) @_type) @happy-path-dim
                      "=" @happy-path-dim)
          consequence: (_) @happy-path-dim
          alternative: (_))
         (:match "\\`Err\\'" @_type))

        ;; Scoped with else: capture only consequence
        ((if_expression
          "if" @happy-path-dim
          condition: (let_condition
                      "let" @happy-path-dim
                      pattern: (tuple_struct_pattern
                                type: (scoped_identifier
                                       name: (identifier) @_type)) @happy-path-dim
                      "=" @happy-path-dim)
          consequence: (_) @happy-path-dim
          alternative: (_))
         (:match "\\`Err\\'" @_type)))))
  "Stores rust queries.
Stores in an alist, give each query a name for easy access.")

(defvar happy-path-dim-settings-alist
  (list (cons 'rust-ts-mode
              (treesit-font-lock-rules
               :language 'rust
               :feature 'happy-path-dim
               (append (alist-get 'err-branch happy-path-rust-queries)
                       (alist-get 'if-let-err happy-path-rust-queries)))))
  "Alist mapping major mode symbol to SETTINGS.

SETTINGS is the return value of ‘treesit-font-lock-rules’. When calling
‘treesit-font-lock-rules’, the feature must always be ‘happy-path-dim’.
The face should always be 'happy-path-dim'.")

(define-minor-mode happy-path-mode
  "Minor mode that dims non-main-flow code for better focus."
  :global nil
  :lighter ""
  (if happy-path-mode
      ;; Enable minor mode.
      (progn
        (dolist (entry happy-path-dim-settings-alist)
          (let ((mode (car entry))
                (settings (cdr entry)))
            (when (derived-mode-p mode)
              (treesit-add-font-lock-rules settings :before))))
        (treesit-font-lock-recompute-features '(happy-path-dim)))
    ;; Disable minor mode.
    (treesit-font-lock-recompute-features nil '(happy-path-dim)))
  (treesit-font-lock-fontify-region (point-min) (point-max)))

(provide 'happy-path)

;;; happy-path.el ends here
