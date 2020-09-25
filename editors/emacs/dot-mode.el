;;; snippets/dot-mode.el -*- lexical-binding: t; -*-

(defvar dot-mode-symbol-regexp
  "[[:lower:]-]+"
  "Intentionally strict symbol definition for `dot-mode'")

(defvar dot-mode-font-lock-keywords
  `((,(concat "-" dot-mode-symbol-regexp) . font-lock-keyword-face)
    ("[.:,/]" . font-lock-builtin-face))
  "`dot-mode' keywords to be used in `font-lock-defaults'.")

(defvar dot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" table) ; strings
    table)
  "Syntax table used in `dot-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dot\\'" . dot-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("dot" . dot-mode))
;;;###autoload
(define-derived-mode dot-mode prog-mode ".dot"
  "A major mode for the .dot programming language."
  :syntax-table dot-mode-syntax-table
  (setq-local font-lock-defaults
              `(,dot-mode-font-lock-keywords
                nil t nil nil
                ;; TODO: add fancy functions to detect function bodies etc.
                )))

(provide 'dot-mode)
