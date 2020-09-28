;;; snippets/dot-mode.el -*- lexical-binding: t; -*-

(defvar dot-mode-symbol-regexp
  "[[:lower:]-]+"
  "Intentionally strict symbol definition for `dot-mode'")

(defvar dot-mode-comment-char ?#
  "The `dot-moder' comment character")

(defvar dot-mode-in-comment nil
  "Whether we're currently fontifying a comment.")

(defvar dot-mode-comment-column nil
  "Column where comment currently being processed was")

(defun dot-mode-comment-matcher (bound)
  "Called as `font-lock-keywords' matcher."
  (when (= ?# (char-after))
    (setq dot-mode-in-comment t
          dot-mode-comment-column (current-column))
    (re-search-forward
     (concat ".*" (char-to-string dot-mode-comment-char) ".+$")
     bound t))
  (when dot-mode-in-comment
    (setq dot-mode-in-comment
          (re-search-forward
           (concat (make-string (1+ dot-mode-comment-column) ?\s) ".+$")
           bound t))))

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
