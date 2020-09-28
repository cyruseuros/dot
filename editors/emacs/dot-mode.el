;;; snippets/dot-mode.el -*- lexical-binding: t; -*-

(defvar dot-mode-symbol-regexp
  "[[:lower:]-]+"
  "Intentionally strict symbol definition for `dot-mode'")

(defvar dot-mode-comment-char ?#
  "The `dot-moder' comment character")

(defvar dot-mode-comment-regex " .*$"
  "Regex to match comments.
To be used after `dot-mode-comment-char' or a sufficient amount
  of leading whitespace until the end of the line.")

(defvar dot-mode-in-comment nil
  "Whether we're currently fontifying a comment.")

(defvar dot-mode-comment-column nil
  "Column where comment currently being processed was")

(defun dot-mode-comment-matcher (bound)
  "Called as `font-lock-keywords' matcher."
  (if (= ?# (char-after))
      (progn
        (setq dot-mode-in-comment t
              dot-mode-comment-column (current-column))
        (re-search-forward
         (concat "^.*" (char-to-string dot-mode-comment-char)
                 dot-mode-comment-regex)
         bound t))
    (when dot-mode-in-comment
      (setq dot-mode-in-comment
            (re-search-forward
             (concat "^" (make-string dot-mode-comment-column ?\s)
                     dot-mode-comment-regex)
             bound t)))))

# multi-line
comment

(defvar dot-mode-font-lock-keywords
  `((,(concat " -" dot-mode-symbol-regexp) . font-lock-keyword-face)
    ("[.:,/]" . font-lock-builtin-face)
    (dot-mode-comment-matcher 0 font-lock-comment-face))
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
