;;; This file defines an Emacs cc-mode style called "common" that follows the
;;; my own coding standard.

(defun cc-inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (looking-back "enum[ \t]+class[^}]+"))))

(defun cc-align-enum-class (langelem)
  (if (cc-inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun cc-align-enum-class-closing-brace (langelem)
  (if (cc-inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun cc-fix-enum-class ()
  (add-to-list 'c-offsets-alist
               '(topmost-intro-cont . cc-align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . cc-align-enum-class-closing-brace)))

(defun cc-init-c++-mode ()
  (cc-fix-enum-class)
  (setq c-label-minimum-indentation 0))

(add-hook 'c++-mode-hook 'cc-init-c++-mode)

(c-add-style
 "common"
 '("gnu"
   (indent-tabs-mode . nil)
   (c-basic-offset . 4)
   (c-block-comment-prefix . "* ")
   (c-hanging-colons-alist
    (case-label after)
    (label after))
   (c-cleanup-list
    empty-defun-braces
    defun-close-semi
    list-close-comma
    scope-operator)
   (c-hanging-semi&comma-criteria
    c-semi&comma-no-newlines-before-nonblanks
    c-semi&comma-no-newlines-for-oneline-inliners
    c-semi&comma-inside-parenlist)
   (c-offsets-alist
    (innamespace . 0)
    (inclass . +)
    (inline-open . 0)
    (statement-cont . +)
    (substatement-open . 0)
    (arglist-intro . +)
    (arglist-close . +))
   (c-hanging-braces-alist
    (substatement-open after)
    (defun-open before after)
    (class-open before after))))

(provide 'common-c++-style)
