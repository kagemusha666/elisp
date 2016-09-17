;;; This file defines an Emacs cc-mode style called "kreatv" that follows the
;;; KreaTV coding standard.

(c-add-style
 "c++-common"
 '("gnu"
   (indent-tabs-mode . nil)
   (c-basic-offset . 2)
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

(provide 'c++-common-style)
