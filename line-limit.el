;;; Limit line length and encourage fixing broken files

(defun find-longest-line-in-buffer ()
  "Find the longest line in the current buffer. Returns a dotted pair
   of the length of the longest line, and the point where the longest
   line was found. See `current-column' for the definition of line
   length."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((maxlen 0)
            (badpos (point)))
        (while (not (eobp))
          (end-of-line)
          (let ((cc (current-column)))
            (when (> cc maxlen)
              (setq maxlen cc)
              (setq badpos (point))))
          (forward-line))
        (cons maxlen badpos)))))

(defun refuse-overlong-lines ()
  "Ask for confirmation if a too long line is found in the current buffer."
  (let ((maxcol (find-longest-line-in-buffer)))
    (when (> (car maxcol) 131)
      (if (yes-or-no-p
           "Found too long line, do you REALLY REALLY want to save? ")
          nil
        (progn
          (goto-char (cdr maxcol))
          (message "Jumping to offensive line")
          t)))))

(defun warn-overlong-lines ()
  "Warn for a too long line and sleep a short while en encourage a fix."
  (let ((maxcol (find-longest-line-in-buffer)))
    (when (> (car maxcol) 131)
      (message "*** This file has lines that are too long! ***")
      (sleep-for 1)
      nil)))

(defun c++-on-open-allow-overlong ()
  "Find out if there is a too long line in a newly opened buffer and
   set allow-overlong-lines properly."
  (make-variable-buffer-local 'allow-overlong-lines)
  (setq allow-overlong-lines nil)
  (let ((maxcol (find-longest-line-in-buffer)))
    (when (> (car maxcol) 131)
      (setq allow-overlong-lines t)))
  (message "%s" allow-overlong-lines))

(add-hook 'c++-mode-hook
          '(lambda ()
             (if (and (boundp allow-overlong-lines) allow-overlong-lines)
                 (add-hook 'write-contents-hooks 'warn-overlong-lines)
               (add-hook 'write-contents-hooks 'refuse-overlong-lines))))
(add-hook 'c++-mode-hook 'c++-on-open-allow-overlong)

(add-hook 'c-mode-hook
          '(lambda ()
             (if (and (boundp allow-overlong-lines) allow-overlong-lines)
                 (add-hook 'write-contents-hooks 'warn-overlong-lines)
               (add-hook 'write-contents-hooks 'refuse-overlong-lines))))
(add-hook 'c-mode-hook 'c++-on-open-allow-overlong)

(provide 'line-limit)
