;;; Provides suitable compile commands.
;;;
;;; Example key bindings to add to .emacs or similar:
;;;
;;; (global-set-key [f5] 'compile)
;;; (global-set-key [f6] 'compile-clean)

(set-default 'compile-command "make all")
(set-default 'compile-clean-command "make clean")

(defun compile-directory ()
  "Get the relative path to a directory where it is suitable start a compile."
  (cond
   ((file-exists-p "Makefile") nil)
   ((file-exists-p "../Makefile") "..")
   ((file-exists-p "../../Makefile") "../..")
   (t nil)))

(defun set-default-compile-command ()
  (let ((path (compile-directory))
	(command "make"))
    (when path (setq command (concat "cd " path " && " command)))
    (set (make-local-variable 'compile-command)
         (concat command " all"))
    (set (make-local-variable 'compile-clean-command)
         (concat command " clean"))))

(defun compile-clean ()
  "Run make clean for the current component"
  (interactive)
  (let ((compile-command compile-clean-command))
    (call-interactively 'compile)))

(add-hook 'c-mode-common-hook 'set-default-compile-command)

(provide 'common-make-compile)
