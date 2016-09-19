;;; This file contains some Emacs settings that are recommended for common
;;; C/C++ development.
;;;
;;; Load it like this:
;;;
;;; (require 'common-recommended)
;;;
;;; The idea is to keep this file quite conservative so that you can load it
;;; without fear of clobbering your settings too much. For instance, it doesn't
;;; define any key bindings.
;;;
;;; However, here are some suggested key bindings that you can add to your
;;; ~/.emacs or similar:
;;;
;;; (global-set-key (kbd "<f11>") 'delete-trailing-whitespace-and-keep-doing-so)
;;; (global-set-key (kbd "<C-f11>") 'toggle-auto-remove-trailing-whitespace))

;; Coding style.
(require 'common-c++-style)

;; Compile for common make.
(require 'common-make-compile)

;; Use the common C/C++ style.
(setq c-default-style "common")

;; Don't indent with tabs.
(setq indent-tabs-mode nil)

(defun cc-mode-init ()
  ;; Add missing final newline automatically when saving a C/C++ file.
  (set (make-local-variable 'require-final-newline) t))
(add-hook 'c-mode-common-hook 'cc-mode-init)

;; Remove inadvertently added trailing whitespace automatically when saving.
(require 'no-trailing-whitespace)

;; Use C++ mode for .h files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; markdown-mode
(autoload
  'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mmd" . markdown-mode))

;; Colorize output in compilation mode buffers (otherwise raw escape character
;; sequences will be visible).
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'common-recommended)
