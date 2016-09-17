;;; This file contains some Emacs settings that are recommended for KreaTV
;;; development.
;;;
;;; Load it like this:
;;;
;;; (add-to-list 'load-path "/home/tools/elisp")
;;; (require 'kreatv-recommended)
;;;
;;; The idea is to keep this file quite conservative so that you can load it
;;; without fear of clobbering your settings too much. For instance, it doesn't
;;; define any key bindings.
;;;
;;; However, here are some suggested key bindings that you can add to your
;;; ~/.emacs or similar:
;;;
;;; (global-set-key (kbd "M-,") 'gtags-find-rtag)
;;; (global-set-key (kbd "M-.") 'gtags-find-tag)
;;; (global-set-key (kbd "M-g M-f") 'gtags-find-file)
;;; (global-set-key (kbd "M-g M-s") 'gtags-find-symbol)
;;; (global-set-key (kbd "M-g M-u") 'gtags-update)
;;; (global-set-key (kbd "M-g f") 'gtags-find-file)
;;; (global-set-key (kbd "M-g s") 'gtags-find-symbol)
;;; (global-set-key (kbd "M-g u") 'gtags-update)
;;; (global-set-key (kbd "<f11>") 'delete-trailing-whitespace-and-keep-doing-so)
;;; (global-set-key (kbd "<C-f11>") 'toggle-auto-remove-trailing-whitespace))

;; KreaTV coding style.
(require 'kreatv-cc-style)

;; Use the kreatv C/C++ style. See
;; <http://twiki.mot.com/bin/view/KreaTV/CppCodingStandard>.
(setq c-default-style "kreatv")

;; Don't indent with tabs.
(setq indent-tabs-mode nil)

(defun kreatv-cc-mode-init ()
  ;; Add missing final newline automatically when saving a C/C++ file.
  (set (make-local-variable 'require-final-newline) t))
(add-hook 'c-mode-common-hook 'kreatv-cc-mode-init)

;; Remove inadvertently added trailing whitespace automatically when saving.
(require 'no-trailing-whitespace)

;; Support for GNU Global. See
;; <http://twiki.mot.com/bin/view/KreaTV/GnuGlobal>.
(require 'kreatv-gtags)

;; Use C++ mode for .h files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Use Python mode for python2 and python3 interpreters.
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

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

(provide 'kreatv-recommended)
