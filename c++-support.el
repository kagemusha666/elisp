
;;; Custom functions

(defun get-other-c++-buffer ()
   (let* ((buffer-base-file-name
           (file-name-sans-extension (buffer-file-name)))
          (h-file-name (concat buffer-base-file-name ".h"))
          (cpp-file-name (concat buffer-base-file-name ".cpp"))
          (c-file-name (concat buffer-base-file-name ".c")))
     (cond
      ((string= cpp-file-name (buffer-file-name))
       (find-file-noselect h-file-name t))
      ((string= c-file-name (buffer-file-name))
       (find-file-noselect h-file-name t))
      ((string= h-file-name (buffer-file-name))
       (if (file-exists-p c-file-name)
           (find-file-noselect c-file-name t)
           (find-file-noselect cpp-file-name t)))
      (t (error "This is not a C/C++ file.")))))

;; Switch to .cpp file when editing .h file and vice versa.
;; Mapped to C-f8 (see below).
(defun switch-to-other-c++-file ()
   (interactive)
   (switch-to-buffer (get-other-c++-buffer)))

;; Switch to .cpp file in other frame, when editing .h file and vice versa.
;; Mapped to C-M-f8 (see below).
(defun switch-to-other-c++-file-in-next-frame ()
   (interactive)
   (set-window-buffer (frame-first-window (next-frame)) (get-other-c++-buffer))
   (other-frame 1))


;;; Changes to support the newest C++ standart

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

(defun c++-enum-fix ()
  (cc-fix-enum-class)
  (setq c-label-minimum-indentation 0))


;;; Setup Zodiac CS

(c-add-style
 "zodiac"
 '("ellemtel"
   (indent-tabs-mode . 1)
   (c-basic-offset . 4)
   (c-block-comment-prefix . "// ")))

(setq c-default-style "zodiac")


;;; Setup some patterns

(define-skeleton copyright
  "Zodiac license boilerplate"
  "//\n"
  "//\n"
  "// Copyright (C) " (format-time-string "%Y") " DOB, LLC.\n"
  "//\n"
  "// @developer Igor Filipenko <igor.filipenko@dev.zodiac.tv>\n"
  "//\n"
  "// Proprietary and Confidential\n"
  "// Unauthorized distribution or copying is prohibited\n"
  "// All rights reserved\n"
  "//\n"
  "// No part of this computer software may be reprinted, reproduced or utilized\n"
  "// in any form or by any electronic, mechanical, or other means, now known or\n"
  "// hereafter invented, including photocopying and recording, or using any\n"
  "// information storage and retrieval system, without permission in writing\n"
  "// from DOB, LLC.\n"
  "//\n")

(define-skeleton todo
  "Zodiac TODO boilerplate" "" "// TODO(igor.filipenko@dev.zodiac.tv>): ")


(defun fix-zodiac-style ()
  "Fixing braces tabulation for zodiac CS"
  (interactive)
  (list
   (replace-string "(" "( ")
   (replace-string ")" " )")
   (replace-string "(  )" "()")))

;;; Setup company

(require 'company)

(setq company-backends '(company-bbdb
                         company-eclim
                         company-semantic
                         helm-company
                         company-c-headers
                         company-capf
                         (company-dabbrev-code
                          company-gtags
                          company-etags
                          company-keywords)
                         company-oddmuse
                         company-files
                         company-dabbrev))


;;; Setting additional key bindings

(define-key c++-mode-map [C-f8] 'switch-to-other-c++-file)
(define-key c++-mode-map [C-M-f8] 'switch-to-other-c++-file-in-next-frame)
(define-key c++-mode-map "\C-c\C-j" 'semantic-ia-fast-jump)
(define-key c++-mode-map "\C-c\C-s" 'semantic-ia-show-summary)
(define-key c++-mode-map "\C-c\C-c"  'company-complete)


;;; Setup C++ hooks

(defun my-c++-mode-hook ()
  "Hook for C/C++"
  (c++-enum-fix)
  (setq c-default-style "zodiac"))

(add-hook 'c-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
                ("\\.c$" . c++-mode))
              auto-mode-alist))

(provide 'c++-support)
