
;;; Setup import tool

(require 'java-imports)
(setq java-imports-find-block-function 'java-imports-find-place-sorted-block)


;;; Setup javadoc support
(require 'javadoc-lookup)
;;(javadoc-add-roots "/usr/share/doc/openjdk-8-jdk/api")
(setq browse-url-browser-function 'xwidget-webkit-browse-url)


;;; Add patterns for documentation

(define-skeleton javadoc-copyright-unlicense
  "Unlicense header boilerplate" ""
  "/*\n"
  " * The contents of this file is free and unencumbered software released into the\n"
  " * public domain. For more information, please refer to <http://unlicense.org/>\n"
  " */\n")

(define-skeleton javadoc-todo
  "TODO boilerplate" "" "// TODO: ")

(define-skeleton javadoc-class
  "Class documentation" ""
  "/**\n"
  " * @author Igor Filipenko\n"
  " *         https://github.com/kagemusha666\n"
  " */")

(define-skeleton javadoc-method
  "Class documentation" ""
  "/**\n"
  " * @param\n"
  " * @return\n"
  " * @throws\n"
  " */")

(define-skeleton javadoc-fields-section
  "Add comment the beginning of fields section" ""
  "// ======================================\n"
  "// =               Fields               =\n"
  "// ======================================\n")

(define-skeleton javadoc-public-methods-section
  "Add comment the beginning of public methods section" ""
  "// ======================================\n"
  "// =           Public Methods           =\n"
  "// ======================================\n")

(define-skeleton javadoc-getters-and-setters-section
  "Add comment the beginning of getters&setters section" ""
  "// ======================================\n"
  "// =           Getters&Setters          =\n"
  "// ======================================\n")

(define-skeleton javadoc-private-methods-section
  "Add comment the beginning of private methods section" ""
  "// ======================================\n"
  "// =           Private Methods          =\n"
  "// ======================================\n")

(define-skeleton javadoc-static-utilities-section
  "Add comment the beginning of static utilities section" ""
  "// ======================================\n"
  "// =           Static Utilities         =\n"
  "// ======================================\n")


;;; JDEE setup

(require 'jdee)
(setq jdee-server-dir "/usr/local/share/jdee/")

(c-add-style
 "java-default"
 '("java"
   (indent-tabs-mode . nil)
   (c-basic-offset . 4)))

(defun java-jdee-mode-hook ()
  "Hook for running Java file..."
  (setq c-default-style "java-default")
  (define-key c-mode-base-map "\C-c\C-c" 'jdee-complete)
  (define-key (current-global-map) (kbd "<f5>") 'jdee-make)
  (java-imports-scan-file))

(setq jdee-make-finish-hook '())

(add-hook 'jdee-mode-hook 'java-jdee-mode-hook)

(provide 'java-support)
