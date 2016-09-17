;;; Example .emacs file
;;; Put together by Thomas J.
;;; Inspiration from Hallon, Mankan and Daniel-san.
;;;
;;; Search for TODO to find things that must changed to your settings.
;;; Additional things might have to be changed to your liking.

 (add-to-list 'load-path "/home/tools/elisp")

;; General environment
(set-language-environment "Latin-1")
(setq default-major-mode 'text-mode)
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq completion-ignored-extensions
      (append '(".obj" ".exe" ".o" ".class" ".hex" ".cod" ".$$$" ".elc")
              completion-ignored-extensions))


;; Unique buffernames
(load "uniquify")
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq frame-title-format '("%b"))


;; File name completion with /
(setq comint-completion-addsuffix t)

;; Show matching parenthesis
(show-paren-mode 1)

;; Show 24 hour time with display time
(setq display-time-24hr-format t)

;; Swedish weeks begin with monday
(setq calendar-week-start-day 1)

;; Require newline at the and of files
(setq require-final-newline t)

;; Emacs kill question
(setq kill-emacs-query-functions
      (cons (lambda ()
              (y-or-n-p "Really kill Emacs? "))
            kill-emacs-query-functions))


;;; ---------------------------------------------------------------------------
;;; Keyboard patches.

;; TODO: Some people might dislike home/end going to beginning/end of
;; buffer. Change to your liking.
(when (string-match "^21" emacs-version)
  (global-set-key [home] 'beginning-of-buffer)
  (global-set-key [end] 'end-of-buffer)
  (when window-system
    (mouse-wheel-mode t)
    (tool-bar-mode 0)))

(global-set-key [delete] "\C-d")
(global-set-key [C-backspace] [?\M-\177])
(global-set-key [C-delete] [?\M-d])
(global-set-key [f5] 'compile)
(global-set-key [down-mouse-3] 'imenu)
(global-set-key [mouse-2] nil)
(global-unset-key [S-backspace])
(global-unset-key [S-delete])

(setq compilation-finish-function '(lambda (buf res)
                                     (if (not (equal (substring res 0 8)
                                                     "finished"))
                                         (next-error))))

;; Scroll one line up.
;; Mapped to C-up (see below)
(defun scroll-line-up ()
  (interactive)
  (forward-line -1)
  (scroll-down 1))

;; Scroll one line down.
;; Mapped to C-down (see below)
(defun scroll-line-down ()
  (interactive)
  (forward-line 1)
  (scroll-up 1))

;; Update buffer when file has change without asking any questions.
;; Mapped to C-f12 (see below)
(defun revert-buffer-no-questions-asked ()
  (interactive)
  (revert-buffer nil t))

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

;; Change DOS newlines to UNIX style.
;; Mapped to C-f9 (see below)
(defun go-unix-file ()
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix))

(global-set-key [C-up] 'scroll-line-up)
(global-set-key [C-down] 'scroll-line-down)
(global-unset-key "\M-gd")
(global-unset-key "\M-gb")
(global-unset-key "\M-gl")
(global-unset-key "\M-gu")
(global-unset-key "\M-go")
(global-unset-key "\M-gi")
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key [M-i] 'indent-region)
(global-set-key [C-f12] 'revert-buffer-no-questions-asked)
(global-set-key [C-f8] 'switch-to-other-c++-file)
(global-set-key [C-M-f8] 'switch-to-other-c++-file-in-next-frame)
(global-set-key [C-f9] 'go-unix-file)


;;; ---------------------------------------------------------------------------
;;; Colors and fonts.

;; TODO: Change colors and fonts to your liking.

;; Colors
(setq default-frame-alist
      '((width . 80) (height . 76)
        (cursor-color . "orchid")
        (foreground-color . "black")
        (background-color . "burlywood")
        (font . "-*-courier-medium-r-normal--14-100-*-*-*-*-iso8859-1")))
(setq initial-frame-alist
      '((width . 80) (height . 76)))

(set-face-foreground 'modeline "yellow")
(set-face-background 'modeline "black")

(defun my-font-lock-colors ()
  (set-face-foreground 'font-lock-comment-face "blue4")
  (set-face-foreground 'font-lock-string-face "light yellow")
  (set-face-foreground 'font-lock-function-name-face "blue1")
  (set-face-foreground 'font-lock-keyword-face "red4")
  (set-face-foreground 'font-lock-variable-name-face "red3")
  (set-face-foreground 'font-lock-type-face "red2")
  (set-face-foreground 'font-lock-constant-face "yellow1")
  (set-face-foreground 'font-lock-builtin-face "blue2")
  (make-face-bold 'font-lock-keyword-face)
  (make-face-bold 'font-lock-function-name-face))
(add-hook 'font-lock-mode-hook 'my-font-lock-colors)


;;; ---------------------------------------------------------------------------
;;; Programming mode stuff.

;; C/C++ stuff according to Kreatel/Motorola coding standards.
(require 'kreatv-recommended)

;; Turn on font-lock in all modes that support it

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
(setq font-lock-maximum-decoration t)

;; IDL mode
(add-hook 'idl-mode-hook
          '(lambda ()
             (make-local-variable 'font-lock-defaults)
             (setq font-lock-defaults '(idl-font-lock-keywords))))

(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
                ("\\.c$" . c++-mode))
              auto-mode-alist))


;;; ---------------------------------------------------------------------------
;;; Printing


(defun kreatel-ps-print-code ()
  (interactive)
  (require 'ps-print)
  (let ((ps-paper-type 'a4)
        (ps-landscape-mode t)
        (ps-number-of-columns 2)
        (ps-line-number 1)
        (ps-zebra-stripes t)
        (ps-print-color-p nil))
    (ps-print-buffer-with-faces "~/test.ps")))

;; Menu bar commands
(define-key menu-bar-tools-menu [ps-print-buffer-with-faces]
  '("Print Code" . kreatel-ps-print-code))
(define-key global-map "\C-cp" 'kreatel-ps-print-code)


;;; ---------------------------------------------------------------------------
;;; Workflex
;;; This is settings for the home made little time reporting tool.
;;; TODO: The whole section can be removed if you decide not to use workflex.

;; TODO: Set directory where to store your time report files.
(setq flex-dir "/home/?/tidlappar")
;; TODO: Set your name here.
(setq flex-name "? ?")
(setq flex-filename-format "%Y-%b.txt")

(require 'workflex "/home/tools/devtools/trunk/elisp/workflex.el")
(setq flex-command "/home/tools/devtools/trunk/bin/workflex")


;;; ---------------------------------------------------------------------------
;;; Programming help functions
;;; TODO: Remove what you want or change key mappings.

(setq load-path (cons "/home/tools/devtools/trunk/elisp" load-path))

;; Insert header in C++ file.
(require 'kreatel-c++-header "c++-header.el")
(global-set-key "\C-c\C-h" 'kreatel-insert-header-stuff)

;; Help functions for unit testing.
(require 'kreatel-unit-test "unittest.el")
(global-set-key "\C-x\C-m" 'kreatel-create-mock-class)
(global-set-key "\C-x\C-t" 'kreatel-create-test-class)

;; Dabbrev expand, smart way to type things smart. Write the beginning
;; of a word anywhere in you buffers and hit esc-esc.
(global-set-key "\M-\e" 'dabbrev-expand)
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)

;;; ---------------------------------------------------------------------------
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

;;;
