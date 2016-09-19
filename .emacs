;;; Example .emacs file
;;;
;;; Search for TODO to find things that must changed to your settings.
;;; Additional things might have to be changed to your liking.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; General environment
(add-to-list 'load-path "~/.emacs.d/elisp/")
(set-language-environment "Latin-1")
(setq default-major-mode 'text-mode)
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq completion-ignored-extensions
      (append '(".obj" ".exe" ".o" ".class" ".hex" ".cod" ".$$$" ".elc")
              completion-ignored-extensions))

;; keyboard scroll one line at a time
(setq scroll-step 1)

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
(global-set-key "\C-q" 'kill-buffer)
(global-set-key "\C-z" 'buffer-menu)
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
          (h-file-name (concat buffer-base-file-name ".h") (concat buffer-base-file-name "*.hpp"))
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

(defun my-font-lock-colors ()
  (set-face-foreground 'minibuffer-prompt "dark orange")
)
(add-hook 'font-lock-mode-hook 'my-font-lock-colors)

;;; ---------------------------------------------------------------------------
;;; Packages.

(require 'package-installer)

(defconst packages
  '(anzu
    company
    duplicate-thing
    ggtags
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    zygospore))

(install-packages packages)

;;; ---------------------------------------------------------------------------
;;; Programming mode stuff.

;; C/C++ stuff according to my own coding standards.
;;(require 'common-recommended)
(require 'setup-emacs-extensions)
(require 'common-recommended)

;; Turn on font-lock in all modes that support it

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
(setq font-lock-maximum-decoration t)

(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
                ("\\.c$" . c++-mode))
              auto-mode-alist))

;;; ---------------------------------------------------------------------------
;; Automitic settings:
