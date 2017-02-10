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
(setq same-window-buffer-names '(*shell* *info* *Help*))

;; keyboard scroll one line at a time
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; Autosave every 500 typed characters
;;(setq auto-save-interval 500)
(setq auto-save-default nil)
;; Delay updates to give Emacs a chance for other changes
(setq linum-delay t)

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
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

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
(global-set-key [C-f9] 'go-unix-file)

;;; ---------------------------------------------------------------------------
;;; Colors and fonts.

;; TODO: Change colors and fonts to your liking.

(defun my-font-lock-colors ()
  (set-face-foreground 'minibuffer-prompt "dark orange"))
(add-hook 'font-lock-mode-hook 'my-font-lock-colors)

;; Turn on font-lock in all modes that support it

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
(setq font-lock-maximum-decoration t)

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
    helm-company
    helm-dash
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
    zygospore
    sr-speedbar
    markdown-mode
    jdee
    javadoc-lookup
    java-imports))

(install-packages packages)

;;; ---------------------------------------------------------------------------
;;; Programming mode stuff.

;;; Language supported
(require 'ide-support)
(require 'browser-support)
(require 'c++-support)
(require 'java-support)

;;; ---------------------------------------------------------------------------
;;; Automatic settings:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (jtags java-imports javadoc-lookup helm-dash markdown-mode zygospore yasnippet ws-butler volatile-highlights undo-tree sr-speedbar smartparens jdee iedit helm-swoop helm-projectile helm-gtags helm-company ggtags function-args duplicate-thing dtrt-indent comment-dwim-2 clean-aindent-mode anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
