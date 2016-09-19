
(defun ps-print-code ()
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
  '("Print Code" . ps-print-code))
(define-key global-map "\C-cp" 'ps-print-code)

(provide 'printing)
