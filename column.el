;;; column.el --- display line and column in the mode line

;; Copyright (C) 1993 Per Abrahamsen.
;; Copyright abandoned.  This file is donated to the public domain.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Version: 0.3
;; Bogus-Bureaucratic-Cruft: How 'bout ESR and the LCD people agreed
;; 	on a common format?

;; LCD Archive Entry:
;; column|Per Abrahamsen|abraham@iesd.auc.dk|
;; Display line and column in the mode line|
;; 1993-12-31|0.3|~/misc/column.el.Z|

;;; Commentary:

;; Save this file in your load-path, insert
;;   (autoload 'display-column-mode "column" nil t)
;; in your `.emacs' file, restart FSF Emacs 19 then activate with 
;;   M-x display-column-mode RET

;;; Change Log:
;;
;; Fri Dec 31 14:02:47 1993
;;      * Replace update-column-function variable with
;;        do-update-column function.
;; Fri Dec 31 13:46:41 1993
;;      * Change mode-line-format directly instead of using a 
;;        minor mode.
;; Thu Dec 16 14:57:15 1993
;;      * Removed (require 'lucid) as unnecessary.
;; Fri Aug 13 02:06:18 1993	Per Abrahamsen
;;      * Made current-column buffer local.
;; Tue Aug 10 10:00:00 1993	Per Abrahamsen
;;      * Created.

;;; Code:

;; String containing current column as last evaluated.
(defvar current-column "0")
(make-variable-buffer-local 'current-column)

(defun do-update-column ()
  ;; Update the string containing the current column.
  (setq current-column (int-to-string (current-column)))
  (set-buffer-modified-p (buffer-modified-p)))

(defvar display-column-mode nil
  "Show current column and line in mode line if non-nil.")

(defvar display-column-format '(current-column "/%l--")
  "Format for displaying the column in the mode line.")

;; Entry for column mode in mode line.
(defconst display-column-entry
  (list 'display-column-mode (cons "" display-column-format)))

(defvar display-column-after ")%]--"
  "Display column after this element in the mode line.")

;; Add display-column-format to mode-line-format after display-column-after.
(or (member display-column-entry mode-line-format)
    (let ((entry (member display-column-after mode-line-format)))
      (setcdr entry (cons display-column-entry (cdr entry)))))

;;;###autoload
(defun display-column-mode (&optional arg)
  "Toggle display column mode.
With prefix arg, turn display column mode on iff arg is positive.

When display column mode is on, the current column and line number are
displayed in the mode line."
  (interactive "P")
  (if (or (and (null arg) display-column-mode)
	  (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if display-column-mode
	  (progn
	    (remove-hook 'post-command-hook 'do-update-column)
	    (setq display-column-mode nil)
	    (set-buffer-modified-p (buffer-modified-p))))
    ;;Turn it on
    (if display-column-mode
	()
      (add-hook 'post-command-hook 'do-update-column)
      (setq display-column-mode t))))

(provide 'column)

;;; column.el ends here
