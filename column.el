;;; column.el --- display line and column in the mode line

;; Copyright (C) 1993 Per Abrahamsen.
;; Copyright abandoned.  This file is donated to the public domain.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Version: 0.1
;; Bogus-Bureaucratic-Cruft: How 'bout ESR and the LCD people agreed
;; 	on a common format?

;;; Commentary:

;; LCD Archive Entry:
;; column|Per Abrahamsen|abraham@iesd.auc.dk|
;; Display line and column in the mode line|
;; 1993-08-13|0.1|~/misc/column.el.Z|

;; Requires FSF Emacs 19.

;;; Change Log:
;; Fri Aug 13 02:06:18 1993	Per Abrahamsen
;;      * Made current-column buffer local.
;; Tue Aug 10 10:00:00 1993	Per Abrahamsen
;;      * Created.

;;; Code:

;; Need remove-hook from lucid.
(require 'lucid)

;; String containing current column as last evaluated.
(defvar current-column "0")
(make-variable-buffer-local 'current-column)

;; Function updating the string containing the current column.
(defvar update-column-function 
  (function (lambda ()
	      (setq current-column (int-to-string (current-column)))
	      (set-buffer-modified-p (buffer-modified-p)))))

(defvar display-column-mode nil
  "Show current column and line in mode line if non-nil.")

;; Add to minor mode list.  Should really be part of mode-line-format.
(or (assq 'display-column-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(display-column-mode (" " current-column "/%l"))
		minor-mode-alist)))

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
	    (remove-hook 'post-command-hook update-column-function)
	    (setq display-column-mode nil)
	    (set-buffer-modified-p (buffer-modified-p))))
    ;;Turn it on
    (if display-column-mode
	()
      (add-hook 'post-command-hook update-column-function)
      (setq display-column-mode t))))

(provide 'column)

;;; column.el ends here
