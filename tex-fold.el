;;; tex-fold.el --- Fold TeX macros.

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auc-tex@sunsite.dk
;; Created: 2004-07-04
;; Keywords: tex, wp

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This file provides support for hiding and unhiding TeX, LaTeX,
;; ContTeXt, Texinfo and similar macros inside of AUCTeX.

;;; Code:

(when (featurep 'xemacs)
  (require 'overlay))

(defgroup TeX-fold nil
  "Fold TeX macros."
  :group 'AUCTeX)

(defcustom TeX-fold-spec-list
  '(("[F]" ("footnote"))
    ("[C]" ("cite")))
  "List of display strings and macros to fold."
  :type '(repeat (group (string :tag "Display String")
			(repeat :tag "Macros" (string))))
  :group 'TeX-fold)

(defcustom TeX-fold-unfold-around-mark t
  "Unfold text around the mark, if active."
  :type 'boolean
  :group 'TeX-fold)

(defface TeX-fold-display-string-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "SlateBlue"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "SlateBlue1"))
    (((class color) (min-colors 8))
     (:foreground "blue"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:slant italic)))
  "Face for display strings."
  :group 'TeX-fold)

(defvar TeX-fold-display-string-face 'TeX-fold-display-string-face
  "Face name for display strings.")

(defvar TeX-fold-open-spots nil)
(make-variable-buffer-local 'TeX-fold-open-spots)

(defvar TeX-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-o\C-o" 'TeX-fold-buffer)
    (define-key map "\C-c\C-o\C-a" 'TeX-fold-clearout-buffer)
    (define-key map "\C-c\C-o\C-c" 'TeX-fold-macro)
    (define-key map "\C-c\C-o\C-e" 'TeX-fold-clearout-macro)
    map))

(defun TeX-fold-buffer ()
  "Hide all macros specified in the variable `TeX-fold-spec-list'."
  (interactive)
  (TeX-fold-clearout-buffer)
  (save-excursion
    (let ((fold-list TeX-fold-spec-list)
	  fold-item)
      (while fold-list
	(beginning-of-buffer)
	(setq fold-item (car fold-list))
	(setq fold-list (cdr fold-list))
	(let ((display-string (nth 0 fold-item))
	      (macros (regexp-opt (nth 1 fold-item) t)))
	  (while (re-search-forward (concat (regexp-quote TeX-esc)
					    macros "\\b") nil t)
	    (let ((ov (make-overlay (match-beginning 0)
				    (save-excursion
				      (goto-char (match-beginning 0))
				      (TeX-find-macro-end))
				    (current-buffer) t nil)))
	      (TeX-fold-hide-item ov display-string))))))))

(defun TeX-fold-macro ()
  "Hide the macro on which point currently is located."
  (interactive)
  (let ((macro-start (TeX-find-macro-start)))
    (if (not macro-start)
	(message "No macro found.")
      (let* ((macro-name (save-excursion
			   (goto-char macro-start)
			   (looking-at (concat (regexp-quote TeX-esc)
					       "\\([A-Za-z@]+\\)"))
			   (match-string 1)))
	     (fold-list TeX-fold-spec-list)
	     fold-item
	     (display-string (progn
			       (catch 'found
				 (while fold-list
				   (setq fold-item (car fold-list))
				   (setq fold-list (cdr fold-list))
				   (when (member macro-name (cadr fold-item))
				     (throw 'found (car fold-item))))))))
	(if (not display-string)
	    (message "Macro not specified in variable `TeX-fold-spec-list'.")
	  (let ((ov (make-overlay macro-start
				  (save-excursion
				    (goto-char macro-start)
				    (TeX-find-macro-end))
				  (current-buffer) t nil)))
	    (TeX-fold-hide-item ov display-string)))))))

(defun TeX-fold-clearout-buffer ()
  "Permanently show all macros in the buffer"
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max))))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-clearout-macro ()
  "Permanently show the macro on which point currently is located."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-remove-overlays (overlays)
  "Remove all overlays set by TeX-fold in OVERLAYS."
  (while overlays
    (when (eq (overlay-get (car overlays) 'category) 'TeX-fold)
      (delete-overlay (car overlays)))
    (setq overlays (cdr overlays))))

(defun TeX-fold-hide-item (ov &optional display-string)
  "Hide a single LaTeX macro.
Put the display string DISPLAY-STRING and other respective
properties onto overlay OV."
  (let ((display-string (or display-string
			    (overlay-get ov 'TeX-fold-display-string))))
    (overlay-put ov 'category 'TeX-fold)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'TeX-fold-display-string display-string)
    (overlay-put ov 'face TeX-fold-display-string-face)
    (if (featurep 'xemacs)
	(progn
	  (overlay-put ov 'invisible t)
	  (set-extent-property ov 'end-glyph (make-glyph display-string)))
      (overlay-put ov 'display display-string))))

(defun TeX-fold-show-item (ov)
  "Show a single LaTeX macro.
Remove the respective properties from the overlay OV."
  (overlay-put ov 'face nil)
  (if (featurep 'xemacs)
      (progn
	(set-extent-property ov 'end-glyph nil)
	(overlay-put ov 'invisible nil))
    (overlay-put ov 'display nil)))

;; Copy and adaption of `reveal-post-command' from reveal.el in GNU
;; Emacs on 2004-07-04.
(defun TeX-fold-post-command ()
  ;; `with-local-quit' is not supported in XEmacs.
  (condition-case nil
      (let ((inhibit-quit nil))
	(condition-case err
	    (let* ((spots (TeX-fold-partition-list
			   (lambda (x)
			     ;; We refresh any spot in the current
			     ;; window as well as any spots associated
			     ;; with a dead window or a window which
			     ;; does not show this buffer any more.
			     (or (eq (car x) (selected-window))
				 (not (window-live-p (car x)))
				 (not (eq (window-buffer (car x))
					  (current-buffer)))))
			   TeX-fold-open-spots))
		   (old-ols (mapcar 'cdr (car spots))))
	      (setq TeX-fold-open-spots (cdr spots))
	      (when (or (and (boundp 'disable-point-adjustment)
			     disable-point-adjustment)
			(and (boundp 'global-disable-point-adjustment)
			     global-disable-point-adjustment)
			;; See preview.el on how to make this configurable.
			(memq this-command (list (key-binding [left])
						 (key-binding [right]))))
		;; Open new overlays.
		(dolist (ol (nconc (when (and TeX-fold-unfold-around-mark
					      (boundp 'mark-active)
					      mark-active)
				     (overlays-at (mark)))
				   (overlays-at (point))))
		  (when (eq (overlay-get ol 'category) 'TeX-fold)
		    (push (cons (selected-window) ol) TeX-fold-open-spots)
		    (setq old-ols (delq ol old-ols))
		    (TeX-fold-show-item ol))))
	      ;; Close old overlays.
	      (dolist (ol old-ols)
		(when (and (eq (current-buffer) (overlay-buffer ol))
			   (not (rassq ol TeX-fold-open-spots)))
		  (if (and (>= (point) (overlay-start ol))
			   (<= (point) (overlay-end ol)))
		      ;; Still near the overlay: keep it open.
		      (push (cons (selected-window) ol) TeX-fold-open-spots)
		    ;; Really close it.
		    (TeX-fold-hide-item ol)))))
	  (error (message "TeX-fold: %s" err))))
    (quit (setq quit-flag t))))

;; Copy and adaption of `cvs-partition' from pcvs-util.el in GNU Emacs
;; on 2004-07-05 to make latex-fold.el mainly self-contained.
(defun TeX-fold-partition-list (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))

(define-minor-mode TeX-fold-mode
  "Toggle TeX-fold-mode on or off.
TeX-fold-mode lets you hide and unhide LaTeX macros.

Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  nil (global-TeX-fold-mode nil " Fold") TeX-fold-keymap
  (if TeX-fold-mode
      (progn
	(set (make-local-variable 'search-invisible) t)
	(add-hook 'post-command-hook 'TeX-fold-post-command nil t))
    (kill-local-variable 'search-invisible)
    (TeX-fold-clearout-buffer)
    (remove-hook 'post-command-hook 'TeX-fold-post-command t)))

(provide 'tex-fold)

;;; tex-fold.el ends here
