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

(defcustom TeX-fold-macro-spec-list
  '(("[f]" ("footnote"))
    ("[c]" ("cite")))
  "List of display strings and macros to fold."
  :type '(repeat (group (string :tag "Display String")
			(repeat :tag "Macros" (string))))
  :group 'TeX-fold)

(defcustom TeX-fold-env-spec-list
  '(("[comment]" ("comment")))
  "List of display strings and environments to fold."
  :type '(repeat (group (string :tag "Display String")
			(repeat :tag "Environments" (string))))
  :group 'TeX-fold)

(defcustom TeX-fold-unspec-macro-display-string "[m]"
  "Display string for unspecified macros.
This string will be displayed if a single macro is being hidden
which is not specified in `TeX-fold-macro-spec-list'."
  :type '(string)
  :group 'TeX-fold)

(defcustom TeX-fold-unspec-env-display-string "[env]"
  "Display string for unspecified environments.
This string will be displayed if a single environment is being
hidden which is not specified in `TeX-fold-env-spec-list'."
  :type '(string)
  :group 'TeX-fold)

(defcustom TeX-fold-unfold-around-mark t
  "Unfold text around the mark, if active."
  :type 'boolean
  :group 'TeX-fold)

(defface TeX-fold-display-string-face
  '((((class color) (background light))
     (:foreground "SlateBlue"))
    (((class color) (background dark))
     (:foreground "SlateBlue1"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:slant italic)))
  "Face for display strings."
  :group 'TeX-fold)

(defvar TeX-fold-display-string-face 'TeX-fold-display-string-face
  "Face for display strings.")

(defvar TeX-fold-open-spots nil)
(make-variable-buffer-local 'TeX-fold-open-spots)

(defvar TeX-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-o\C-b" 'TeX-fold-buffer)
    (define-key map "\C-c\C-o\C-m" 'TeX-fold-macro)
    (define-key map "\C-c\C-o\C-e" 'TeX-fold-env)
    (define-key map "\C-c\C-o\C-x" 'TeX-fold-clearout-buffer)
    (define-key map "\C-c\C-o\C-c" 'TeX-fold-clearout-item)
    map))

(defun TeX-fold-buffer ()
  "Hide all configured macros and environments in the current buffer.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (TeX-fold-clearout-buffer)
  (TeX-fold-buffer-type 'env)
  (TeX-fold-buffer-type 'macro))

(defun TeX-fold-buffer-type (type)
  "Fold all items of type TYPE in buffer.
TYPE can be one of the symbols 'env for environments or 'macro for macros."
  (save-excursion
    ;; All these `(eq type 'env)' tests are ugly, I know.
    (let ((fold-list (if (eq type 'env)
			 TeX-fold-env-spec-list
		       TeX-fold-macro-spec-list))
	  fold-item)
      (when (or (and (eq type 'env)
		     (or (eq major-mode 'latex-mode)
			 (eq major-mode 'doctex-mode)))
		(eq type 'macro))
	(while fold-list
	  (beginning-of-buffer)
	  (setq fold-item (car fold-list))
	  (setq fold-list (cdr fold-list))
	  (let ((display-string (nth 0 fold-item))
		(items (regexp-opt (nth 1 fold-item) t)))
	    (while (re-search-forward (if (eq type 'env)
					  (concat (regexp-quote TeX-esc)
						  "begin[ \t]*{" items "}")
					(concat (regexp-quote TeX-esc)
						items "\\b"))
				      nil t)
	      (let* ((item-start (match-beginning 0))
		     (item-end (if (eq type 'env)
				   (save-excursion
				     (goto-char (match-end 0))
				     (LaTeX-find-matching-end))
				 (save-excursion
				   (goto-char item-start)
				   (TeX-find-macro-end)))))
		(TeX-fold-make-overlay item-start item-end display-string)
		;; Jump to end of macro/env to avoid nested overlays
		;; as this will wreak havoc with display strings as
		;; long as the overlays are not prioritized.
		(goto-char item-end)))))))))

(defun TeX-fold-macro ()
  "Hide the macro on which point currently is located."
  (interactive)
  (TeX-fold-item 'macro))

(defun TeX-fold-env ()
  "Hide the environment on which point currently is located."
  (interactive)
  (TeX-fold-item 'env))

(defun TeX-fold-item (type)
  "Hide the item on which point currently is located.
TYPE specifies the type of item and can be one of the symbols
'env for environments or 'macro for macros."
  (if (and (eq type 'env)
	   (not (or (eq major-mode 'latex-mode)
		    (eq major-mode 'doctex-mode))))
      (message
       "Folding of environments is supported in LaTeX or docTeX mode only.")
    (let ((item-start (if (eq type 'env)
			  (condition-case nil
			      (save-excursion
				(LaTeX-find-matching-begin) (point))
			    (error nil))
			(TeX-find-macro-start))))
      (if (not item-start)
	  (message (if (eq type 'env)
		       "No environment found."
		     "No macro found."))
	(let* ((item-name (save-excursion
			    (goto-char item-start)
			    (looking-at (if (eq type 'env)
					    (concat (regexp-quote TeX-esc)
						    "begin[ \t]*{"
						    "\\([A-Za-z]+\\)}")
					  (concat (regexp-quote TeX-esc)
						  "\\([A-Za-z@]+\\)")))
			    (match-string-no-properties 1)))
	       (fold-list (if (eq type 'env)
			      TeX-fold-env-spec-list
			    TeX-fold-macro-spec-list))
	       fold-item
	       (display-string (or (catch 'found
				     (while fold-list
				       (setq fold-item (car fold-list))
				       (setq fold-list (cdr fold-list))
				       (when (member item-name (cadr fold-item))
					 (throw 'found (car fold-item)))))
				   ;; Item is not specified.
				   (if (eq type 'env)
				       TeX-fold-unspec-env-display-string
				     TeX-fold-unspec-macro-display-string)))
	       (item-end (if (eq type 'env)
			     (save-excursion
			       (goto-char (match-end 0))
			       (LaTeX-find-matching-end))
			   (save-excursion
			     (goto-char item-start)
			     (TeX-find-macro-end)))))
	  (TeX-fold-make-overlay item-start item-end display-string))))))

(defun TeX-fold-clearout-buffer ()
  "Permanently show all macros in the buffer"
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max))))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-clearout-item ()
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

(defun TeX-fold-make-overlay (ov-start ov-end display-string)
  "Make an overlay to cover the item and hide it.
The overlay will reach from OV-START to OV-END and will display
by DISPLAY-STRING.  The end of the overlay and its display string
may be altered to prevent overfull lines."
  (let* ((overfull (and (not (featurep 'xemacs)) ; Doesn't work on XEmacs
					         ; anyway.
			(save-excursion
			  (goto-char ov-end)
			  (search-backward "\n" ov-start t))
			(> (+ (- ov-start
				 (save-excursion
				   (goto-char ov-start)
				   (line-beginning-position)))
			      (- (save-excursion
				   (goto-char ov-end)
				   (line-end-position))
				 ov-end))
			   (current-fill-column))))
	 (ov-end (if overfull
		     (save-excursion
		       (goto-char ov-end)
		       (skip-chars-forward " \t")
		       (point))
		   ov-end))
	 (display-string (concat display-string (when overfull "\n"))))
    (let ((ov (make-overlay ov-start ov-end (current-buffer) t nil)))
      ;; Give environments a higher priority so that their display
      ;; string overrides those of possibly enclosed macros.
      (overlay-put ov 'priority (if (eq type 'env) 2 1))
      (TeX-fold-hide-item ov display-string))))

(defun TeX-fold-hide-item (ov &optional display-string)
  "Hide a single macro or environment.
Put the display string DISPLAY-STRING and other respective
properties onto overlay OV."
  (let ((display-string (or display-string
			    (overlay-get ov 'TeX-fold-display-string))))
    (overlay-put ov 'category 'TeX-fold)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'TeX-fold-display-string display-string)
    (if (featurep 'xemacs)
	(let ((glyph (make-glyph display-string)))
	  (overlay-put ov 'invisible t)
	  (set-glyph-property glyph 'face TeX-fold-display-string-face)
	  (set-extent-property ov 'end-glyph glyph))
      (overlay-put ov 'face TeX-fold-display-string-face)
      (overlay-put ov 'display display-string))))

(defun TeX-fold-show-item (ov)
  "Show a single LaTeX macro or environment.
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
			   (not (rassq ol TeX-fold-open-spots))
			   (or (not (featurep 'xemacs))
			       (and (featurep 'xemacs)
				    (not (extent-detached-p ol)))))
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
  nil nil TeX-fold-keymap
  (if TeX-fold-mode
      (progn
	(set (make-local-variable 'search-invisible) t)
	(add-hook 'post-command-hook 'TeX-fold-post-command nil t))
    (kill-local-variable 'search-invisible)
    (remove-hook 'post-command-hook 'TeX-fold-post-command t)
    (TeX-fold-clearout-buffer))
  (TeX-set-mode-name))

(provide 'tex-fold)

;;; tex-fold.el ends here
