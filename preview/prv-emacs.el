;;; prv-emacs.el --- GNU Emacs specific code for preview.el

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: David Kastrup <David.Kastrup@t-online.de>
;; Keywords: convenience, tex, wp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(defvar preview-compatibility-macros nil
  "List of macros only present when compiling/loading.")

(defimage preview-nonready-icon ((:type xpm :file "help.xpm" :ascent 80)
				 (:type pbm :file "help.pbm" :ascent
					80))
  "The symbol used for previews to be generated.
Usually a question mark")

(defimage preview-icon ((:type xpm :file "search.xpm" :ascent 100)
			(:type pbm :file "search.pbm" :ascent 100))
  "The symbol used for an open preview.
Usually a magnifying glass.")

(defmacro preview-create-icon (file type ascent)
  "Create an icon from FILE, image TYPE and ASCENT."
  `(list 'image
	 :file ,file
	 :type ,type
	 :ascent ,ascent
	 :heuristic-mask t))

(defun preview-add-urgentization (fun ov buff)
  "Cause FUN to be called with OV and BUFF when redisplayed."
  (let ((dispro (overlay-get ov 'display)))
    (unless (eq (car dispro) 'when)
      (overlay-put ov 'display `(when (,fun ,ov ,buff) . ,dispro)))))

(defun preview-remove-urgentization (ov)
  "Undo urgentization of OV by `preview-add-urgentization'.
Returns the old arguments to `preview-add-urgentization'
if there was any urgentization."
  (let ((dispro (overlay-get ov 'display)))
    (when (eq (car dispro) 'when)
      (prog1
	  (car (cdr dispro))
	  (overlay-put ov 'display (cdr (cdr dispro)))))))

(defmacro preview-image-from-icon (icon)
  "Generate a copy of the ICON that is \"editable\".
Which means that `preview-replace-icon-image' can be called
on the value returned here, and wherever the value was used,
the new image will appear, while ICON itself is not changed.
Typical would be a CONS-cell."
  `(cons 'image (cdr ,icon)))

(defmacro preview-string-from-image (image)
  "Make a string displaying IMAGE."
  `(propertize "x" 'display ,image))

(defmacro preview-replace-icon (icon replacement)
  "Replace an ICON representation by REPLACEMENT, another icon."
  `(setcdr ,icon (cdr ,replacement)))

(defvar preview-button-1 [mouse-2])
(defvar preview-button-2 [mouse-3])

(defmacro preview-make-clickable (&optional map string helpstring click1 click2)
  "Generate a clickable string or keymap.
If MAP is non-nil, it specifies a keymap to add to, otherwise
a new one is created.  If STRING is given, the result is made
a property of it.  In that case, HELPSTRING is a format string
with one or two %s specifiers for preview's clicks, displayed
as a help-echo.  CLICK1 and CLICK2 are functions to call
on preview's clicks."
  `(let (,@(if string `((res (copy-sequence ,string))))
	   (resmap ,(or map '(make-sparse-keymap))))
     ,@(if click1
	   `((define-key resmap ,preview-button-1 ,click1)))
     ,@(if click2
	   `((define-key resmap ,preview-button-2 ,click2)))
     ,@(if string
	   `((add-text-properties
	      0 (length res)
	      (list 'mouse-face 'highlight
	      'help-echo (format ,helpstring preview-button-1 preview-button-2)
	      'local-map resmap)
	      res)
	     res)
	 '(resmap))))

(defun preview-ps-image (filename scale)
  "Place a PostScript image directly by Emacs.
This uses Emacs built-in PostScript image support for
rendering the preview image in EPS file FILENAME, with
a scale factor of SCALE indicating the relation of desired
image size on-screen to the size the PostScript code
specifies."
  (let ((bb (preview-extract-bb filename)))
;; should the following 2 be rather intbb?
    (create-image filename 'postscript nil
		  :pt-width (round
			     (* scale (- (aref bb 2) (aref bb 0))))
		  :pt-height (round
			      (* scale (- (aref bb 3) (aref bb 1))))
		  :bounding-box (preview-int-bb bb)
		  :ascent (preview-ascent-from-bb bb)
		  :heuristic-mask '(65535 65535 65535)
		  )
    ))

(defvar preview-overlay nil)

(put 'preview-overlay
     'modification-hooks
     '(preview-handle-modification))

(put 'preview-overlay
     'insert-in-front-hooks
     '(preview-handle-insert-in-front))

(put 'preview-overlay
     'intangible t)

;; We have to fake our way around atomicity, but at least this is more
;; efficient than the XEmacs version which has to cope with not being
;; able to use local change hooks at all.

(defun preview-handle-insert-in-front
  (ov after-change beg end &optional length)
  "Hook function for insert-in-front-hooks property."
  (when after-change
    (if (overlay-get ov 'intangible)
	(move-overlay ov end (overlay-end ov))
      (overlay-put ov 'modification-hooks nil)
      (overlay-put ov 'insert-in-front-hooks nil)
      (preview-disable ov))))

(defun preview-handle-modification
  (ov after-change beg end &optional length)
  "Hook function for modification-hooks property."
  (when after-change
    (overlay-put ov 'modification-hooks nil)
    (overlay-put ov 'insert-in-front-hooks nil)
    (if (overlay-get ov 'intangible)
	(progn
	  (kill-region (overlay-start ov) (overlay-end ov))
	  (preview-delete ov))
      (preview-disable ov))))

(put 'preview-overlay 'isearch-open-invisible 'preview-toggle)

(put 'preview-overlay 'isearch-open-invisible-temporary 'preview-toggle)

(defun preview-toggle (ov &rest args)
  "Toggle visibility of preview overlay OV.
ARGS can be one of the following, in order to make this most
useful for isearch hooks: nothing, which means making the
overlay contents visible.  nil currently means the same (this
is the value used when isearch temporarily opens the overlay)
but could change in future, for example displaying both preview
and source text.  t makes the contents invisible, and 'toggle
toggles it."
  (let ((old-urgent (preview-remove-urgentization ov))
	(intangible
	 (if (eq 'toggle (car args))
	     (not (overlay-get ov 'intangible))
	   (car args)))
	(strings (overlay-get ov 'strings)))
    (overlay-put ov 'intangible intangible)
    (if intangible
	(progn
	  (dolist (prop '(display local-map mouse-face help-echo))
	    (overlay-put ov prop
			 (get-text-property 0 prop (car strings))))
	  (overlay-put ov 'before-string nil)
	  (overlay-put ov 'face nil))
      (dolist (prop '(display local-map mouse-face help-echo))
	(overlay-put ov prop nil))
      (overlay-put ov 'face 'preview-face)
      (overlay-put ov 'before-string (cdr strings)))
    (if old-urgent
	(apply 'preview-add-urgentization old-urgent))))


(provide 'prv-emacs)
;;; prv-emacs.el ends here
