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

(defcustom preview-transparent-color '(highlight :background)
  "Color to appear transparent in previews.
Set this to something unusual when using `preview-transparent-border',
to the default background in most other cases."
  :type '(radio (const :tag "None" nil)
		 (const :tag "Autodetect" t)
		 (color :tag "By name" :value "white")
		 (list :tag "Take from face"
		       :value (default :background)
		       (face)
		       (choice :tag "What to take"
			(const :tag "Background" :value :background)
			(const :tag "Foreground" :value :foreground))))
  :group 'preview-appearance)

(defcustom preview-transparent-border 1.5
  "Width of transparent border for previews in pt.
Setting this to a numeric value will add a border of
`preview-transparent-color' around images, and will turn
the heuristic-mask setting of images to default to 't since
then the borders are correctly detected even in case of
palette operations.  If the transparent color is something
not present otherwise in the image, the cursor display
will affect just this border.  A width of 0 is interpreted
by PostScript as meaning a single pixel, other widths are
interpreted as PostScript points (1/72 of 1in)"
  :group 'preview-appearance
  :type '(choice (const :value nil :tag "No border")
		 (number :value 1.5 :tag "Border width in pt")))

(defun preview-get-heuristic-mask ()
  "Get heuristic-mask to use for previews.
Consults `preview-transparent-color'."
  (cond ((stringp preview-transparent-color)
	 (color-values preview-transparent-color))
	((or (not (consp preview-transparent-color))
	     (integerp (car preview-transparent-color)))
	 preview-transparent-color)
	(t (color-values (preview-inherited-face-attribute
			  (nth 0 preview-transparent-color)
			  (nth 1 preview-transparent-color)
			  'default)))))

(defmacro preview-create-icon (file type ascent)
  "Create an icon from FILE, image TYPE and ASCENT."
  `(list 'image
	 :file ,file
	 :type ,type
	 :ascent ,ascent
	 :heuristic-mask (if preview-transparent-border t (preview-get-heuristic-mask))))

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
	      'keymap resmap)
	      res)
	     res)
	 '(resmap))))

(defun preview-int-bb (bb)
  "Make integer bounding box from possibly float BB."
  ;; Due to a bug in earlier Emacs versions, we make this a list instead
  ;; of a vector
  (when bb
    (list
     (floor (aref bb 0))
     (floor (aref bb 1))
     (ceiling (aref bb 2))
     (ceiling (aref bb 3)))))

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
		  :heuristic-mask (if preview-transparent-border t (preview-get-heuristic-mask)))))

(defvar preview-overlay nil)

(put 'preview-overlay
     'modification-hooks
     '(preview-handle-modification))

(put 'preview-overlay
     'insert-in-front-hooks
     '(preview-handle-insert-in-front))

;; We have to fake our way around atomicity, but at least this is more
;; efficient than the XEmacs version which has to cope with not being
;; able to use local change hooks at all.

;; Here is the beef: for best intuitiveness, we want to have
;; insertions be carried out as expected before iconized text
;; passages, but we want to insert *into* the overlay when not
;; iconized.  A preview that has become empty can not get content
;; again: we remove it.  A disabled preview needs no insert-in-front
;; handler.

(defun preview-handle-insert-in-front
  (ov after-change beg end &optional length)
  "Hook function for insert-in-front-hooks property."
  (when (and (not undo-in-progress) after-change)
    (if (eq (overlay-get ov 'preview-state) 'active)
	(move-overlay ov end (overlay-end ov))
      (overlay-put ov 'insert-in-front-hooks nil)
      (preview-disable ov))))

(defun preview-handle-modification
  (ov after-change beg end &optional length)
  "Hook function for modification-hooks property."
  (when after-change
    (if (and (eq (overlay-start ov) (overlay-end ov))
	     (not undo-in-progress))
	(preview-delete ov)
      (when (overlay-get ov 'insert-in-front-hooks)
	(overlay-put ov 'insert-in-front-hooks nil)
	(preview-disable ov)))))

(put 'preview-overlay 'isearch-open-invisible #'preview-toggle)

(put 'preview-overlay 'isearch-open-invisible-temporary
     #'preview-toggle)

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
	(preview-state
	 (if (if (eq (car args) 'toggle)
		 (null (eq (overlay-get ov 'preview-state) 'active))
	       (car args))
	     'active
	   'inactive))
	(strings (overlay-get ov 'strings)))
    (unless (eq (overlay-get ov 'preview-state) 'disabled)
      (overlay-put ov 'preview-state preview-state)
      (if (eq preview-state 'active)
	  (progn
	    (overlay-put ov 'category 'preview-overlay)
	    (overlay-put ov 'invisible t)
	    (if (eq (overlay-start ov) (overlay-end ov))
		(overlay-put ov 'before-string (car strings))
	      (dolist (prop '(display keymap mouse-face help-echo))
		(overlay-put ov prop
			     (get-text-property 0 prop (car strings))))
	      (overlay-put ov 'before-string nil))
	    (overlay-put ov 'face nil)
	    (with-current-buffer (overlay-buffer ov)
	      (add-hook 'pre-command-hook #'preview-mark-point nil t)
	      (add-hook 'post-command-hook #'preview-move-point nil t)))
	(dolist (prop '(display keymap mouse-face help-echo invisible))
	  (overlay-put ov prop nil))
	(overlay-put ov 'face 'preview-face)
	(overlay-put ov 'before-string (cdr strings)))
      (if old-urgent
	  (apply 'preview-add-urgentization old-urgent)))))

(defvar preview-marker (make-marker)
  "Marker for fake intangibility.")

(defun preview-mark-point ()
  "Mark position for fake intangibility."
;;  seems to hurt more than it helps.
;;  (when (eq (get-char-property (point) 'preview-state) 'active)
;;    (set-marker preview-marker (point))
;;    (preview-move-point))
  (set-marker preview-marker (point)))

(defun preview-move-point ()
  "Move point out of fake-intangible areas."
  (when (and (eq (marker-buffer preview-marker) (current-buffer))
	     (not disable-point-adjustment)
	     (not global-disable-point-adjustment))
    (let* ((pt (point))
	   (backward (< pt (marker-position preview-marker))))
      (while (catch 'loop
	       (dolist (ovr (overlays-at pt))
		 (when (and
			(eq (overlay-get ovr 'preview-state) 'active)
			(> pt (overlay-start ovr)))
		   (setq pt (if backward
				(overlay-start ovr)
			      (overlay-end ovr)))
		   (throw 'loop t))))
      nil)
      (goto-char pt))))

(defun preview-gs-color-value (value)
  "Return string to be used as color value for an RGB component.
Conversion from Emacs color numbers (0 to 65535) to GhostScript
floats."
  (format "%g" (/ value 65535.0)))

(defun preview-inherited-face-attribute (face attribute &optional
					      fallbacks)
  "Fetch face attribute while adhering to inheritance.
This searches FACE for an ATTRIBUTE.  If it is 'unspecified,
first inheritance is consulted (if FALLBACKS is non-NIL), then
FALLBACKS is searched if it is a face or a list of faces.
Relative specs are evaluated recursively until they get absolute or
are not resolvable.  Relative specs are float values."
  (let ((value (face-attribute face attribute)))
    (when fallbacks
      (setq fallbacks
	    (append
	     (let ((ancestors (face-attribute face :inherit)))
	       (cond ((facep ancestors) (list ancestors))
		     ((consp ancestors) ancestors)))
	     (cond ((facep fallbacks) (list fallbacks))
		   ((consp fallbacks) fallbacks)))))
    (cond ((null fallbacks) value)
	  ((floatp value)
	   (let ((avalue
		  (preview-inherited-face-attribute
		   (car fallbacks) attribute (or (cdr fallbacks) t))))
	     (cond ((integerp avalue)
		    (round (* avalue value)))
		   ((floatp avalue)
		    (* value avalue))
		   (t value))))
	  ((eq value 'unspecified)
	   (preview-inherited-face-attribute
	    (car fallbacks) attribute (or (cdr fallbacks) t)))
	  (t value))))

(defun preview-gs-get-colors ()
  "Return color setup tokens for GhostScript.
Fetches the current screen colors and makes a list of tokens
suitable for passing into GhostScript as arguments.
Pure borderless black-on-white will return NIL."
  (let
      ((bg (color-values (preview-inherited-face-attribute
			  'preview-reference-face :background 'default)))
       (fg (color-values (preview-inherited-face-attribute
			  'preview-reference-face :foreground 'default)))
       (mask (preview-get-heuristic-mask)))
    (if (equal '(65535 65535 65535) bg)
	(setq bg nil))
    (if (equal '(0 0 0) fg)
	(setq fg nil))
    (unless (and (numberp preview-transparent-border)
		 (consp mask) (integerp (car mask)))
      (setq mask nil))
    (append
     (if (or mask (and bg (not fg)))
	 '("gsave"))
     (if bg
	 (append
	  (mapcar #'preview-gs-color-value bg)
	  '("setrgbcolor" "clippath" "fill")))
     (if mask
	 (append
	  (mapcar #'preview-gs-color-value mask)
	  '("setrgbcolor" "false" "setstrokeadjust")
	  (list (number-to-string (* 2 preview-transparent-border)))
	  '("setlinewidth" "clippath" "strokepath" "fill")))
     (if (or mask (and bg (not fg)))
	 '("grestore"))
     (if fg
	 (append
	  (mapcar #'preview-gs-color-value fg)
	  '("setrgbcolor"))))))

(provide 'prv-emacs)
;;; prv-emacs.el ends here
