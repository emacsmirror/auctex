;;; prv-xemacs.el --- XEmacs support for preview-latex

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

(require 'overlay)

;; Compatibility macros and functions.

(eval-when-compile
  (defvar preview-compatibility-macros nil
    "List of macros only present when compiling/loading.")

  (defmacro preview-defmacro (name &rest rest)
    (unless (fboundp name)
      (push name preview-compatibility-macros)
      `(defmacro ,name ,@rest)))
  (push 'preview-defmacro preview-compatibility-macros))

(preview-defmacro assoc-default (key alist test)
  `(cdr (assoc* ,key ,alist
                :test #'(lambda(a b) (funcall ,test b a)))))

(preview-defmacro display-mm-height () '(device-mm-height))
(preview-defmacro display-mm-width () '(device-mm-width))
(preview-defmacro display-pixel-height () '(device-pixel-height))
(preview-defmacro display-pixel-width () '(device-pixel-width))
(preview-defmacro line-beginning-position () '(point-at-bol))
(preview-defmacro line-end-position () '(point-at-eol))

;; This is not quite the case, but unless we're playing with duplicable extents,
;; the two are equivalent in XEmacs.
(unless (fboundp 'match-string-no-properties)
  (define-compatible-function-alias 'match-string-no-properties 'match-string))

(preview-defmacro easy-menu-create-menu (menu-name menu-items)
  "Return a menu called MENU-NAME with items described in MENU-ITEMS.
MENU-NAME is a string, the name of the menu.  MENU-ITEMS is a list of items
as described in `easy-menu-define'. The syntax of the list returned
is suitable for passing to `easy-menu-define' or `easy-menu-add-item'."
  `(list ,menu-name ,@(eval menu-items)))

(preview-defmacro face-attribute (face attr)
  `(cond
    ((eq ,attr :height)
     (round (/ (* 720.0 (face-height ,face) (device-mm-height)) (device-pixel-height) 25.4)))
    ((eq ,attr :foreground)
     (face-foreground-instance ,face))
    ((eq ,attr :background)
     (face-background-instance ,face))
    (t
     (error 'unimplemented (concat "Don't know how to fake " (symbol-name ,attr))))))

(preview-defmacro make-temp-file (prefix dir-flag)
  (if (not dir-flag)
      (error 'unimplemented "Can only fake make-temp-file for directories"))
  `(let (file)
     (while (condition-case ()
                (progn
                  (setq file
                        (make-temp-name ,prefix))
                  (make-directory file)
                  nil)
              (file-already-exists t))
       nil)
     file))

(preview-defmacro set-buffer-multibyte (multibyte)
  "Set the representation type of the current buffer.  If MULTIBYTE
is non-`nil', the buffer becomes multibyte.  If MULTIBYTE is
`nil', the buffer becomes unibyte.

Because XEmacs does not implement multibyte versus unibyte buffers
per se (they just have encodings which may be unibyte or multibyte),
this is only implemented for the `nil' case."
  (if (not multibyte)
      `(if (fboundp 'set-buffer-file-coding-system)
           (set-buffer-file-coding-system 'binary))
    (error 'unimplemented "`set-buffer-multibyte is only implemented for the binary case.")))

(preview-defmacro next-single-char-property-change (pos prop)
  "Return the position of next property change for a specific property.
This is like `next-single-property-change', except that if no
change is found before the end of the buffer, it returns
\(point-max) rather than `nil'."
  `(or (next-single-property-change ,pos ,prop)
       (point-max)))

(preview-defmacro previous-single-char-property-change (pos prop)
  "Return the position of previous property change for a specific property.
This is like `next-single-property-change', except that if no
change is found before the end of the buffer, it returns
\(point-min) rather than `nil'."
  `(or (previous-single-property-change ,pos ,prop)
       (point-min)))

(preview-defmacro with-temp-message (message &rest body)
  "Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area.

The message is displayed with label `progress'; see `display-message'."
  (let ((current-message (make-symbol "current-message"))
        (temp-message (make-symbol "with-temp-message")))
    `(let ((,temp-message ,message)
           (,current-message))
       (unwind-protect
           (progn
             (when ,temp-message
               (setq ,current-message (current-message))
               (display-message 'progress ,temp-message))
             ,@body)
         (and ,temp-message
              (if ,current-message
                  (display-message 'progress ,current-message)
                (message nil)))))))

(defun preview-mark-active ()
  "Return t if the mark is active."
  (and (mark)
       t))

;; Stuff missing from XEmacs that should really be there.
;; In time, this will hopefully all migrate into XEmacs.

; XEmacs's `add-to-list' takes only two arguments.
(defun add-to-list (list-var element &optional append)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
         (if append
             (append (symbol-value list-var) (list element))
           (cons element (symbol-value list-var))))))

;; Replacement for `extents-at', which does not exist in XEmacs-21.1.
(defsubst preview-extents-at (pos &optional object property)
  "Returns all extents at POS in OBJECT which have PROPERTY set."
  (let ((extents))
    (map-extents #'(lambda (extent unused)
                     (setq extents (nconc extents (list extent))))
                 object pos (1+ pos) nil 'start-or-end-in-region property)))

;; We have to pick the image-specifier's inst-list apart by hand.  This code
;; won't work if there are multiple valid image types in a single glyph in a
;; single (locale * domain): but we don't care, because only the first of those
;; will ever be rendered (and make-glyph probably can't create such images
;; anyway).
;; How horrible.

(defmacro glyph-image-type (glyph)
  "Return GLYPH's image type."
  `(map-specifier (glyph-image ,glyph)
                  #'(lambda (spec locale inst-list null)
                      (elt (cdr (assoc (list (device-type)) inst-list)) 0))))

;; Images.

;; TODO: Generalize this so we can create the fixed icons using it.

;; Argh, dired breaks :file :(
;; This is a temporary kludge to get around that until a fixed dired
;; or a fixed XEmacs is released.
(defmacro preview-create-icon (file type ascent)
  "Create an icon from FILE, image TYPE and ASCENT."
  `(let ((glyph
          (make-glyph
           (vector ,type
                   :file ,file
                   :data (with-temp-buffer
                           (insert-file-contents-literally ,file)
                           (buffer-string))))))
     (set-glyph-baseline glyph ,ascent)
     glyph))

(defvar preview-nonready-icon
  (let ((glyph-xpm-filename (locate-data-file "prevwork.xpm"))
        (glyph-xbm-filename (locate-data-file "prevwork.xbm")))
    (if (not glyph-xpm-filename)
        (cerror 'file-error "Image not installed" "prevwork.xpm"))
    (if (not glyph-xbm-filename)
        (cerror 'file-error "Image not installed" "prevwork.xbm"))
    (let ((glyph
           (make-glyph
            (list
             `[xpm :file ,glyph-xpm-filename]
             `[xbm :file ,glyph-xbm-filename]))))
      (set-glyph-baseline glyph 90)
      glyph))
  "The symbol used for previews to be generated.")

(defvar preview-error-icon
  (let ((glyph-xpm-filename (locate-data-file "preverr.xpm"))
        (glyph-xbm-filename (locate-data-file "preverr.xbm")))
    (if (not glyph-xpm-filename)
        (cerror 'file-error "Image not installed" "preverr.xpm"))
    (if (not glyph-xbm-filename)
        (cerror 'file-error "Image not installed" "preverr.xbm"))
    (let ((glyph
           (make-glyph
            (list
             `[xpm :file ,glyph-xpm-filename]
           `[xbm :file ,glyph-xbm-filename]))))
      (set-glyph-baseline glyph 90)
      glyph))
  "The symbol used for PostScript errors.")

(defvar preview-icon
  (let ((glyph-xpm-filename (locate-data-file "preview.xpm"))
        (glyph-xbm-filename (locate-data-file "preview.xbm")))
    (if (not glyph-xpm-filename)
        (cerror 'file-error "Image not installed" "preview.xpm"))
    (if (not glyph-xbm-filename)
        (cerror 'file-error "Image not installed" "preview.xbm"))
    (let ((glyph
           (make-glyph
            (list
             `[xpm :file ,glyph-xpm-filename]
             `[xbm :file ,glyph-xbm-filename]))))
      (set-glyph-baseline glyph 75)
      glyph))
  "The symbol used for an open preview.")

;; Image frobbing.

(defun preview-add-urgentization (fun ov &rest rest)
  "Cause FUN (function call form) to be called when redisplayed.
FUN must be a form with OV as first argument,
REST as the remainder, returning T.  An alternative is to give
what `preview-remove-urgentization' returns, this will reinstate
the previous state."
  (set-extent-initial-redisplay-function
   ov
   (if (null rest)
       fun
     `(lambda (ov) (,fun ,ov ,@rest)))))

(defun preview-remove-urgentization (ov)
  "Undo urgentization of OV by `preview-add-urgentization'.
Returns the old arguments to `preview-add-urgentization'
if there was any urgentization."
  (prog1 (list (extent-property ov 'initial-redisplay-function) ov)
    (set-extent-initial-redisplay-function ov nil)))

(defmacro preview-nonready-copy ()
  "Prepare for a later call of `preview-replace-active-icon'."
  'preview-nonready-icon)

(defmacro preview-replace-active-icon (ov replacement)
  "Replace the active Icon in OV by REPLACEMENT, another icon."
  `(let ((replacement ,replacement))
     (set-extent-property ,ov 'preview-image replacement)
     (add-text-properties 0 1 (list 'end-glyph replacement)
			  (car (extent-property ,ov 'strings)))
     (if (eq (extent-property ,ov 'preview-state) 'active)
	 (set-extent-end-glyph ,ov replacement))))

(defvar preview-button-1 'button2)
(defvar preview-button-2 'button3)

;; The `x' and invisible junk is because XEmacs doesn't bother to insert
;; the extents of a zero-length string. Bah.
;; When this is fixed, we'll autodetect this case and use zero-length
;; strings where possible.
(defmacro preview-make-clickable (&optional map glyph helpstring click1 click2)
  "Generate a clickable string or keymap.
If MAP is non-nil, it specifies a keymap to add to, otherwise
a new one is created.  If GLYPH is given, the result is made
to display it wrapped in a string.  In that case,
HELPSTRING is a format string with one or two %s specifiers
for preview's clicks, displayed as a help-echo.  CLICK1 and CLICK2
are functions to call on preview's clicks."
  `(let (,@(and glyph '((res (copy-sequence "x"))))
           (resmap ,(or map '(make-sparse-keymap))))
     ,@(if click1
           `((define-key resmap preview-button-1 ,click1)))
     ,@(if click2
           `((define-key resmap preview-button-2 ,click2)))
     ,@(if glyph
	   `((add-text-properties
              0 1
              (list 'end-glyph ,glyph
		    'mouse-face 'highlight
              'preview-balloon-help
	      ,(if (stringp helpstring)
		   (format helpstring preview-button-1 preview-button-2)
		 `(format ,helpstring preview-button-1 preview-button-2))
              'preview-keymap resmap)
              res)
             res)
	 '(resmap))))

(defun preview-click-reroute (ov event)
  "If OV received a click EVENT on a glyph, reroute to special map."
  (let ((oldmap (extent-keymap ov)))
    (unwind-protect
	(progn
	  (set-extent-keymap ov
			     (and (event-over-glyph-p event)
				  (extent-property ov 'preview-keymap)))
	  (dispatch-event event))
      (set-extent-keymap ov oldmap))))

(defun preview-reroute-map (ov)
  "Get rerouting keymap for OV for catching glyph clicks only."
  (let ((map (make-sparse-keymap))
	(fun `(lambda (event)
		(interactive "e")
		(preview-click-reroute ,ov event))))
    (define-key map preview-button-1 fun)
    (define-key map preview-button-2 fun)
    map))

(defun preview-balloon-reroute (ov)
  "Give balloon help only if over glyph of OV."
  (and (eq ov (event-glyph-extent (mouse-position-as-motion-event)))
       (extent-property ov 'preview-balloon-help)))

(defun preview-ps-image (filename scale &optional box)
  "Place a PostScript image directly by Emacs.
This uses XEmacs built-in PostScript image support for
rendering the preview image in EPS file FILENAME, with
a scale factor of SCALE indicating the relation of desired
image size on-screen to the size the PostScript code
specifies.  If BOX is present, it is the bounding box info.

Since there is, as yet, no such support, this is stubbed out.
This will not be so forever."
  (error 'image-conversion-error "PostScript images are not supported."))

;; Most of the changes to this are junking the use of overlays;
;; a bit of it is different, and there's a little extra paranoia.

;; We also have to move the image from the begin to the end-glyph
;; whenever the extent is invisible because of a bug in XEmacs-21.4's
;; redisplay engine.
(defun preview-toggle (ov &optional arg)
  "Toggle visibility of preview overlay OV.
ARG can be one of the following: t displays the overlay,
nil displays the underlying text, and 'toggle toggles."
  (if (not (bufferp (extent-object ov)))
      (error 'wrong-type-argument ov))
  (let ((old-urgent (preview-remove-urgentization ov))
        (preview-state
         (if (if (eq arg 'toggle)
                 (not (eq (extent-property ov 'preview-state) 'active))
               arg)
             'active
           'inactive))
        (strings (extent-property ov 'strings)))
    (unless (eq (extent-property ov 'preview-state) 'disabled)
      (set-extent-property ov 'preview-state preview-state)
      (if (eq preview-state 'active)
          (progn
	    (unless (extent-keymap ov)
	      (set-extent-keymap ov (preview-reroute-map ov))
	      (set-extent-property ov 'balloon-help #'preview-balloon-reroute))
	    (set-extent-begin-glyph ov nil)
	    (set-extent-end-glyph-layout ov 'text)
	    (set-extent-end-glyph ov (get-text-property
				      0 'end-glyph (car strings)))
            (set-extent-properties ov '(invisible t
					isearch-open-invisible ignore
					isearch-invisible t
                                        face nil))
	    (dolist (prop '(preview-keymap
			    mouse-face preview-balloon-help))
              (set-extent-property ov prop
                                   (get-text-property 0 prop (car strings)))))
	(unless (cdr strings)
	  (setcdr strings (preview-inactive-string ov)))
	(set-extent-end-glyph ov nil)
	(set-extent-begin-glyph-layout ov 'text)
	(set-extent-begin-glyph ov (get-text-property
				    0 'end-glyph (cdr strings)))
        (set-extent-properties ov `(face preview-face
				    mouse-face nil
				    invisible nil
				    isearch-invisible nil
				    preview-keymap
				    ,(get-text-property
				     0 'preview-keymap (cdr strings))
				    preview-balloon-help
				    ,(get-text-property
				     0 'preview-balloon-help (cdr strings)))))
      (if old-urgent
          (apply 'preview-add-urgentization old-urgent)))))

(defun preview-gs-color-value (value)
  "Return string to be used as color value for an RGB component.
Conversion from Emacs color numbers (0 to 65535) in VALUE
to GhostScript floats."
  (format "%g" (/ value 65535.0)))

; Does FALLBACKS need to be implemented? Likely not.
(defmacro preview-inherited-face-attribute (face attribute &optional
                                              fallbacks)
  "Fetch face attribute while adhering to inheritance.
This searches FACE and all its ancestors for an ATTRIBUTE.
FALLBACKS is unused."
  `(face-attribute ,face ,attribute))

(defmacro preview-with-LaTeX-menus (&rest bodyforms)
  "Activates the LaTeX menus for the BODYFORMS.
This makes it possible to add to them.

Because of a bug in easymenu.el, we can only add items to the
current menubar.  So we temporarily add the TeX and LaTeX menus
to the current menubar.  This is a quite appalling kludge."
  `(let* ((current-menubar (list LaTeX-mode-menu TeX-mode-menu)))
     ,@bodyforms))

(defun preview-gs-get-colors ()
  "Return color setup tokens for GhostScript.
Fetches the current screen colors and makes a list of tokens
suitable for passing into GhostScript as arguments.
Pure borderless black-on-white will return NIL."
  (let
      ((bg (color-instance-rgb-components (preview-inherited-face-attribute
             'preview-reference-face :background 'default)))
       (fg (color-instance-rgb-components (preview-inherited-face-attribute
                                           'preview-reference-face :foreground 'default))))
    (if (equal '(65535 65535 65535) bg)
        (setq bg nil))
    (if (equal '(0 0 0) fg)
        (setq fg nil))
    (append
     (if (and bg (not fg))
         '("gsave"))
     (if bg
         (append
          (mapcar #'preview-gs-color-value bg)
          '("setrgbcolor" "clippath" "fill")))
     (if (and bg (not fg))
         '("grestore"))
     (if fg
         (append
          (mapcar #'preview-gs-color-value fg)
          '("setrgbcolor"))))))

(defun preview-mode-setup ()
  "Setup proper buffer hooks and behavior for previews."
  (mapc #'(lambda (hook) (make-local-hook hook))
        '(pre-command-hook post-command-hook
          before-change-functions after-change-functions))
  (add-hook 'pre-command-hook #'preview-mark-point nil t)
  (add-hook 'post-command-hook #'preview-move-point nil t)
  (unless (and (boundp 'balloon-help-mode)
	       balloon-help-mode)
    (balloon-help-minor-mode 1))
  (add-hook 'before-change-functions #'preview-handle-before-change nil t)
  (add-hook 'after-change-functions #'preview-handle-after-change nil t))

(defvar preview-marker (make-marker)
  "Marker for fake intangibility.")

(defvar preview-temporary-opened nil)

(defun preview-mark-point ()
  "Mark position for fake intangibility."
;;  seems to hurt more than it helps.
;;  (when (eq (get-char-property (point) 'preview-state) 'active)
;;    (set-marker preview-marker (point))
;;    (preview-move-point))
  (set-marker preview-marker (point)))

(defcustom preview-auto-reveal 'reveal-mode
  "*Cause previews to open automatically when entered.
Set to t or nil, or to a symbol which will be consulted
if defined.  The default is to follow the setting of
`reveal-mode'.  As long as that is undefined, this
defaults to off."
  :group 'preview-appearance
  :type '(choice (const :tag "Off" nil)
		 (const :tag "On" t)
		 symbol))

(defun preview-move-point ()
  "Move point out of fake-intangible areas."
  (preview-check-changes)
  (let (newlist (pt (point)))
    (setq preview-temporary-opened
	  (dolist (ov preview-temporary-opened newlist)
	    (if (catch 'keep
		  (unless (and (overlay-buffer ov)
			       (eq (overlay-get ov 'preview-state) 'inactive))
		    (throw 'keep nil))
		  (unless (and (eq (overlay-buffer ov) (current-buffer))
                               (not (extent-detached-p ov)))
		    (throw 'keep t))
		  (when (and (>= pt (overlay-start ov))
			     (< pt (overlay-end ov)))
		    (throw 'keep t))
		  (preview-toggle ov t)
		  nil)
		(push ov newlist))))
    (if	(and (boundp preview-auto-reveal)
	     (symbol-value preview-auto-reveal))
	(map-extents #'preview-open-overlay nil
		     pt pt nil nil 'preview-state 'active)
      (let ((backward (and (eq (marker-buffer preview-marker) (current-buffer))
			   (< pt (marker-position preview-marker)))))
	(while (catch 'loop
		 (dolist (ovr (preview-extents-at pt nil 'preview-state))
		   (when (and
			  (eq (overlay-get ovr 'preview-state) 'active))
		     (setq pt (if (and backward
				       (> (overlay-start ovr) (point-min)))
				  (1- (overlay-start ovr))
				(overlay-end ovr)))
		     (throw 'loop t))))
	  nil)
	(goto-char pt)))))

(defun preview-open-overlay (ovr ignored)
  "Open the active preview OVR, IGNORED gets ignored.
NIL is returned: this is for `map-extents'."
  (preview-toggle ovr)
  (push ovr preview-temporary-opened)
  nil)

(defadvice isearch-highlight (before preview protect disable)
  "Make isearch open preview text that's a search hit.
Also make `query-replace' open preview text about to be replaced."
  (map-extents #'preview-open-overlay nil
	       (ad-get-arg 0) (ad-get-arg 1)
	       nil nil 'preview-state 'active))

(defcustom preview-query-replace-reveal t
  "*Make `isearch' and `query-replace' autoreveal previews."
  :group 'preview-appearance
  :type 'boolean
  :require 'preview
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (if value
	     (ad-enable-advice 'isearch-highlight 'before 'preview)
	   (ad-disable-advice 'isearch-highlight 'before 'preview))
	 (ad-activate 'isearch-highlight))
  :initialize #'custom-initialize-reset)

;; Here is the beef: for best intuitiveness, we want to have
;; insertions be carried out as expected before iconized text
;; passages, but we want to insert *into* the overlay when not
;; iconized.  A preview that has become empty can not get content
;; again: we remove it.  A disabled preview needs no insert-in-front
;; handler.

(defvar preview-change-list nil
  "List of tentatively changed overlays.")

(defun preview-register-change (ov map-arg)
  "Register not yet changed OV for verification.
This stores the old contents of the overlay in the
`preview-prechange' property and puts the overlay into
`preview-change-list' where `preview-check-changes' will
find it at some later point of time.  MAP-ARG is ignored;
it is usually generated by `map-extents'."
  (unless (extent-property ov 'preview-prechange)
    (if (eq (extent-property ov 'preview-state) 'disabled)
	(set-extent-property ov 'preview-prechange t)
      (set-extent-property ov
			   'preview-prechange
			   (save-restriction
			     (widen)
			     (buffer-substring-no-properties
			      (extent-start-position ov)
			      (extent-end-position ov)))))
    (push ov preview-change-list))
  nil)

(defun preview-check-changes ()
  "Check whether the contents under the overlay have changed.
Disable it if that is the case.  Ignores text properties."
  (dolist (ov preview-change-list)
    (condition-case nil
	(with-current-buffer (extent-object ov)
	  (let ((text (save-restriction
			(widen)
			(buffer-substring-no-properties
			 (extent-start-position ov)
			 (extent-end-position ov)))))
	    (if (zerop (length text))
		(preview-delete ov)
	      (unless
		  (or (eq (extent-property ov 'preview-state) 'disabled)
		      (string= text (extent-property ov 'preview-prechange)))
		(preview-disable ov)))))
      (error nil))
    (set-extent-property ov 'preview-prechange nil))
  (setq preview-change-list nil))

(defun preview-handle-before-change (beg end)
  "Hook function for `before-change-functions'.
Receives BEG and END, the affected region."
  (map-extents #'preview-register-change nil beg end
	       nil nil 'preview-state))

(defun preview-handle-after-change (beg end length)
  "Hook function for `after-change-functions'.
Receives BEG and END, the affected region, and LENGTH
of an insertion."
  (when (and preview-change-list
	     (zerop length)
	     (not (eq this-command 'undo)))
    (map-extents (lambda (ov maparg)
		   (set-extent-endpoints
		    ov maparg (extent-end-position ov))) nil
		    beg beg end 'start-in-region 'preview-state 'active)
    (map-extents (lambda (ov maparg)
		   (set-extent-endpoints
		    ov (extent-start-position ov) maparg)) nil
		    end end beg 'end-in-region 'preview-state 'active)))

(defun preview-export-image (image)
  "Format an IMAGE into something printable."
  (list (image-instance-file-name (glyph-image-instance image))
	(glyph-image-type image)
	(glyph-baseline-instance image)))

(defun preview-import-image (image)
  "Convert the printable IMAGE rendition back to an image."
  (if (eq (car image) 'image)
      (let ((plist (cdr image)))
	(preview-create-icon
	 (plist-get plist :file)
	 (plist-get plist :type)
	 (plist-get plist :ascent)))
    (preview-create-icon (nth 0 image)
			 (nth 1 image)
			 (nth 2 image))))

;; Now bind the list of compatibility macros into the compiled code.

(defvar preview-compatibility-macros
  (eval-when-compile preview-compatibility-macros)
    "List of macros only present when compiling/loading.")

(provide 'prv-xemacs)

;;; Local variables:
;;; eval: (put 'preview-defmacro 'lisp-indent-function 'defun)
;;; end:

;;; prv-xemacs.el ends here
