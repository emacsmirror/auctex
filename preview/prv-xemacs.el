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

;; TODO: This is not as elegant as I'd like; the `list' should
;; be evalled at macroexpand time.
(preview-defmacro propertize (string &rest properties)
  `(let ((res (copy-sequence ,string)))
     (add-text-properties 0 (length res)
                          (list ,@properties) res)
     res))

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
     (face-height ,face))
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

(preview-defmacro next-single-char-property-change (pos prop &optional object limit)
  "Return the position of next property change for a specific property.

This is like `next-single-property-change', except that if no
change is found before the end of the OBJECT, it returns the
maximum valid position in OBJECT rather than `nil'."
  `(or (next-single-property-change ,pos ,prop ,object ,limit)
       ;; The lack of checking that object isa buffer-or-string is safe;
       ;; `next-single-property-change' throws a noncontinuable error
       ;; if this is not so.
       (and (sequencep ,object)
            (length ,object))
       (and (bufferp ,object)
            (point-max ,object))))

(preview-defmacro previous-single-char-property-change (pos prop &optional object limit)
  "Return the position of previous property change for a specific property.

This is like `next-single-char-property-change', but scans back
from POS instead of forward, and returns the minimum valid
position in OBJECT if no change is found."
  `(or (previous-single-property-change ,pos ,prop ,object ,limit)
       ;; The lack of checking that object isa buffer-or-string is safe;
       ;; `next-single-property-change' throws a noncontinuable error
       ;; if this is not so.
       (and (sequencep ,object)
            0)
       (and (bufferp ,object)
            (point-min ,object))))

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

(defmacro map-plist (function plist)
  "Map FUNCTION over a property list PLIST.
FUNCTION should take one argument, a cons cell."
  `(check-valid-plist ,plist)
  `(mapc ,function (plist-to-alist ,plist)))

(defun destructive-replace-glyph (glyph new-glyph)
  "Destructively replace the contents of GLYPH with those of NEW-GLYPH."
  (map-plist #'(lambda (property)
                 ; XEmacs hands out nil properties that can't be set :(
                 (and (cdr property)
                      (set-glyph-property new-glyph (car property) (cdr property))))
             (object-plist glyph))
  new-glyph)

(defun copy-glyph (glyph)
  "Return a deep copy of GLYPH."
  (destructive-replace-glyph glyph (make-glyph nil (glyph-type glyph))))

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

;; Images.

;; TODO: Generalize this so we can create the fixed icons using it.

(defmacro preview-create-icon (file type ascent)
  "Create an icon from FILE, image TYPE and ASCENT."
  `(let ((glyph
          (make-glyph
           (vector ,type
                   :file ,file))))
     (set-glyph-baseline glyph ,ascent)
     glyph))

(defvar preview-nonready-icon
  (let ((glyph
         (make-glyph
          (list
           `[xpm :file ,(locate-data-file "help.xpm")]
           `[xbm :file ,(locate-data-file "help.xbm")]))))
    (set-glyph-baseline glyph 80)
    glyph)
  "The symbol used for previews to be generated.
Usually a question mark.")

(defvar preview-icon
  (let ((glyph
         (make-glyph
          (list
           `[xpm :file ,(locate-data-file "search.xpm")])))) 
    (set-glyph-baseline glyph 100)
    glyph)
  "The symbol used for an open preview.
Usually a magnifying glass.")

;; Image frobbing.

(defun preview-add-urgentization (fun ov &rest rest)
  "Cause FUN (function call form) to be called when redisplayed.
FUN must be a form with OV as first argument,
REST as the remainder, returning T."
  (set-extent-property ov 'initial-redisplay-function
                       `(lambda (ov) (,fun ,ov ,@rest))))

(defun preview-remove-urgentization (ov)
  "Undo urgentization of OV by `preview-add-urgentization'.
Returns the old arguments to `preview-add-urgentization'
if there was any urgentization."
  (let ((old-urgent (extent-property ov 'initial-redisplay-function)))
    (set-extent-property ov 'initial-redisplay-function nil)
    (and (consp old-urgent)
	 (nth 2 old-urgent))))

(defmacro preview-image-from-icon (icon)
  "Generate a copy of the ICON that is \"editable\".
Which means that `preview-replace-icon' can be called on the
value returned here, and wherever the value was used, the new
image will appear, while ICON itself is not changed."
  `(copy-glyph ,icon))

;; TODO: Shouldn't this be renamed to something like `preview-image-property'?
;; We don't *need* to create a silly one-character string with contents that are
;; never seen...
(defmacro preview-string-from-image (image)
  "Make a string displaying IMAGE.
In fact, it won't display IMAGE without preprocessing; the
image is in the `glyph' property, but you must move it to either
the `begin-glyph' or `end-glyph' property of some extent."
  `(propertize "x" 'glyph ,image))

(defmacro preview-replace-icon (icon replacement)
  "Replace an ICON representation by REPLACEMENT, another icon."
  `(destructive-replace-glyph ,icon (copy-glyph ,replacement)))

(defvar preview-button-1 [mouse-2])
(defvar preview-button-2 [mouse-3])

;; TODO: doesn't seem quite to work yet; the image is highlighted
;;       but not click-responsive.
;; This is so similar to the GNU Emacs function that some refactoring
;; is probably called for.
(defmacro preview-make-clickable (&optional map string helpstring click1 click2)
  "Generate a clickable string or keymap.
If MAP is non-nil, it specifies a keymap to add to, otherwise
a new one is created.  If STRING is given, the result is made
a property of it.  In that case, HELPSTRING is a format string
with one or two %s specifiers for preview's clicks, displayed
via balloon-help.  CLICK1 and CLICK2 are functions to call
on preview's clicks."
  `(let (,@(if string `((res (copy-sequence ,string))))
           (resmap ,(or map '(make-sparse-keymap))))
     ,@(if click1
           `((define-key resmap preview-button-1 ,click1)))
     ,@(if click2
           `((define-key resmap preview-button-2 ,click2)))
     ,@(if string
           `((add-text-properties
              0 (length res)
              (list 'mouse-face 'highlight
              'balloon-help (lambda (extent)
                              (format ,helpstring preview-button-1 preview-button-2))
              'keymap resmap)
              res)
             res)
         '(resmap))))

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

;; TODO: Everything to do with point-motion and calling `preview-disable'.

;; Most of the changes to this are junking the use of overlays;
;; a bit of it is different, and there's a little extra paranoia.

;; We also have to move the image from the begin to the end-glyph
;; whenever the extent is invisible because of a bug in XEmacs-21.4's
;; redisplay engine.
(defun preview-toggle (ov &optional arg)
  "Toggle visibility of preview overlay OV.
ARG can be one of the following: t displays the overlay,
nil displays the underlying text, and 'toggle toggles."
;; TODO: Fix the messing around with glyphs-in-properties versus
;;       glyphs on their own.
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
            (set-extent-properties ov `(invisible t
                                        face nil
                                        begin-glyph nil
                                        begin-glyph-layout text
                                        end-glyph ,(get-text-property 0 'glyph (car strings))))
            (dolist (prop '(keymap mouse-face balloon-help))
              (set-extent-property ov prop
                                   (get-text-property 0 prop (car strings))))
;            (with-current-buffer (overlay-object ov)  ; not yet implemented
;              (add-hook 'pre-command-hook #'preview-mark-point nil t)
;              (add-hook 'post-command-hook #'preview-move-point nil t))
            )
        (dolist (prop '(keymap mouse-face balloon-help invisible))
          (set-extent-property ov prop nil))
        (set-extent-properties ov `(face preview-face
                                    begin-glyph ,(get-text-property 0 'glyph (cdr strings))
                                    begin-glyph-layout text
                                    end-glyph nil)))
      (if old-urgent
          (apply 'preview-add-urgentization old-urgent)))))

(defun preview-gs-color-value (value)
  "Return string to be used as color value for an RGB component.
Conversion from Emacs color numbers (0 to 65535) in VALUE
to GhostScript floats."
  (format "%g" (/ value 65535.0)))

; Does FALLBACKS need to be implemented? Likely not.
(defun preview-inherited-face-attribute (face attribute &optional
                                              fallbacks)
  "Fetch face attribute while adhering to inheritance.
This searches FACE and all its ancestors for an ATTRIBUTE.
FALLBACKS is unused."
  (face-attribute face attribute))

(defmacro preview-with-LaTeX-menus (&rest bodyforms)
  "Activates the LaTeX menus for the BODYFORMS.
This makes it possible to add to them.

Because of a bug in easymenu.el, we can only add items to the
current menubar. So we temporarily add the TeX and LaTeX menus
to the current menubar. This is a quite appalling kludge."
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

(provide 'prv-xemacs)

;;; Local variables:
;;; eval: (put 'preview-defmacro 'lisp-indent-function 'defun)
;;; end:

;;; prv-xemacs.el ends here
