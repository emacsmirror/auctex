;;; preview.el --- embed preview LaTeX images in source buffer

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: David Kastrup <David.Kastrup@t-online.de>
;; Keywords: tex, wp, convenience

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

;; $Id: preview.el,v 1.10 2001-09-26 20:39:37 dakas Exp $
;;
;; This style is for the "seamless" embedding of generated EPS images
;; into LaTeX source code.  The current usage is to put
;; (require 'preview)
;; into your .emacs file and the file somewhere into your load-path.
;; Auc-TeX is required as to now.
;; Please use the usual configure script for installation.
;; Quite a few things with regard to its operation can be configured
;; by using
;; M-x customize-group RET preview RET
;; LaTeX needs to access a special style file "preview.sty".  For the
;; installation of this style file, use the provided configure and
;; install scripts.

;;; History:
;;

;;; Code:

(eval-when-compile
  (defvar error)
  (require 'tex-buf)
  (defvar TeX-auto-file)
  (require 'tq))

(defgroup preview nil "Embed Preview images into LaTeX buffers."
  :group 'AUC-TeX)

(defcustom preview-image-type 'png
  "*Image type to be used in images."
  :group 'preview
  :type '(choice (const postscript)
		 (const png)
		 (symbol :tag "Other")))
  
(defcustom preview-image-creators
  '(postscript (place (preview-eps-place))
    png (open (preview-gs-open png ("-sDEVICE=png256"))
	 place (preview-gs-place)
	 close (preview-gs-close)))
  "*Define functions for generating images.
These functions get called with the type of the image as
first argument, the scale (required real life size per
postscript size) to use as second argument, a list with
members of the form (IMAGE-DESCRIPTOR EPSFILENAME BBOX) and
any remaining arguments configured here."
  :group 'preview
  :type '(plist :value-type
		(plist :key-type symbol
		       :value-type (list function
					 (repeat :inline t sexp))
		       :options (open place close))))

(defun preview-call-hook (symbol &rest rest)
  "Call a function from `preview-image-creators'.
This looks up SYMBOL in the `preview-image-creators' entry
for the image type `preview-image-type' and calls the
hook function given there with the arguments specified there
followed by REST.  If such a function is specified in there,
that is."
  (let ((hook (plist-get (plist-get preview-image-creators
			       preview-image-type) symbol)))
    (when hook
      (apply (car hook) (append (cdr hook) rest)))))
	 	   

;; (defun preview-extract-bb (filename)
;;   "Extract EPS bounding box vector from FILENAME."
;;   (let ((str
;; 	 (with-output-to-string
;; 	   (with-current-buffer
;; 	       standard-output
;; 	     (call-process "grep" filename '(t nil) nil "^%%\\(HiRes\\)\\?BoundingBox:")))))
;;     (if (or (string-match   "^%%HiResBoundingBox:\
;;  +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\)"
;; 			    str)
;; 	    (string-match   "^%%BoundingBox:\
;;  +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\)"
;; 			    str))
;; 	(vector
;; 	 (string-to-number (match-string 1 str))
;; 	 (string-to-number (match-string 2 str))
;; 	 (string-to-number (match-string 3 str))
;; 	 (string-to-number (match-string 4 str))
;; 	 )
;;       )
;;     )
;; )

(defcustom preview-bb-filesize 1024
  "Size of file area scanned for bounding box information."
  :group 'preview :type 'integer)

(defun preview-extract-bb (filename)
  "Extract EPS bounding box vector from FILENAME."
  (with-temp-buffer
    (insert-file-contents-literally filename nil 0 preview-bb-filesize
				    t)
    (goto-char 1)
    (when (search-forward-regexp "%%BoundingBox:\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)" nil t)
      (vector
       (string-to-number (match-string 1))
       (string-to-number (match-string 2))
       (string-to-number (match-string 3))
       (string-to-number (match-string 4))
       ))))

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

(defcustom preview-gs-command "gs"
  "*How to call gs for conversion from EPS.  See also `preview-gs-options'."
  :group 'preview
  :type 'string)

(defcustom preview-gs-options '("-q" "-DSAFER" "-dNOPAUSE"
				"-DNOPLATFONTS" "-dTextAlphaBits=4"
				"-dGraphicsAlphaBits=4")
  "*Options with which to call gs for conversion from EPS.
See also `preview-gs-command'."
  :group 'preview
  :type '(repeat string))

(defvar preview-gs-urgent nil
  "List of high priority overlays to convert using gs.
Buffer-local to the appropriate TeX process buffer.")
(make-variable-buffer-local 'preview-gs-urgent)

(defvar preview-gs-queue nil
  "List of overlays to convert using gs.
Buffer-local to the appropriate TeX process buffer.")
(make-variable-buffer-local 'preview-gs-queue)

(defvar preview-gs-outstanding nil
  "Overlays currently processed.")
(make-variable-buffer-local 'preview-gs-outstanding)

(defcustom preview-gs-outstanding-limit 2
  "*Number of requests allowed to be outstanding.
This is the number of not-yet-completed requests we
might at any time have piped into GhostScript.  If
this number is larger, the probability of GhostScript
working continuously is higher when Emacs is rather
busy.  If this number is smaller, redisplay will
follow changes in the displayed buffer area faster."
  :group 'preview
  :type '(restricted-sexp
	  :match-alternatives
	  ((lambda (value) (and
			    (integerp value)
			    (> value 0)
			    (< value 10))))
	  :tag "small number"))

(defvar preview-gs-tq nil
  "Transaction queue for gs processing.")
(make-variable-buffer-local 'preview-gs-tq)

(defvar preview-gs-image-type nil
  "Image type for gs produced images.")
(make-variable-buffer-local 'preview-gs-image-type)

(defvar preview-scale nil
  "Screen scale of images.
Magnify by this factor to make images blend with other
screen content.  Buffer-local to rendering buffer.")
(make-variable-buffer-local 'preview-scale)

(defvar preview-resolution nil
  "Screen resolution where rendering started.
Cons-cell of x and y resolution, given in
dots per inch.  Buffer-local to rendering buffer.")
(make-variable-buffer-local 'preview-resolution)

(defun preview-gs-resolution (scale xres yres)
  "Generate resolution argument for gs.
Calculated from real-life factor SCALE and XRES and
YRES, the screen resolution in dpi."
  (format "-r%gx%g"
	  (* scale xres)
	  (* scale yres)))

(defun preview-gs-behead-outstanding ()
  "Remove leading element of outstanding queue after error.
Return element if non-nil."
  (let ((ov (pop preview-gs-outstanding)))
    (when ov
      (condition-case nil
	  (preview-delete-file
	   (aref (overlay-get ov 'queued) 0)
	   t)
	(error nil))
      (preview-deleter ov))
    ov))
    
(defun preview-gs-sentinel (process string)
  "Sentinel function for rendering process.
Gets the default PROCESS and STRING arguments
and tries to restart GhostScript if necessary."
  (if process (TeX-format-mode-line process))
  (let ((status (process-status process)))
    (when (or (eq status 'exit) (eq status 'signal))
      ;; process died.
      ;;  Throw away culprit, go on.
      (with-current-buffer (process-buffer process)
	;; Shut down queue
	(tq-close preview-gs-tq)
	(if (or (null (preview-gs-behead-outstanding))
		(eq status 'signal))
	  ;; if process was killed explicitly by signal, or if nothing
	  ;; was processed, we give up on the matter altogether.
	    (progn
	      (mapc (function preview-deleter) preview-gs-outstanding)
	      (mapc (function preview-deleter) preview-gs-queue)
	      (setq preview-gs-outstanding nil)
	      (setq preview-gs-urgent nil)
	      (setq preview-gs-queue nil))
	    
	    ;; restart only if we made progress since last call
	  (setq preview-gs-urgent (nconc preview-gs-outstanding
					 preview-gs-urgent))
	  (setq preview-gs-outstanding nil)
	  (preview-gs-restart))))))

(defvar preview-gs-command-line nil)
(make-variable-buffer-local 'preview-gs-command-line)

(defun preview-gs-restart ()
  "Start a new GhostScript conversion process."
  (when (or preview-gs-urgent preview-gs-queue)
    (let* ((buff (current-buffer))
	   (process-connection-type nil)
	   (process
	    (apply (function start-process) "GS" buff
		   preview-gs-command
		   preview-gs-command-line)))
      (set-process-sentinel process (function preview-gs-sentinel))
      (setq preview-gs-tq (tq-create process))
      (tq-enqueue preview-gs-tq
		  "" "GS>" buff (function preview-gs-transact)))))

(defalias 'preview-gs-close (function preview-gs-restart))

(defun preview-gs-open (imagetype gs-optionlist)
  "Start a GhostScript conversion pass.
IMAGETYPE specifies the Emacs image type for the generated
files, GS-OPTIONLIST is a list of options to pass into
GhostScript for getting that sort of image type, for
example \"-sDEVICE=png256\" will go well with 'png."
  (setq preview-gs-image-type imagetype)
  (setq preview-gs-command-line (append
				 preview-gs-options
				 gs-optionlist
				 (list (preview-gs-resolution
					preview-scale
					(car preview-resolution)
					(cdr preview-resolution)))))
  (setq preview-gs-queue nil)
  (setq preview-gs-urgent nil))

(defun preview-gs-urgentize (ov buff)
  "Make a displayed overlay render with higher priority.
This function is used in fake conditional display properties
for reordering the conversion order to prioritize on-screen
images.  OV is the overlay in question, and BUFF is the
GhostScript process buffer where the buffer-local queue
is located."
  (when (and (overlay-get ov 'queued)
	     (overlay-buffer ov))
    (with-current-buffer buff
      (push ov preview-gs-urgent)))
  (overlay-put ov 'display nil)
  nil)

(defimage preview-nonready-icon ((:type xpm :file "help.xpm" :ascent 80)
				 (:type pbm :file "help.pbm" :ascent 80)))

(defun preview-gs-place (ov tempdir snippet)
  "Generate an image placeholder rendered over by GhostScript.
This enters OV into all proper queues in order to make it
render this image for real later, and returns a substitute
image to be placed as a first measure.  TEMPDIR and SNIPPET
are used for making the name of the file to be generated."
  (let ((thisimage (cons 'image (cdr preview-nonready-icon))))
    (overlay-put ov 'queued
		 (vector
		  (preview-make-filename
		   (format "prevnew.%03d" snippet) tempdir t)
		  (preview-extract-bb (car (overlay-get ov 'filename)))
		  thisimage))
    (push ov preview-gs-queue)
    (overlay-put ov 'display `(when (preview-gs-urgentize ,ov ,(current-buffer)) . nil))
    thisimage))
		
(defun preview-gs-transact (buff answer)
  "Work off GhostScript transaction queue in buffer BUFF.
This routine is the action routine called via `tq-enqueue'.
It receives the GhostScript process buffer as CLOSURE argument,
and the standard output of GhostScript as ANSWER.  It will then
proceed to feed GhostScript with further rendering commands."
  (with-current-buffer buff
    (save-excursion
      (goto-char (point-max))
      (unless (string= answer "GS>")
	(insert answer))
      (condition-case whatgives
	  (let ((ov (pop preview-gs-outstanding)))
	    (when ov
	      (condition-case nil
		  (preview-delete-file (overlay-get ov 'filename) t)
		(file-error nil))
	      (let* ((queued (overlay-get ov 'queued))
		     (newfile (aref queued 0))
		     (bbox (aref queued 1))
		     (img (aref queued 2)))
		(overlay-put ov 'queued nil)
		(overlay-put ov 'filename newfile)
		(setcdr img (list :file (car newfile)
				  :type preview-gs-image-type
				  :heuristic-mask t
				  :ascent (preview-ascent-from-bb bbox)))))
	    (while
		(and
		 (< (length preview-gs-outstanding)
		    preview-gs-outstanding-limit)
		 (setq ov
		       (or (pop preview-gs-urgent)
			   (pop preview-gs-queue))))
	      (let ((queued (overlay-get ov 'queued)))
		(when (and queued
			   (not (memq ov preview-gs-outstanding))
			   (overlay-buffer ov))
		  (let ((newfile (aref queued 0))
			(bbox (aref queued 1))
			(oldfile (overlay-get ov 'filename)))
		    (setq preview-gs-outstanding
			  (nconc preview-gs-outstanding
				 (list ov)))
		    (tq-enqueue
		     preview-gs-tq
		     (format "clear \
/preview-LaTeX-state save def \
<< \
/PageSize [%g %g] \
/PageOffset [%g %g] \
/OutputFile (%s) >> setpagedevice \
(%s) run \
preview-LaTeX-state restore\n"
			     (- (aref bbox 2) (aref bbox 0))
			     (- (aref bbox 3) (aref bbox 1))
			     (- (aref bbox 0)) (aref bbox 1)
			     (car newfile)
			     (car oldfile))
		     "GS\\(<[0-9]+\\)?>"
		     buff
		     (function preview-gs-transact)) ))))
	    (unless (or ov preview-gs-outstanding)
	      (process-send-eof (get-buffer-process buff))))
	(error (insert (error-message-string whatgives)))))))

(defcustom preview-scale-function (function preview-scale-from-face)
  "*Scale factor for included previews.
This can be either a function to calculate the scale, or
a fixed number."
  :group 'preview
  :type '(choice (function-item preview-scale-from-face)
		 (const 1.0)
		 (number :value 1.0)
		 (function :value preview-scale-from-face)))

(defcustom preview-default-document-pt 10
  "*Assumed document point size for `preview-scale-from-face'.
If the point size (such as 11pt) of the document cannot be
determined from the document options itself, assume this size.
This is for matching screen font size and previews."
  :group 'preview
  :type
          '(choice (const :tag "10pt" 10)
                  (const :tag "11pt" 11)
                  (const :tag "12pt" 12)
                  (number :tag "Other" :value 11.0))
)

(defun preview-document-pt ()
  "Calculate the default font size of document.
If packages, classes or styles were called with an option
like 10pt, size is taken from the first such option if you
had let your document be parsed by AucTeX.  Otherwise
the value is taken from `preview-default-document-pt'."
  (or (and (boundp 'TeX-auto-file)
	   (catch 'return (dolist (option TeX-auto-file nil)
			    (if (string-match "\\`\\([0-9]+\\)pt\\'" option)
				(throw 'return
				       (string-to-number
					(match-string 1 option)))))) )
      preview-default-document-pt))

(defun preview-scale-from-face ()
  "Calculate preview scale from default face.
This calculates the scale of EPS images from a document assumed
to have a default font size given by function `preview-document-pt'
so that they match the current default face in height."
  (/ (face-attribute 'default :height) 10.0 (preview-document-pt)))


(defun preview-ascent-from-bb (bb)
  "This calculates the image ascent from its bounding box.
The bounding box BB needs to be a 4-component vector of
numbers (can be float if available)."
  ;; baseline is at 1in from the top of letter paper (11in), so it is
  ;; at 10in from the bottom precisely, which is 720 in PostScript
  ;; coordinates.  If our bounding box has its bottom not above this
  ;; line, and its top above, we can calculate a useful ascent value.
  ;; If not, something is amiss.  We just use 100 in that case.

  (let ((bottom (aref bb 1))
	(top (aref bb 3)))
    (if (and (<= bottom 720)
	     (> top 720))
	(round (* 100.0 (/ (- top 720.0) (- top bottom))))
      100)))

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
     '(preview-disable) )

(put 'preview-overlay
     'insert-in-front-hooks
     '(preview-disable) )

(defcustom preview-face 'highlight
  "Face to use for preview."
  :group 'preview
  :type 'face)

(put 'preview-overlay 'face preview-face)

(put 'preview-overlay 'invisible t)

(defun preview-toggle (ov &rest ignored)
  "Toggle visibility of preview overlay OV.
The argument IGNORED is ignored so that you can use this
function for modification hooks."
  (let ((invisible (null (overlay-get ov 'invisible)))
	(strings (overlay-get ov 'strings)))
    (overlay-put ov 'invisible invisible)
    (overlay-put ov 'before-string (if invisible
				       (car strings)
				     (cdr strings)))
    ))

(put 'preview-overlay 'isearch-open-invisible 'preview-toggle)
(put 'preview-overlay 'isearch-open-invisible-temporary 'preview-toggle)

(defun preview-make-map (ovr)
  "Make an initial keymap for preview overlay OVR."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] `(lambda () (interactive) (preview-toggle ,ovr)))
    (define-key map [mouse-3] `(lambda () (interactive) (preview-deleter ,ovr)))
    map))

(defun preview-regenerate (ovr)
  "Pass the modified region in OVR again through LaTeX."
  (let ((begin (overlay-start ovr))
	(end (overlay-end ovr)))
    (preview-deleter ovr)
    (TeX-region-create (TeX-region-file TeX-default-extension)
		       (buffer-substring begin end)
		       (file-name-nondirectory (buffer-file-name))
		       (count-lines (save-restriction (widen) (point-min))
				    begin)))
  (TeX-command "Graphic Preview" 'TeX-region-file))

(defimage preview-icon ((:type xpm :file "search.xpm" :ascent 100)
			(:type pbm :file "search.pbm" :ascent 100)))

(defun preview-disabled-string (ov)
  "Generate a before-string for disabled preview overlay OV."
  (propertize "x" 'display preview-icon
	          'local-map (overlay-get ov 'preview-map)
		  'help-echo "mouse-2 regenerates preview
mouse-3 kills preview"))

(defun preview-disable (ovr &rest ignored)
  "Change overlay behaviour of OVR after source edits.
IGNORED gets ignored so that `preview-disable' might be
used in change hooks."
  (define-key (overlay-get ovr 'preview-map) [mouse-2]
    `(lambda () (interactive) (preview-regenerate ,ovr)))
  (let ((str (preview-disabled-string ovr)))
    (overlay-put ovr 'before-string str)
    (overlay-put ovr 'invisible nil)
    (overlay-put ovr 'strings (cons str str))
    (overlay-put ovr 'insert-in-front-hooks nil)
    (overlay-put ovr 'modification-hooks nil))
  (let ((filename (overlay-get ovr 'filename)))
    (when filename
      (overlay-put ovr 'filename nil)
      (condition-case nil
	  (preview-delete-file filename)
	(file-error nil)))))
    
(defun preview-deleter (ovr &rest ignored)
  "Delete preview overlay OVR, taking any associated file along.
IGNORED arguments are ignored, making this function usable as
a hook in some cases"
  (let ((filename (overlay-get ovr 'filename)))
    (delete-overlay ovr)
    (when filename
      (overlay-put ovr 'filename nil)
      (condition-case nil
	  (preview-delete-file filename)
	(file-error nil)))))

(defun preview-clearout (&optional start end)
  "Clear out all previews in the current region.
When called interactively, the current region is used.
Non-interactively, the region between START and END is
affected.  Those two values default to the borders of
the entire buffer."
  (interactive "r")
  (dolist (ov (overlays-in (or start 1)
			   (or end (1+ (buffer-size)))))
    (if (eq (overlay-get ov 'category) 'preview-overlay)
	(preview-deleter ov))))

(add-hook 'kill-buffer-hook (function preview-clearout) nil nil)

(defvar preview-temp-dirs nil
"List of top level temporary directories in use from preview.
Any directory not in this list will be cleared out by preview
on first use."
)

(defun preview-inactive-string (ov)
  "Generate before-string for an inactive preview overlay OV.
This is for overlays where the source text has been clicked
visible."
  (concat
   (propertize "x" 'display preview-icon
	       'local-map (overlay-get ov 'preview-map)
	       'help-echo "mouse-2 toggles preview
mouse-3 kills preview")
   "\n") )

(defun preview-eps-place (ov tempdir snippet)
  "Generate an image via direct Emacs EPS rendering.
Since OV already carries all necessary information,
 the arguments TEMPDIR and SNIPPET passed via a hook
mechanism are ignored."
  (preview-ps-image (car (overlay-get ov 'filename)) preview-scale))

(defun preview-active-string (ov tempdir snippet)
  "Generate before-string for active image overlay OV.
This calls the `place' hook indicated by `preview-image-type'
in `preview-image-creators' with OV, TEMPDIR and SNIPPET
and expects an image property as result."
  (propertize "x" 'display
	      (preview-call-hook 'place ov tempdir snippet)
	      'local-map (overlay-get ov 'preview-map)
	      'help-echo "mouse-2 opens text
mouse-3 kills preview") )

(defun preview-make-filename (file tempdir &optional dont-register)
  "Generate a preview filename from FILE and TEMPDIR.
Those consist of a CONS-cell with absolute file name as CAR
and TEMPDIR as CDR.  TEMPDIR is a CONS-cell with the directory
name as CAR and the reference count as CDR.  The reference count
is not incremented if DONT-REGISTER is non-NIL."

  (unless dont-register
    (setcdr tempdir (1+ (cdr tempdir))))
  (cons (expand-file-name file (car tempdir)) tempdir))

(defun preview-delete-file (file &optional dont-register)
  "Delete a preview FILE.
If DONT-REGISTER is non-nil, the reference count of its
directory is not adjusted.  See `preview-make-filename'
for a description of the data structure.  If the directory
becomes empty, it gets deleted."
  (delete-file (car file))
  (when (and (null dont-register)
	     (zerop (setcdr (cdr file) (1- (cdr (cdr file))))))
    (delete-directory (car (cdr file)))))

(defun preview-place-preview (tempdir snippet source start end)
  "Generate and place an overlay preview image.
This generates the EPS filename used in TEMPDIR (see
`preview-make-filename' for its definition) for preview
snippet SNIPPET in buffer SOURCE, and uses it for the
region between START and END."
  (let ((ov (with-current-buffer source
	      (preview-clearout start end)
	      (make-overlay start end nil nil nil))))
    (overlay-put ov 'category 'preview-overlay)
    (overlay-put ov 'preview-map (preview-make-map ov))
    (overlay-put ov 'filename (preview-make-filename
			       (format "preview.%03d" snippet) tempdir))
    (let ((active (preview-active-string ov tempdir snippet)))
      (overlay-put ov 'strings
		   (cons active
			 (preview-inactive-string ov)))
      (overlay-put ov 'before-string active))))


(defun preview-back-command (&optional posn buffer)
  "Move backward a TeX token from POSN in BUFFER.
Actually, this does not move but only returns the position.
Defaults to point in current buffer."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if posn (goto-char posn))
    (condition-case nil
	(progn
	  (if (eq (char-before) ?\$) (backward-char)
	    (while
		(let ((oldpoint (point)))
		  (backward-sexp)
		  (and (not (eq oldpoint (point)))
		       (eq ?\( (char-syntax (char-after)))))))
	  (point))
      (error nil))))

(defcustom preview-default-option-list '("displaymath" "floats" "graphics")
  "*Specifies default options to pass to preview package.
These options are only used when the LaTeX document in question does
not itself load the preview package, namely when you use preview
on a document not configured for preview.  \"auctex\", \"active\"
and \"delayed\" need not be specified here."
  :group 'preview
  :type '(list (set :inline t :tag "Options known to work"
		    :format "%t:\n%v%h" :doc
"The above options are all the available ones
at the time of the release of this package.
You should not need \"Other options\" unless you
upgraded to a fancier version of just the LaTeX style."
		    (const "displaymath")
		    (const "floats")
		    (const "graphics")
		    (const "textmath"))
	       (repeat :inline t :tag "Other options" (string))))

(make-variable-buffer-local 'preview-default-option-list)

(defun preview-make-options ()
  "Create default option list to pass into LaTeX preview package."
  (mapconcat 'identity preview-default-option-list ","))
		
(defun LaTeX-preview-setup ()
  "Hook function for embedding the preview package into Auc-TeX.
This is called by `LaTeX-mode-hook' and changes Auc-TeX variables
to add the preview functionality."
  (remove-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
  (require 'tex-buf)
  (setq TeX-command-list
       (append TeX-command-list
	       '(("Graphic Preview"
		  "%l '\\nonstopmode\\PassOptionsToPackage{auctex,active}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[%P]{preview}\\fi}\\input{%t}';dvips -Pwww -i -E %d -o %m/preview.000"
		  TeX-inline-preview nil))))
  (setq TeX-error-description-list
       (cons '("Package Preview Error.*" .
"The auctex option to preview should not be applied manually.  If you
see this error message, either you did something too clever, or the
preview Emacs Lisp package something too stupid.") TeX-error-description-list))
  (add-hook 'TeX-translate-location-hook 'preview-translate-location)
  (setq TeX-expand-list
	(append TeX-expand-list
		'(("%m" preview-create-subdirectory)
		  ("%P" preview-make-options)) )))

(defun preview-clean-subdir (dir)
  "Cleans out a temporary DIR with preview image files."
  (condition-case err
      (progn
	(mapc 'delete-file
	      (directory-files dir t "\\`pre" t))
	(delete-directory dir))
    (error (message "Deletion of %s failed: %s" dir
		    (error-message-string err)))))


(defvar TeX-active-tempdir)
(make-variable-buffer-local 'TeX-active-tempdir)

(defun preview-create-subdirectory ()
  "Create a temporary subdir for the current TeX process.
If necessary, generates a fitting top
directory or cleans out an existing one (if not yet
visited in this session), then returns the name of
the created subdirectory.  `TeX-active-tempdir' is
set to the corresponding TEMPDIR descriptor as described
in `preview-make-filename'.  The directory is registered
in `preview-temp-dirs' in order not to be cleaned out
later while in use."
  (let ((topdir (expand-file-name (TeX-active-master "prv"))))
    (unless (member topdir preview-temp-dirs)
      (if (file-directory-p topdir)
	  ;;  Cleans out the top preview directory by
	  ;;  removing subdirs possibly left from a previous session.
	  (mapc 'preview-clean-subdir
		(directory-files topdir t "\\`tmp" t))
	(make-directory topdir))
      (setq preview-temp-dirs (cons topdir preview-temp-dirs)) )
    (setq TeX-active-tempdir
	  (cons (make-temp-file (expand-file-name
			   "tmp" (file-name-as-directory topdir)) t) 0))
    (car TeX-active-tempdir)))

(defun preview-translate-location ()
  "Skip Package Preview Errors via `throw' of 'preview-error-tag.
Used as function in `TeX-translate-location-hook'."
  (if (string-match "Package Preview Error.*" error)
      (condition-case nil
	  (throw 'preview-error-tag t)
	(no-catch nil))))

(defun preview-parse-TeX (reparse)
  "Implementation of error parsing in preview package.
See `TeX-parse-TeX' for documentation of REPARSE."
  (while
      (catch 'preview-error-tag
	(TeX-parse-TeX reparse)
	nil)))

;; Hook into TeX immediately if it's loaded, use LaTeX-mode-hook if not.
(if (featurep 'tex)
    (LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))
      
(defvar preview-snippet nil
  "Number of current preview snippet.")
(make-local-variable 'preview-snippet)
(defvar preview-snippet-start nil
  "Point of start of current preview snippet.")
(make-local-variable 'preview-snippet-start)

(defun preview-parse-messages ()
  "Turn all preview snippets into overlays.
This parses the pseudo error messages from the preview
document style for LaTeX."
  (let ((tempdir TeX-active-tempdir))
    (with-current-buffer (TeX-active-buffer)
      (TeX-parse-reset)
      (preview-call-hook 'open)
      (unwind-protect
	  (progn
	    (setq preview-snippet 0)
	    (setq preview-snippet-start nil)
	    (goto-char TeX-error-point)
	    (while
		(re-search-forward
		 (concat "\\("
			 "^! Package Preview Error:[^.]*\\.\\|"
			 "(\\|"
			 ")\\|"
			 "!offset([---0-9]*)\\|"
			 "!name([^)]*)"
			 "\\)") nil t)
	      (let ((string (TeX-match-buffer 1)))
		(cond
		 (;; Preview error
		  (string-match "^! Package Preview Error: Snippet \\([---0-9]+\\) \\(started\\|ended\\)\\." string)
		  (preview-analyze-error
		   (string-to-int (match-string 1 string))
		   (string= (match-string 2 string) "started")
		   tempdir))
		 ;; New file -- Push on stack
		 ((string= string "(")
		  (re-search-forward "\\([^()\n \t]*\\)")
		  (setq TeX-error-file
			(cons (TeX-match-buffer 1) TeX-error-file))
		  (setq TeX-error-offset (cons 0 TeX-error-offset)))
		 
		 ;; End of file -- Pop from stack
		 ((string= string ")")
		  (setq TeX-error-file (cdr TeX-error-file))
		  (setq TeX-error-offset (cdr TeX-error-offset)))
		 
		 ;; Hook to change line numbers
		 ((string-match "!offset(\\([---0-9]*\\))" string)
		  (rplaca TeX-error-offset
			  (string-to-int (match-string 1 string))))
		 
		 ;; Hook to change file name
		 ((string-match "!name(\\([^)]*\\))" string)
		  (rplaca TeX-error-file (match-string 1 string)))
		 (t (error "Should have matched %s" string)))))
	    (if (zerop preview-snippet)
		(preview-clean-subdir (car tempdir))) )
	(preview-call-hook 'close)))))

(defun preview-analyze-error (snippet startflag tempdir)
  "Analyze a preview diagnostic for snippet SNIPPET.
The diagnostic is for the start of the snippet if STARTFLAG
is set, and an overlay will be generated for the corresponding
file dvips put into the directory indicated by TEMPDIR."
  
  (let* (;; We need the error message to show the user.
	 (error (progn
		  (re-search-forward "\\(.*\\)")
		  (TeX-match-buffer 1)))

	 ;; And the context for the help window.
	 (context-start (point))

	 ;; And the line number to position the cursor.
	 (line (if (re-search-forward "l\\.\\([0-9]+\\)" nil t)
		   (string-to-int (TeX-match-buffer 1))
		 1))
	 ;; And a string of the context to search for.
	 (string (progn
		   (beginning-of-line)
		   (re-search-forward " \\(\\.\\.\\.\\)?\\(.*$\\)")
		   (TeX-match-buffer 2)))

	 ;; And we have now found to the end of the context.
	 (context (buffer-substring context-start (progn
						    (forward-line 1)
						    (end-of-line)
						    (point))))
	 ;; We may use these in another buffer.
	 (offset (car TeX-error-offset) )
	 (file (car TeX-error-file)))
	 
    ;; Remember where we was.
    (setq TeX-error-point (point))

    ;; Find the error.
    (if (null file)
	(error "Error occured after last TeX file closed"))
    (run-hooks 'TeX-translate-location-hook)
    (if startflag
	(unless (eq snippet (1+ preview-snippet))
	  (message "Preview snippet %d out of sequence" snippet))
      (unless (eq snippet preview-snippet)
	(message "End of Preview snippet %d unexpected" snippet)
	(setq preview-snippet-start nil)))
    (setq preview-snippet snippet)
    (let* ((buffer (find-file-noselect file))
	   (next-point
	    (with-current-buffer buffer
	      (save-excursion
		(goto-line (+ offset line))
		(search-forward string (line-end-position) t)
		(or (and startflag (preview-back-command))
		    (point))))))
      (if startflag
	  (setq preview-snippet-start next-point)
	(if preview-snippet-start
	    (progn
	      (preview-place-preview tempdir
				     snippet
				     buffer
				     preview-snippet-start
				     next-point)
	      (setq preview-snippet-start nil))
	  (message "Unexpected end of Preview snippet %d" snippet))))))

(defun preview-get-geometry (process)
  "Transfer display geometry parameters of current display.
Those are put in local variables `preview-scale' and
`preview-resolution' of GhostScript rendering buffer from PROCESS."
  (with-current-buffer (process-buffer process)
    (setq preview-scale (funcall preview-scale-function))
    (setq preview-resolution (cons
			      (/ (* 25.4 (display-pixel-width))
				 (display-mm-width))
			      (/ (* 25.4 (display-pixel-height))
				 (display-mm-height))))))

(defun preview-TeX-inline-sentinel (process name)
  "Sentinel function for preview.
See `TeX-sentienel-function' and `set-process-sentinel'
for definition of PROCESS and NAME."
  (if process (TeX-format-mode-line process))
  (if (eq (process-status process) 'exit)
      (with-current-buffer TeX-command-buffer
	(preview-parse-messages))))
  
(defun TeX-inline-preview (name command file)
  "Main function called by AucTeX.
NAME, COMMAND and FILE are described in `TeX-command-list'."
  (let ((process (TeX-run-format name command file)))
    (preview-get-geometry process)
    (setq TeX-sentinel-function 'preview-TeX-inline-sentinel)
    (setq TeX-parse-function 'preview-parse-TeX)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(provide 'preview)
;;; preview.el ends here
