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

;; $Id: preview.el,v 1.30 2001-10-13 21:46:26 dakas Exp $
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
;;
;; Please report bugs with M-x preview-report-bug RET
;;
;;; History:
;;

;;; Code:

(eval-when-compile
  (require 'tex-buf)
  (defvar error))

(require (if (string-match "XEmacs" (emacs-version))
	     'prv-xemacs 'prv-emacs))

(defgroup preview nil "Embed Preview images into LaTeX buffers."
  :group 'AUC-TeX)

(defcustom preview-image-creators
  '((postscript (place preview-eps-place))
    (png (open preview-gs-open png ("-sDEVICE=png256"))
	 (place preview-gs-place)
	 (close preview-gs-close))
    (jpeg (open preview-gs-open jpeg ("-sDEVICE=jpeg"))
	  (place preview-gs-place)
	  (close preview-gs-close))
    (pnm (open preview-gs-open pbm ("-sDEVICE=pnmraw"))
	  (place preview-gs-place)
	  (close preview-gs-close))
    (tiff (open preview-gs-open tiff ("-sDEVICE=tiffpack"))
	  (place preview-gs-place)
	  (close preview-gs-close)))
  "Define functions for generating images.
These functions get called in the process of generating inline
images of the specified type.  The open function is called
at the start of a rendering pass, the place function for
placing every image, the close function at the end of
the pass.  Look at the documentation of the various
functions used here for the default settings, and at
the function `preview-call-hook' through which those are
called.  Additional argument lists specified in here
are passed to the functions before any additional
arguments given to `preview-call-hook'."
  :group 'preview
  :type '(alist :key-type (symbol :tag "Preview's image type")
		:value-type
		(alist :tag "Handler" :key-type (symbol :tag "Operation:")
		       :value-type (list :tag "Handler"
					 (function :tag "Handler function")
					 (repeat :tag "Additional \
function args" :inline t sexp))
		       :options (open place close))))

(defcustom preview-image-type 'png
  "*Image type to be used in images."
  :group 'preview
  :type (append '(choice)
	      (mapcar (lambda (symbol) (list 'const (car symbol)))
		      preview-image-creators)
	      '((symbol :tag "Other"))))

(defun preview-call-hook (symbol &rest rest)
  "Call a function from `preview-image-creators'.
This looks up SYMBOL in the `preview-image-creators' entry
for the image type `preview-image-type' and calls the
hook function given there with the arguments specified there
followed by REST.  If such a function is specified in there,
that is."
  (let ((hook (cdr (assq symbol
		    (cdr (assq preview-image-type
			       preview-image-creators))))))
    (when hook
      (apply (car hook) (append (cdr hook) rest)))))
	 	   

(defvar TeX-active-tempdir nil
  "CONS of a directory name and a reference count.")
(make-variable-buffer-local 'TeX-active-tempdir)

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
    (goto-char (point-min))
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

(defvar preview-gs-answer nil
  "Accumulated answer of GhostScript process.")
(make-variable-buffer-local 'preview-gs-answer)

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

(defun preview-gs-behead-outstanding (err)
  "Remove leading element of outstanding queue after error.
Return element if non-nil.  ERR is the error string to
show as response of GhostScript."
  (let ((ov (pop preview-gs-outstanding)))
    (when ov
      (preview-gs-flag-error ov err)
      (overlay-put ov 'queued nil))
    ov))
    
(defvar preview-gs-command-line nil)
(make-variable-buffer-local 'preview-gs-command-line)

(defun preview-gs-sentinel (process string)
  "Sentinel function for rendering process.
Gets the default PROCESS and STRING arguments
and tries to restart GhostScript if necessary."
  (with-current-buffer (process-buffer process)
    (TeX-command-mode-line process)
    (let ((status (process-status process)))
      (when (memq status '(exit signal))
	;; process died.
	;;  Throw away culprit, go on.
	(let* ((err (concat preview-gs-answer "\n"
			      (process-name process) " " string))
	       (ov (preview-gs-behead-outstanding err)))
	  (when (and (null ov) preview-gs-queue)
	    (save-excursion
	      (goto-char (process-mark process))
	      (insert-before-markers preview-gs-command " "
				     (mapconcat #'shell-quote-argument
						preview-gs-command-line
						" ") "\n" err)))
	  (delete-process process)
	  (if (or (null ov)
		  (eq status 'signal))
	      ;; if process was killed explicitly by signal, or if nothing
	      ;; was processed, we give up on the matter altogether.
	      (progn
		(mapc #'preview-delete preview-gs-outstanding)
		(dolist (ov preview-gs-queue)
		  (if (overlay-get ov 'queued)
		      (preview-delete ov)))
		(setq preview-gs-outstanding nil)
		(setq preview-gs-queue nil))
	  
	    ;; restart only if we made progress since last call
	    (setq preview-gs-queue (nconc preview-gs-outstanding
					  preview-gs-queue))
	    (setq preview-gs-outstanding nil)
	    (preview-gs-restart)))))))

(defun preview-gs-filter (process string)
  "Filter function for processing GhostScript output.
Gets the usual PROCESS and STRING parameters, see
`set-process-filter' for a description."
  (with-current-buffer (process-buffer process)
    (setq preview-gs-answer (concat preview-gs-answer string))
    (while (string-match "GS\\(<[0-9+]\\)?>" preview-gs-answer)
      (let* ((pos (match-end 0))
	     (answer (substring preview-gs-answer 0 pos)))
	(setq preview-gs-answer (substring preview-gs-answer pos))
	(preview-gs-transact process answer)))))

(defun preview-gs-restart ()
  "Start a new GhostScript conversion process."
  (when preview-gs-queue
    (let* ((process-connection-type nil)
	   (process
	    (apply #'start-process
		   "Preview-GhostScript"
		   (current-buffer)
		   preview-gs-command
		   preview-gs-command-line)))
      (setq preview-gs-answer "")
      (set-process-sentinel process #'preview-gs-sentinel)
      (set-process-filter process #'preview-gs-filter)
      (TeX-command-mode-line process))))

(defalias 'preview-gs-close #'preview-gs-restart)

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
  (setq preview-gs-queue nil))

(defun preview-gs-urgentize (ov buff)
  "Make a displayed overlay render with higher priority.
This function is used in fake conditional display properties
for reordering the conversion order to prioritize on-screen
images.  OV is the overlay in question, and BUFF is the
GhostScript process buffer where the buffer-local queue
is located."
  ;; It does not matter that ov gets queued twice in that process: the
  ;; first version to get rendered will clear the 'queued property.
  ;; It cannot get queued more than twice since we remove the
  ;; conditional display property responsible for requeuing here.
  ;; We don't requeue if the overlay has been killed (its buffer made
  ;; nil).  Not necessary, but while we are checking...
  ;; We must return t.
  (preview-remove-urgentization ov)
  (when (and (overlay-get ov 'queued)
	     (overlay-buffer ov))
    (with-current-buffer buff
      (push ov preview-gs-queue)))
  t)


(defun preview-gs-place (ov snippet)
  "Generate an image placeholder rendered over by GhostScript.
This enters OV into all proper queues in order to make it render
this image for real later, and returns a substitute image
to be placed as a first measure.  `TeX-active-tempdir' and
SNIPPET are used for making the name of the file to be generated."
  (let ((thisimage (preview-image-from-icon preview-nonready-icon))
	(filenames (overlay-get ov 'filenames)))
    (setcdr filenames
	    (list (preview-make-filename
		   (format "prevnew.%03d" snippet))))
    (overlay-put ov 'queued
		 (vector
		  (preview-extract-bb (car (car filenames)))
		  thisimage
		  nil))
    (push ov preview-gs-queue)
    (preview-add-urgentization #'preview-gs-urgentize ov (current-buffer))
    thisimage))
		
(defun preview-mouse-open-error (string)
  "Display STRING in a new view buffer on click."
  (let ((buff (get-buffer-create
	       "*Preview-GhostScript-Error*")))
    (with-current-buffer buff
      (kill-all-local-variables)
      (setq buffer-undo-list t)
      (erase-buffer)
      (insert string)
      (goto-char (point-min)))
    (view-buffer-other-window
     buff nil 'kill-buffer)))

(defun preview-mouse-open-eps (file)
  "Display eps FILE in a view buffer on click."
  (let ((default-major-mode
	  (or
	   (assoc-default "x.ps" auto-mode-alist #'string-match)
	   default-major-mode))
	(buff (get-file-buffer file)))
    (save-excursion
      (if buff
	  (pop-to-buffer buff)
	(view-file-other-window file))
      (if (eq major-mode 'ps-mode)
	  (message "%s" (substitute-command-keys "\
Try \\[ps-run-start] \\[ps-run-buffer] and \
\\<ps-run-mode-map>>\\[ps-run-mouse-goto-error] on error offset." ))))))

(defun preview-gs-flag-error (ov err)
  "Make an eps error flag in overlay OV for ERR string."
  (overlay-put
   ov 'after-string
   (propertize
    (concat
     (preview-make-clickable
      nil
      "[Error]"
      "%s views error message"
      `(lambda() (interactive "@")
	 (preview-mouse-open-error
	  ,(concat preview-gs-command " "
		   (mapconcat #'shell-quote-argument
			      preview-gs-command-line
			      " ")
		   "\nGS>"
		   (aref (overlay-get ov 'queued) 2)
		   err))))
     " in "
     (preview-make-clickable
      nil
      "[EPS-file]"
      "%s views EPS file"
      `(lambda() (interactive "@")
	 (preview-mouse-open-eps
	  ,(car (nth 0 (overlay-get ov 'filenames)))))))
     'face 'preview-error-face)))

(defun preview-gs-transact (process answer)
  "Work off GhostScript transaction.
This routine is the action routine called via the process filter.
The GhostScript process buffer of PROCESS will already be selected, and
and the standard output of GhostScript up to the next prompt will be
given as ANSWER."
  (save-excursion
    (goto-char (point-max))
    (condition-case whatgives
	(let ((ov (pop preview-gs-outstanding))
	      (have-error (not (string= answer "GS>"))))
	  (when (and ov (overlay-buffer ov))
	    (let ((queued (overlay-get ov 'queued)))
	      (when queued
		(let* ((bbox (aref queued 0))
		       (img (aref queued 1))
		       (filenames (overlay-get ov 'filenames))
		       (oldfile (nth 0 filenames))
		       (newfile (nth 1 filenames)))
		  (if have-error
		      (preview-gs-flag-error ov answer)
		    (condition-case nil
			(preview-delete-file oldfile)
		      (file-error nil))
		    (overlay-put ov 'filenames (cdr filenames))
		    (preview-replace-icon
		     img
		     (preview-create-icon (car newfile)
					  preview-gs-image-type
					  (preview-ascent-from-bb
					   bbox))))
		  (overlay-put ov 'queued nil)))))
	  (while (and (< (length preview-gs-outstanding)
			 preview-gs-outstanding-limit)
		      (setq ov (pop preview-gs-queue)))
	    (let ((queued (overlay-get ov 'queued)))
	      (when (and queued
			 (not (memq ov preview-gs-outstanding))
			 (overlay-buffer ov))
		(let* ((filenames (overlay-get ov 'filenames))
		       (oldfile (nth 0 filenames))
		       (newfile (nth 1 filenames))
		       (bbox (aref queued 0))
		       (gs-line (format
				 "clear \
/preview-LaTeX-state save def << \
/PageSize [%g %g] /PageOffset [%g %g] /OutputFile (%s) \
>> setpagedevice (%s) run preview-LaTeX-state restore\n"
				 (- (aref bbox 2) (aref bbox 0))
				 (- (aref bbox 3) (aref bbox 1))
				 (- (aref bbox 0)) (aref bbox 1)
				 (car newfile)
				 (car oldfile))))
		  (setq preview-gs-outstanding
			(nconc preview-gs-outstanding
			       (list ov)))
		  (aset queued 2 gs-line)
		  (process-send-string
		   process
		   gs-line
		   )))))
	  (unless preview-gs-outstanding
	    (process-send-eof process)))
      (error (insert-before-markers (error-message-string whatgives))))))

(defcustom preview-scale-function #'preview-scale-from-face
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
  (or (and (boundp 'TeX-active-styles)
	   (catch 'return (dolist (option TeX-active-styles nil)
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


(defface preview-face '((t (:background "lightgray")))
  "Face to use for the source of preview."
  :group 'preview)

(defface preview-error-face '((t (:background "red")))
  "Face for displaying error message overlays."
  :group 'preview)

(defun preview-regenerate (ovr)
  "Pass the modified region in OVR again through LaTeX."
  (let ((begin (overlay-start ovr))
	(end (overlay-end ovr)))
    (with-current-buffer (overlay-buffer ovr)
      (preview-delete ovr)
      (TeX-region-create (TeX-region-file TeX-default-extension)
			 (buffer-substring begin end)
			 (file-name-nondirectory (buffer-file-name))
			 (save-restriction
			   (widen)
			   (let ((inhibit-point-motion-hooks t)
				 (inhibit-field-text-motion t))
			     (+ (count-lines (point-min) begin)
				(save-excursion
				  (goto-char begin)
				  (if (bolp) 0 -1)))))))
    (TeX-command "Generate Preview" 'TeX-region-file)))

(defun preview-disabled-string (ov)
  "Generate a before-string for disabled preview overlay OV."
  (concat (preview-make-clickable
	   (overlay-get ov 'preview-map)
	   (preview-string-from-image preview-icon)
	   "\
%s regenerates preview
%s kills preview"
	   `(lambda() (interactive) (preview-regenerate ,ov)))
;; icon on separate line only for stuff starting on its own line
	  (save-excursion
	    (with-current-buffer
		(overlay-buffer ov)
	      (goto-char (overlay-start ov))
	      (if (bolp) "\n" "")))))

(defun preview-disable (ovr)
  "Change overlay behaviour of OVR after source edits."
  (overlay-put ovr 'queued nil)
  (preview-remove-urgentization ovr)
  (preview-toggle ovr)
  (overlay-put ovr 'before-string (preview-disabled-string ovr))
  (dolist (filename (overlay-get ovr 'filenames))
    (condition-case nil
	(preview-delete-file filename)
      (file-error nil))
    (overlay-put ovr 'filenames nil)))
    
(defun preview-delete (ovr &rest ignored)
  "Delete preview overlay OVR, taking any associated file along.
IGNORED arguments are ignored, making this function usable as
a hook in some cases"
  (let ((filenames (overlay-get ovr 'filenames)))
    (delete-overlay ovr)
    (dolist (filename filenames)
      (condition-case nil
	  (preview-delete-file filename)
	(file-error nil)))
    (overlay-put ovr 'filenames nil)))

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
	(preview-delete ov))))

(add-hook 'kill-buffer-hook #'preview-clearout nil nil)

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
   (preview-make-clickable (overlay-get ov 'preview-map)
			   (preview-string-from-image preview-icon)
			   "\
%s redisplays preview
%s kills preview")
;; icon on separate line only for stuff starting on its own line
   (save-excursion
     (with-current-buffer
	 (overlay-buffer ov)
       (goto-char (overlay-start ov))
       (if (bolp) "\n" "")))))


(defun preview-eps-place (ov snippet)
  "Generate an image via direct Emacs EPS rendering.
Since OV already carries all necessary information,
the argument SNIPPET passed via a hook mechanism is ignored."
  (preview-ps-image (car (nth 0 (overlay-get ov 'filenames))) preview-scale))

(defun preview-active-string (ov snippet)
  "Generate before-string for active image overlay OV.
This calls the `place' hook indicated by `preview-image-type'
in `preview-image-creators' with OV and SNIPPET
and expects an image property as result."
  (preview-make-clickable
   (overlay-get ov 'preview-map)
   (preview-string-from-image (preview-call-hook 'place ov snippet))
   "%s opens text
%s kills preview"))

(defun preview-make-filename (file)
  "Generate a preview filename from FILE and `TeX-active-tempdir'.
Those consist of a CONS-cell with absolute file name as CAR
and `TeX-active-tempdir' as CDR.  `TeX-active-tempdir' is a
CONS-cell with the directory name as CAR and the reference count
as CDR."
  (setcdr TeX-active-tempdir (1+ (cdr TeX-active-tempdir)))
  (cons (expand-file-name file (car TeX-active-tempdir))
	TeX-active-tempdir))

(defun preview-delete-file (file)
  "Delete a preview FILE.
See `preview-make-filename' for a description of the data
structure.  If the containing directory becomes empty,
it gets deleted as well."
  (unwind-protect
      (delete-file (car file))
    (let ((tempdir (cdr file)))
      (when tempdir
	(if (> (cdr tempdir) 1)
	    (setcdr tempdir (1- (cdr tempdir)))
	  (setcdr file nil)
	  (delete-directory (car tempdir)))))))

(defun preview-place-preview (snippet source start end)
  "Generate and place an overlay preview image.
This generates the EPS filename used in `TeX-active-tempdir'
\(see `preview-make-filename' for its definition) for preview
snippet SNIPPET in buffer SOURCE, and uses it for the
region between START and END."
  (let ((ov (with-current-buffer source
	      (preview-clearout start end)
	      (make-overlay start end nil nil nil))))
    (overlay-put ov 'category 'preview-overlay)
    (overlay-put ov 'preview-map
		 (preview-make-clickable
		  nil nil nil
		  `(lambda() (interactive) (preview-toggle ,ov 'toggle))
		  `(lambda() (interactive) (preview-delete ,ov))))
    (overlay-put ov 'filenames (list (preview-make-filename
				      (format "preview.%03d" snippet))))
    (overlay-put ov 'strings
		 (cons (preview-active-string ov snippet)
		       (preview-inactive-string ov)))
    (preview-toggle ov t)))


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

(defcustom preview-default-option-list '("displaymath" "floats"
					 "graphics" "sections")
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
		    (const "textmath")
		    (const "sections"))
	       (repeat :inline t :tag "Other options" (string))))

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
	       '(("Generate Preview"
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
		  ("%P" preview-make-options)) ))
  (easy-menu-add-item TeX-mode-menu
		      nil
		      (let ((file 'TeX-command-on-current))
			(TeX-command-menu-entry (car (last TeX-command-list))))))

(defun preview-clean-subdir (dir)
  "Cleans out a temporary DIR with preview image files."
  (condition-case err
      (progn
	(mapc 'delete-file
	      (directory-files dir t "\\`pre" t))
	(delete-directory dir))
    (error (message "Deletion of %s failed: %s" dir
		    (error-message-string err)))))

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
(make-variable-buffer-local 'preview-snippet)
(defvar preview-snippet-start nil
  "Point of start of current preview snippet.")
(make-variable-buffer-local 'preview-snippet-start)

(defun preview-parse-messages ()
  "Turn all preview snippets into overlays.
This parses the pseudo error messages from the preview
document style for LaTeX."
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
	       (string= (match-string 2 string) "started")))
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
	      (rplaca TeX-error-file (match-string 1 string))))))
	(if (zerop preview-snippet)
	    (preview-clean-subdir (car TeX-active-tempdir))) )
    (preview-call-hook 'close)))

(defun preview-analyze-error (snippet startflag)
  "Analyze a preview diagnostic for snippet SNIPPET.
The diagnostic is for the start of the snippet if STARTFLAG
is set, and an overlay will be generated for the corresponding
file dvips put into the directory indicated by `TeX-active-tempdir'."
  
  (let* (;; We need the error message to show the user.
	 op                            ;; temporary variable
	 (error (progn
		  (setq op (point))
		  (end-of-line)
		  (buffer-substring op (point))))
	 
	 ;; And the context for the help window.
	 (context-start (point))
	 
	 ;; And the line number to position the cursor.
;;; variant 1: profiling seems to indicate the regexp-heavy solution
;;; to be favorable.  Will XEmacs like this kind of regexp?
	 (line (and (re-search-forward "\
^l\\.\\([0-9]+\\) \\(\\.\\.\\.\\)?\\(.*?\\)
\\(.*?\\)\\(\\.\\.\\.\\)?$" nil t)
		    (string-to-int (match-string 1))))
	 ;; And a string of the context to search for.
	 (string (and line (match-string 3)))
	 (after-string (and line (buffer-substring
				  (+ (match-beginning 4)
				     (- (match-end 3)
					(match-beginning 0)))
				  (match-end 4))))
;; variant 2:
;; 	 (line (and (re-search-forward "^l\\.\\([0-9]+\\) \
;; \\(\\.\\.\\.\\)?" nil t)
;; 		    (string-to-int (match-string 1))))
;; 	 (string (when line
;; 		   (setq op (point))
;; 		   (end-of-line)
;; 		   (buffer-substring op (point))))
;; 	 (after-string (when line
;; 			 (setq op (point))
;; 			 (forward-line)
;; 			 (setq op (+ (point) (- op (match-beginning 0))))
;; 			 (end-of-line)
;; 			 (setq op (buffer-substring op (point)))
;; 			 (if (and (> (length op) 2)
;; 				  (string= (substring op -3) "..."))
;; 			     (substring op 0 -3)
;; 			   op)))

	 ;; And we have now found to the end of the context.
	 (context (buffer-substring context-start (point)))
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
    (when line
      (let* ((buffer (find-file-noselect file))
	     (case-fold-search nil)
	     (next-point
	      (with-current-buffer buffer
		(save-excursion
		  (goto-line (+ offset line))
		  (if (search-forward (concat string after-string)
				      (line-end-position) t)
		      (backward-char (length after-string)))
		  (or (and startflag (preview-back-command))
		      (point))))))
	(if startflag
	    (setq preview-snippet-start next-point)
	  (if preview-snippet-start
	      (progn
		(preview-place-preview snippet
				       buffer
				       preview-snippet-start
				       next-point)
		(setq preview-snippet-start nil))
	    (message "Unexpected end of Preview snippet %d" snippet)))))))

(defun preview-get-geometry (buff)
  "Transfer display geometry parameters from current display.
Those are put in local variables `preview-scale' and
`preview-resolution'.  Calculation is done in source buffer
specified by BUFF."
  (let (scale res)
    (with-current-buffer buff
      (setq scale (funcall preview-scale-function)
	    res (cons (/ (* 25.4 (display-pixel-width))
			 (display-mm-width))
		      (/ (* 25.4 (display-pixel-height))
			 (display-mm-height)))))
    (setq preview-scale scale)
    (setq preview-resolution res)))

(defun preview-TeX-inline-sentinel (process name)
  "Sentinel function for preview.
See `TeX-sentinel-function' and `set-process-sentinel'
for definition of PROCESS and NAME."
  (if process (TeX-format-mode-line process))
  (if (eq (process-status process) 'exit)
      (preview-parse-messages)))
  
(defun TeX-inline-preview (name command file)
  "Main function called by AucTeX.
NAME, COMMAND and FILE are described in `TeX-command-list'."
  (let ((tempdir TeX-active-tempdir)
	(commandbuff (current-buffer))
	(process (TeX-run-format "Preview-LaTeX" command file)))
    (preview-get-geometry commandbuff)
    (setq TeX-active-tempdir tempdir)
    (setq TeX-sentinel-function 'preview-TeX-inline-sentinel)
    (setq TeX-parse-function 'preview-parse-TeX)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defconst preview-version
  (let ((name "$Name:  $")
	(rev "$Revision: 1.30 $"))
    (or (if (string-match "\\`[$]Name: *\\([^ ]+\\) *[$]\\'" name)
	    (match-string 1 name))
	(if (string-match "\\`[$]Revision: *\\([^ ]+\\) *[$]\\'" rev)
	    (format "CVS-%s" (match-string 1 rev)))
	"unknown"))
  "Preview version.
If not a regular release, CVS revision of `preview.el'.")

(defun preview-report-bug () "Report a bug in the preview-latex package."
  (interactive)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "preview-latex-bugs@lists.sourceforge.net"
     preview-version
     '(AUC-TeX-version
       image-types
       preview-image-type
       preview-image-creators
       preview-gs-command
       preview-gs-options
       preview-gs-outstanding-limit
       preview-scale-function
       preview-default-option-list)
     nil
     (lambda ()
       (insert (format "\nOutput from running `%s -h':\n"
		       preview-gs-command))
       (call-process preview-gs-command nil t nil "-h")
       (insert "\n"))
     "Remember to cover the basics.  Including a minimal LaTeX example
file exhibiting the problem might help."
     )))

(eval-and-compile
  (when (boundp 'preview-compatibility-macros)
    (mapc #'fmakunbound preview-compatibility-macros)
    (makunbound 'preview-compatibility-macros)))

(provide 'preview)
;;; preview.el ends here
