;;; preview.el --- embed preview LaTeX images in source buffer

;; Copyright (C) 2001, 2002  Free Software Foundation, Inc.

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

;; $Id: preview.el,v 1.75 2002-03-14 17:32:52 dakas Exp $
;;
;; This style is for the "seamless" embedding of generated EPS images
;; into LaTeX source code.  Please see the README and INSTALL files
;; for further instruction.  If you know what you are doing, in a
;; nutshell
;;
;; (autoload 'LaTeX-preview-setup "preview")
;; (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
;;
;; at the correct location will enable this mode.
;;
;; Please use the usual configure script for installation.
;; Quite a few things with regard to its operation can be configured
;; by using
;; M-x customize-group RET preview RET
;; LaTeX needs to access a special style file "preview.sty".  For the
;; installation of this style file, use the provided configure and
;; INSTALL scripts.
;;
;; Please report bugs with M-x preview-report-bug RET
;;

;;; Code:

(eval-when-compile
  (require 'tex-site)
  (require 'tex-buf)
  (require 'latex)
  (require 'desktop)
  (require 'info)
  (defvar error))

(require (if (string-match "XEmacs" (emacs-version))
	     'prv-xemacs 'prv-emacs))

(defgroup preview nil "Embed Preview images into LaTeX buffers."
  :group 'AUC-TeX
  :prefix "preview-")

(defgroup preview-gs nil "Preview's GhostScript Renderer."
  :group 'preview
  :prefix "preview-")

(defgroup preview-appearance nil "Preview image appearance."
  :group 'preview
  :prefix "preview-")

(defgroup preview-latex nil "LaTeX options for preview."
  :group 'preview
  :prefix "preview-")

(defcustom preview-image-creators
  '((postscript
     (open preview-eps-open)
     (place preview-eps-place))
    (png (open preview-gs-open png ("-sDEVICE=png16m"))
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
  :group 'preview-gs
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
  :group 'preview-gs
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
  "List of directory name, top directory name and reference count.")
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

(defcustom preview-preserve-indentation t
  "*Whether to keep additional whitespace at the left of a line."
  :group 'preview-appearance :type 'boolean)

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
       (if preview-preserve-indentation
	   (min 72 (string-to-number (match-string 1)))
	 (string-to-number (match-string 1)))
       (string-to-number (match-string 2))
       (string-to-number (match-string 3))
       (string-to-number (match-string 4))
       ))))

(defcustom preview-gs-command "gs"
  "*How to call gs for conversion from EPS.  See also `preview-gs-options'."
  :group 'preview-gs
  :type 'string)

(defcustom preview-gs-options '("-q" "-dSAFER" "-dDELAYSAFER" "-dNOPAUSE"
				"-DNOPLATFONTS" "-dTextAlphaBits=4"
				"-dGraphicsAlphaBits=4")
  "*Options with which to call gs for conversion from EPS.
See also `preview-gs-command'."
  :group 'preview-gs
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
  :group 'preview-gs
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

(defvar preview-gs-colors nil
  "GhostScript color setup tokens.")
(make-variable-buffer-local 'preview-gs-colors)

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
(defvar preview-gs-file nil)
(make-variable-buffer-local 'preview-gs-file)

(defun preview-gs-sentinel (process string)
  "Sentinel function for rendering process.
Gets the default PROCESS and STRING arguments
and tries to restart GhostScript if necessary."
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (setq compilation-in-progress (delq process compilation-in-progress)))
    (when (buffer-name (process-buffer process))
      (with-current-buffer (process-buffer process)
	(TeX-command-mode-line process)
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
	      (preview-gs-restart))))))))

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
      (process-kill-without-query process)
      (set-process-sentinel process #'preview-gs-sentinel)
      (set-process-filter process #'preview-gs-filter)
      (setq mode-name "Preview-GhostScript")
      (push process compilation-in-progress)
      (TeX-command-mode-line process)
      (set-buffer-modified-p (buffer-modified-p))
      process)
    ))

(defalias 'preview-gs-close #'ignore)

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
  (let ((process (preview-start-dvips)))
    (setq TeX-sentinel-function #'preview-gs-dvips-sentinel)
    (preview-parse-messages)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel "Preview-DviPS" (cdr preview-gs-file)
				process))))

(defun preview-eps-open ()
  "Place everything nicely for direct PostScript rendering."
  (let* ((TeX-process-asynchronous nil)
	 (process (preview-start-dvips)))
    (TeX-synchronous-sentinel "Preview-DviPS" (cdr preview-gs-file)
			      process))
  (preview-parse-messages))
  

(defun preview-gs-dvips-sentinel (process command)
  "Sentinel function for indirect rendering DviPS process.
The usual PROCESS and COMMAND arguments for
`TeX-sentinel-function' apply."
  (let ((status (process-status process)))
    (cond ((eq status 'exit)
	   (delete-process process)
	   (preview-gs-restart))
	  ((eq status 'signal)
	   (delete-process process)
	   (dolist (ov preview-gs-queue)
	     (if (overlay-get ov 'queued)
		 (preview-delete ov)))
	   (setq preview-gs-queue nil)
	   (preview-clean-subdir (nth 0 TeX-active-tempdir))))))

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
		  nil
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
\\<ps-run-mode-map>\\[ps-run-mouse-goto-error] on error offset." ))))))

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
	  ,(concat
		   (mapconcat #'shell-quote-argument
			      (cons preview-gs-command
				    preview-gs-command-line)
			      " ")
		   "\nGS>"
		   (aref (overlay-get ov 'queued) 1)
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
		       (img (overlay-get ov 'preview-image))
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
		       (bbox (aset queued 0
				   (preview-extract-bb (car oldfile))))
		       (gs-line (format
				 "clear \
<< \
/PageSize [%g %g] /PageOffset [%g %g] /OutputFile (%s) \
>> setpagedevice [save] %s (%s) (r) file cvx \
systemdict /.runandhide known revision 700 ge and {.setsafe {.runandhide}} if \
stopped {handleerror quit} if count 1 ne {quit} if \
cleardictstack 0 get restore\n"
				 (- (aref bbox 2) (aref bbox 0))
				 (- (aref bbox 3) (aref bbox 1))
				 (- (aref bbox 0)) (aref bbox 1)
				 (car newfile)
				 (mapconcat #'identity
					    preview-gs-colors " ")
				 (car oldfile))))
		  (setq preview-gs-outstanding
			(nconc preview-gs-outstanding
			       (list ov)))
		  (aset queued 1 gs-line)
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
  :group 'preview-appearance
  :type '(choice (function-item preview-scale-from-face)
		 (const 1.0)
		 (number :value 1.0)
		 (function :value preview-scale-from-face)))

(defcustom preview-default-document-pt 10
  "*Assumed document point size for `preview-scale-from-face'.
If the point size (such as 11pt) of the document cannot be
determined from the document options itself, assume this size.
This is for matching screen font size and previews."
  :group 'preview-appearance
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
  (catch 'return (dolist (option (TeX-style-list) preview-default-document-pt)
		   (if (string-match "\\`\\([0-9]+\\)pt\\'" option)
		       (throw 'return
			      (string-to-number
			       (match-string 1 option)))))))

(defun preview-scale-from-face ()
  "Calculate preview scale from `preview-reference-face'.
This calculates the scale of EPS images from a document assumed
to have a default font size given by function `preview-document-pt'
so that they match the reference face in height."
  (/ (preview-inherited-face-attribute 'preview-reference-face :height
				       'default) 10.0 (preview-document-pt)))

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


(defface preview-face '((((background dark))
			 (:background "dark slate gray"))
			(t
			 (:background "beige")))
  "Face to use for the source of preview."
  :group 'preview-appearance)

(defface preview-error-face '((((class color)) (:background "red"))
			      (t (:inverse-video t)))
  "Face for displaying error message overlays."
  :group 'preview-appearance)

(defface preview-reference-face '((t nil))
  "Face consulted for colors and scale of active previews.
Fallback to :inherit and 'default implemented."
  :group 'preview-appearance)

(defun preview-regenerate (ovr)
  "Pass the modified region in OVR again through LaTeX."
  (let ((begin (overlay-start ovr))
	(end (overlay-end ovr)))
    (with-current-buffer (overlay-buffer ovr)
      (preview-delete ovr)
      (preview-region begin end))))

(defun preview-region (begin end)
  "Run preview on region between BEGIN and END."
  (interactive "r")
;;  (preview-clearout begin end)
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
			      (if (bolp) 0 -1))))))
  (TeX-command "Generate Preview" 'TeX-region-file))

(defcustom preview-inner-environments '("Bmatrix" "Vmatrix" "aligned"
					"array" "bmatrix" "cases"
					"gathered" "matrix" "pmatrix"
					"smallmatrix" "split"
					"subarray" "vmatrix")
  "Environments not to be previewed on their own."
  :group 'preview-latex
  :type '(repeat string))


(defun preview-environment (count)
  "Run preview on LaTeX environment.
This avoids running environments through preview that are
indicated in `preview-inner-environments'.  If you use a prefix
argument COUNT, the corresponding level of outward nested
environments is selected." (interactive "p")
  (save-excursion
    (let (currenv pos startp endp)
      (dotimes (i (1- count))
	(setq currenv (LaTeX-current-environment))
	(if (string= currenv "document")
	    (error "No enclosing outer environment found"))
	(LaTeX-find-matching-begin))
      (while (member (setq currenv (LaTeX-current-environment))
		     preview-inner-environments)
	(LaTeX-find-matching-begin))
      (if (string= currenv "document")
	  (error "No enclosing outer environment found"))
      (let ((startp (save-excursion (LaTeX-find-matching-begin) (point)))
	    (endp (save-excursion (LaTeX-find-matching-end) (point))))
	(preview-region startp endp)))))

(defun preview-section ()
  "Run preview on LaTeX section." (interactive)
  (save-excursion
    (LaTeX-mark-section)
    (preview-region (region-beginning) (region-end))))

(defun preview-next-border (backwards)
  "Search for the next interesting border for `preview-at-point'.
Searches backwards if BACKWARDS is non-nil."
  (let (history preview-state (pt (point)))
      (while
	  (null
	   (memq
	    (setq preview-state
		  (if backwards
		      (if (> (setq pt
				   (previous-single-char-property-change
				    pt 'preview-state)) (point-min))
			  (get-char-property (1- pt) 'preview-state)
			'active)
		    (if (< (setq pt
				 (next-single-char-property-change
				  pt 'preview-state)) (point-max))
			(get-char-property pt 'preview-state)
		      'active)))
	    '(active inactive)))
	(setq history (and (not preview-state) pt)))
      (or history pt)))
	     
(defun preview-at-point ()
  "Do the appropriate preview thing at point.
If the cursor is positioned on or inside of a preview area, this
toggles its visibility, regenerating the preview if necessary.  If
not, it will run the surroundings through preview.  The surroundings
include all areas up to the next valid preview, unless invalid
previews occur before, in which case the area will include the last
such preview."
  (interactive)
  (catch 'exit
    (dolist (ovr (overlays-at (point)))
      (let ((preview-state (overlay-get ovr 'preview-state)))
	(when preview-state
	  (if (eq preview-state 'disabled)
	      (preview-regenerate ovr)
	    (preview-toggle ovr 'toggle))
	  (throw 'exit t))))
    (preview-region (preview-next-border t)
		    (preview-next-border nil))))

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
  (overlay-put ovr 'timestamp nil)
  (preview-toggle ovr)
  (overlay-put ovr 'preview-state 'disabled)
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
	(file-error nil)))))

(defun preview-clearout (&optional start end keep-dir timestamp)
  "Clear out all previews in the current region.
When called interactively, the current region is used.
Non-interactively, the region between START and END is
affected.  Those two values default to the borders of
the entire buffer.  If KEEP-DIR is set to a value from
`TeX-active-tempdir', previews associated with that
directory are kept.  The same holds for previews with
the given value of TIMESTAMP."
  (interactive "r")
  (dolist (ov (overlays-in (or start (point-min))
			   (or end (point-max))))
    (and (overlay-get ov 'preview-state)
	 (not (rassq keep-dir (overlay-get ov 'filenames)))
	 (not (and timestamp
		   (equal timestamp (overlay-get ov 'timestamp))))
	 (preview-delete ov))))

(defun preview-clearout-buffer (&optional buffer)
  "Clearout BUFFER from previews, current buffer if nil."
  (interactive)
  (if buffer
      (with-current-buffer buffer (preview-clearout))
    (preview-clearout)))

(defun preview-kill-buffer-cleanup (&optional buf)
  "This is a cleanup function just for use in hooks.
Cleans BUF or current buffer.  The difference to
`preview-clearout-buffer' is that previews
associated with the last buffer modification time are
kept."
  (with-current-buffer (or buf (current-buffer))
    (save-restriction
      (widen)
      (preview-clearout (point-min) (point-max) nil (visited-file-modtime)))))

(add-hook 'kill-buffer-hook #'preview-kill-buffer-cleanup)
(add-hook 'before-revert-hook #'preview-kill-buffer-cleanup)

(defun desktop-buffer-preview-misc-data ()
  "Hook function that extracts previews for persistent sessions."
  (unless (buffer-modified-p)
    (save-restriction
      (widen)
      (let (save-info (timestamp (visited-file-modtime)))
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (when (and (memq (overlay-get ov 'preview-state) '(active inactive))
		     (null (overlay-get ov 'queued))
		     (eq 1 (length (overlay-get ov 'filenames))))
	    (push (preview-dissect ov timestamp) save-info)))
	(and save-info
	     (cons 'preview (cons timestamp save-info)))))))

(add-hook 'desktop-buffer-misc-functions #'desktop-buffer-preview-misc-data)

(defvar preview-temp-dirs nil
"List of top level temporary directories in use from preview.
Any directory not in this list will be cleared out by preview
on first use."
)

(defun preview-dissect (ov timestamp)
  "Extract all persistent data from OV and TIMESTAMP it."
  (let ((filenames (butlast (nth 0 (overlay-get ov 'filenames)))))
    (overlay-put ov 'timestamp timestamp)
    (list (overlay-start ov)
	  (overlay-end ov)
	  (overlay-get ov 'preview-image)
	  filenames)))

(defun preview-buffer-restore (buffer-misc)
  "Restore previews from BUFFER-MISC if proper."
  (and (eq 'preview (pop desktop-buffer-misc))
       (equal (pop desktop-buffer-misc)
	      (visited-file-modtime))
       (let (tempdirlist)
	 (dolist (ovdata desktop-buffer-misc)
	   (setq tempdirlist
		 (apply #'preview-reinstate-preview tempdirlist ovdata))))))
  
(defun desktop-buffer-preview ()
  "Hook function for restoring persistent previews into a buffer."
  (and (eq (car desktop-buffer-misc) 'preview)
       desktop-buffer-file-name
       (file-readable-p desktop-buffer-file-name)
       (let ((buf (find-file-noselect desktop-buffer-file-name)))
	 (with-current-buffer buf
	   (preview-buffer-restore desktop-buffer-misc)
	   buf))))

(add-hook 'desktop-buffer-handlers 'desktop-buffer-preview)

(defun preview-cleanout-tempfiles ()
  "Clean out all directories with non-persistent previews.
This is called as a hook when exiting Emacs."
  (mapc #'preview-kill-buffer-cleanup (buffer-list)))

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

(defun preview-active-string (ov image)
  "Generate before-string for active image overlay OV.
The IMAGE for clicking is passed in as an argument."
  (preview-make-clickable
   (overlay-get ov 'preview-map)
   (preview-string-from-image image)
   "%s opens text
%s kills preview"))

(defun preview-make-filename (file)
  "Generate a preview filename from FILE and `TeX-active-tempdir'.
Those consist of a CONS-cell with absolute file name as CAR
and `TeX-active-tempdir' as CDR.  `TeX-active-tempdir' is a
list with the directory name, the reference count and its top directory
name elements."
  (setcar (nthcdr 2 TeX-active-tempdir) (1+ (nth 2 TeX-active-tempdir)))
  (cons (expand-file-name file (nth 0 TeX-active-tempdir))
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
	(if (> (nth 2 tempdir) 1)
	    (setcar (nthcdr 2 tempdir) (1- (nth 2 tempdir)))
	  (setcdr file nil)
	  (delete-directory (nth 0 tempdir)))))))

(defun preview-place-preview (snippet source start end)
  "Generate and place an overlay preview image.
This generates the EPS filename used in `TeX-active-tempdir'
\(see `preview-make-filename' for its definition) for preview
snippet SNIPPET in buffer SOURCE, and uses it for the
region between START and END."
  (let* ((save-temp TeX-active-tempdir)
	 (ov (with-current-buffer source
	       (preview-clearout start end save-temp)
	       (make-overlay start end nil nil nil))) image)
    (overlay-put ov 'preview-map
		 (preview-make-clickable
		  nil nil nil
		  `(lambda() (interactive) (preview-toggle ,ov 'toggle))
		  `(lambda() (interactive) (preview-delete ,ov))))
    (overlay-put ov 'filenames (list (preview-make-filename
				      (format "preview.%03d" snippet))))
    (overlay-put ov 'preview-image
		 (setq image (preview-call-hook 'place ov snippet)))
    (overlay-put ov 'strings
		 (cons (preview-active-string ov image)
		       (preview-inactive-string ov)))
    (preview-toggle ov t)))

(defun preview-reinstate-preview (tempdirlist start end image filename)
  "Reinstate a single preview.
This gets passed TEMPDIRLIST, a list consisting of the kind
of entries used in `TeX-active-tempdir' and returns an augmented
list.  START and END give the buffer location where the preview
is to be situated, IMAGE the image to place there, and FILENAME
the file to use: a triple consisting of filename, its temp directory
and the corresponding topdir."
  (when (file-readable-p (car filename))
    (setq TeX-active-tempdir
	  (or (assoc (nth 1 filename) tempdirlist)
	      (car (push (append (cdr filename) (list 0)) tempdirlist))))
    (setcar (nthcdr 2 TeX-active-tempdir) (1+ (nth 2 TeX-active-tempdir)))
    (setcar (cdr TeX-active-tempdir)
	    (car (or (member (nth 1 TeX-active-tempdir) preview-temp-dirs)
		     (progn
		       (add-hook 'kill-emacs-hook #'preview-cleanout-tempfiles t)
		       (push (nth 1 TeX-active-tempdir) preview-temp-dirs)))))
    (setcdr filename TeX-active-tempdir)
    (let ((ov (make-overlay start end nil nil nil)))
      (overlay-put ov 'preview-map
		   (preview-make-clickable
		    nil nil nil
		    `(lambda() (interactive) (preview-toggle ,ov 'toggle))
		    `(lambda() (interactive) (preview-delete ,ov))))
      (overlay-put ov 'filenames (list filename))
      (overlay-put ov 'preview-image image)
      (overlay-put ov 'strings
		   (cons (preview-active-string
			  ov image)
			 (preview-inactive-string ov)))
      (preview-toggle ov t)))
  tempdirlist)

(defun preview-back-command (&optional posn buffer)
  "Move backward a TeX token from POSN in BUFFER.
Actually, this does not move but only returns the position.
Defaults to point in current buffer."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if posn (goto-char posn))
    (condition-case nil
	(or (search-backward-regexp "\\(\\$\\$?\
\\|\\\\[^a-zA-Z@]\
\\|\\\\[a-zA-Z@]+\
\\|\\\\begin[ \t]*{[^}]+}\
\\)\\=" (line-beginning-position) t)
	    (progn
	      (while
		  (let ((oldpoint (point)))
		    (backward-sexp)
		    (and (not (eq oldpoint (point)))
			 (eq ?\( (char-syntax (char-after))))))
	      (point)))
      (error nil))))

(defcustom preview-default-option-list '("displaymath" "floats"
					 "graphics" "sections")
  "*Specifies default options to pass to preview package.
These options are only used when the LaTeX document in question does
not itself load the preview package, namely when you use preview
on a document not configured for preview.  \"auctex\", \"active\"
and \"delayed\" need not be specified here."
  :group 'preview-latex
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
		    (const "sections")
		    (const "noconfig")
		    (const "psfixbb"))
	       (repeat :inline t :tag "Other options" (string))))

(defun preview-make-options ()
  "Create default option list to pass into LaTeX preview package."
  (mapconcat #'identity preview-default-option-list ","))

(defcustom preview-default-preamble '("\\RequirePackage[%P]{preview}")
  "*Specifies default preamble to add to a LaTeX document.
If the document does not itself load the preview package, that is,
when you use preview on a document not configured for preview, this
list of LaTeX commands is inserted just before \\begin{document}."
  :group 'preview-latex
  :type '(list (repeat :inline t :tag "Preamble commands" (string))))

(defun preview-make-preamble ()
  "Create default preamble to add to LaTeX document."
  (mapconcat #'identity preview-default-preamble "\n"))

(defcustom preview-LaTeX-command "%l '\\nonstopmode\
\\PassOptionsToPackage{auctex,active}{preview}\
\\AtBeginDocument{\\ifx\\ifPreview\\undefined\
%D\\fi}\\input{%t}'"
  "*Command used for starting a preview.
See description of `TeX-command-list' for details."
  :group 'preview-latex
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (if (featurep 'latex)
	     (LaTeX-preview-setup)))
  :initialize #'custom-initialize-default)

(defcustom preview-dvips-command
  "dvips -Pwww -i -E %d -o %m/preview.000"
  "*Command used for converting to single EPS images."
  :group 'preview-latex
  :type 'string)

(defun preview-goto-info-page ()
  "Read documentation for preview-latex in the info system."
  (interactive)
  (require 'info)
  (Info-goto-node "(preview-latex)"))

(eval-after-load 'info '(add-to-list 'Info-file-list-for-emacs
				     '("preview" . "preview-latex")))

;;;###autoload
(defun LaTeX-preview-setup ()
  "Hook function for embedding the preview package into Auc-TeX.
This is called by `LaTeX-mode-hook' and changes Auc-TeX variables
to add the preview functionality."
  (remove-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (require 'tex-buf)
  (let ((preview-entry (list "Generate Preview" preview-LaTeX-command
			     #'TeX-inline-preview nil)))
    (setq TeX-command-list
	  (nconc (delq
		  (assoc (car preview-entry) TeX-command-list)
		  TeX-command-list)
		 (list preview-entry))))
  (add-to-list 'TeX-error-description-list
	       '("Package Preview Error.*" .
"The auctex option to preview should not be applied manually.  If you
see this error message, either you did something too clever, or the
preview Emacs Lisp package something too stupid."))
  (add-hook 'TeX-translate-location-hook #'preview-translate-location)
  (add-to-list 'TeX-expand-list
	       '("%m" (lambda ()
			(shell-quote-argument
			 (preview-create-subdirectory)))) t)
  (add-to-list 'TeX-expand-list
	       '("%D" preview-make-preamble) t)
  (add-to-list 'TeX-expand-list
	       '("%P" preview-make-options) t)
  (define-key LaTeX-mode-map "\C-c\C-p\C-p" #'preview-at-point)
  (define-key LaTeX-mode-map "\C-c\C-p\C-r" #'preview-region)
  (define-key LaTeX-mode-map "\C-c\C-p\C-i" #'preview-goto-info-page)
;;  (define-key LaTeX-mode-map "\C-c\C-p\C-q" #'preview-paragraph)
  (define-key LaTeX-mode-map "\C-c\C-p\C-e" #'preview-environment)
  (define-key LaTeX-mode-map "\C-c\C-p\C-s" #'preview-section)
  (define-key LaTeX-mode-map "\C-c\C-p\C-c\C-r" #'preview-clearout)
  (define-key LaTeX-mode-map "\C-c\C-p\C-c\C-b" #'preview-clearout-buffer)
  (preview-with-LaTeX-menus
   (easy-menu-add-item nil
		       '("Command")
		       (TeX-command-menu-entry
			(assoc "Generate Preview" TeX-command-list)))
   (easy-menu-add-item nil '("LaTeX")
		       '("Preview"
			 ["on/off at point" preview-at-point t]
			 ["Environment" preview-environment t]
			 ["Section" preview-section t]
			 ["Region" preview-region mark-active]
			 ["Clearout region" preview-clearout mark-active]
			 ["Clearout buffer" preview-clearout-buffer t]
			 ("Customize"
			  ["Browse options"
			   (customize-group 'preview) t]
			  ["Generate custom menu"
			   (easy-menu-add-item
			    nil '("LaTeX" "Preview")
			    (customize-menu-create 'preview)) t])
			 ["Read documentation" preview-goto-info-page t])
		       "Miscellaneous"))
  (if (boundp 'desktop-buffer-misc)
      (preview-buffer-restore desktop-buffer-misc)))

(defun preview-clean-subdir (dir)
  "Cleans out a temporary DIR with preview image files."
  (condition-case err
      (progn
	(mapc #'delete-file
	      (directory-files dir t "\\`pre" t))
	(delete-directory dir))
    (error (message "Deletion of `%s' failed: %s" dir
		    (error-message-string err)))))

(defun preview-clean-topdir (topdir)
  "Cleans out TOPDIR from temporary directories.
This does not erase the directory itself since its permissions
might be needed for colloborative work on common files."
  (mapc #'preview-clean-subdir
	(condition-case nil
	    (directory-files topdir t "\\`tmp" t)
	  (file-error nil))))

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
    (if (file-directory-p topdir)
	(unless (member topdir preview-temp-dirs)
	  ;;  Cleans out the top preview directory by
	  ;;  removing subdirs possibly left from a previous session.
	  (preview-clean-topdir topdir)
	  (push topdir preview-temp-dirs))
      (make-directory topdir)
      (add-to-list 'preview-temp-dirs topdir))
    (add-hook 'kill-emacs-hook #'preview-cleanout-tempfiles t)
    (setq TeX-active-tempdir
	  (list (make-temp-file (expand-file-name
			   "tmp" (file-name-as-directory topdir)) t)
		topdir
		0))
    (nth 0 TeX-active-tempdir)))

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
(if (featurep 'latex)
    (LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup))

;;;###autoload (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
      
(defun preview-parse-messages ()
  "Turn all preview snippets into overlays.
This parses the pseudo error messages from the preview
document style for LaTeX."
  (with-temp-message "locating previews..."
    (TeX-parse-reset)
    (unwind-protect
	(let ((parsestate (list 0 nil nil nil nil nil)))
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
		(setq parsestate
		      (apply #'preview-analyze-error
			     (string-to-int (match-string 1 string))
			     (string= (match-string 2 string) "started")
			     parsestate)))
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
	  (if (zerop (car parsestate))
	      (preview-clean-subdir (nth 0 TeX-active-tempdir))) )
      (preview-call-hook 'close))))

(defun preview-analyze-error (snippet start lsnippet lstart lfile lline lbuffer lpoint)
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
    (if start
	(unless (eq snippet (1+ lsnippet))
	  (message "Preview snippet %d out of sequence" snippet))
      (unless (eq snippet lsnippet)
	(message "End of Preview snippet %d unexpected" snippet)
	(setq lstart nil)))
    (setq lsnippet snippet)
    (when line
      (setq line (+ line offset))
      (let* ((buffer (if (string= lfile file)
			 lbuffer
		       (find-file-noselect (setq lfile file))))
	     (case-fold-search nil)
	     (next-point
	      (with-current-buffer buffer
		(save-excursion
		  (if (eq buffer lbuffer)
		      (progn
			(goto-char lpoint)
			(if (eq selective-display t)
			    (re-search-forward "[\n\C-m]" nil 'end (- line lline))
			  (forward-line (- line lline))))
		    (goto-line line))
		  (setq lline line
			lpoint (point)
			lbuffer buffer)
		  (if (search-forward (concat string after-string)
				      (line-end-position) t)
		      (backward-char (length after-string))
		    (search-forward string (line-end-position) t))
		  (point)))))
	(if start
	    (setq lstart next-point)
	  (if lstart
	      (progn
		(preview-place-preview
		 snippet
		 buffer
		 (if (eq next-point lstart)
		     lstart
		   (or
		    (preview-back-command lstart
					  buffer)
		    lstart))
		 next-point)
		(setq lstart nil))
	    (message "Unexpected end of Preview snippet %d" snippet))))))
  (list lsnippet lstart lfile lline lbuffer lpoint))

(defun preview-get-geometry (buff)
  "Transfer display geometry parameters from current display.
Those are put in local variables `preview-scale' and
`preview-resolution'.  Calculation is done in source buffer
specified by BUFF."
  (let (scale res colors)
    (with-current-buffer buff
      (setq scale (if (functionp preview-scale-function)
		      (funcall preview-scale-function)
		    preview-scale-function)
	    res (cons (/ (* 25.4 (display-pixel-width))
			 (display-mm-width))
		      (/ (* 25.4 (display-pixel-height))
			 (display-mm-height)))
	    colors (preview-gs-get-colors)))
    (setq preview-scale scale)
    (setq preview-resolution res)
    (setq preview-gs-colors colors)))

(defun preview-start-dvips ()
  "Start a DviPS process."
  (let* ((file preview-gs-file)
	 tempdir
	 (command (with-current-buffer TeX-command-buffer
		    (prog1
			(TeX-command-expand preview-dvips-command
					    (car file))
		      (setq tempdir TeX-active-tempdir))))
	 (name "Preview-DviPS"))
    (setq TeX-active-tempdir tempdir)
    (goto-char (point-max))
    (insert-before-markers "Running `" name "' with ``" command "''\n")
    (setq mode-name name)
    (setq TeX-sentinel-function
	  (lambda (process name) (message "%s: done." name)))
    (if TeX-process-asynchronous
	(let ((process (start-process name (current-buffer) TeX-shell
				      TeX-shell-command-option
				      command)))
	  (if TeX-after-start-process-function
	      (funcall TeX-after-start-process-function process))
	  (TeX-command-mode-line process)
	  (set-process-filter process 'TeX-command-filter)
	  (set-process-sentinel process 'TeX-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (push process compilation-in-progress)
	  (sit-for 0)
	  process)
      (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process TeX-shell nil (current-buffer) nil
		    TeX-shell-command-option
		    command))))

(defun preview-TeX-inline-sentinel (process name)
  "Sentinel function for preview.
See `TeX-sentinel-function' and `set-process-sentinel'
for definition of PROCESS and NAME."
  (if process (TeX-format-mode-line process))
  (let ((status (process-status process)))
    (if (memq status '(signal exit))
	(delete-process process))
    (if (eq status 'exit)
	(unwind-protect
	    (preview-call-hook 'open)
	  (setq compilation-in-progress
		(delq process compilation-in-progress))))))

(defun TeX-inline-preview (name command file)
  "Main function called by AucTeX.
NAME, COMMAND and FILE are described in `TeX-command-list'."
  (let ((commandbuff (current-buffer))
	(pr-file (cons
		  (if TeX-current-process-region-p
		      'TeX-region-file
		    'TeX-master-file)
		  file))
	(process (TeX-run-format "Preview-LaTeX" command file)))
    (preview-get-geometry commandbuff)
    (setq preview-gs-file pr-file)
    (setq TeX-sentinel-function 'preview-TeX-inline-sentinel)
    (setq TeX-parse-function 'preview-parse-TeX)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defconst preview-version (eval-when-compile
  (let ((name "$Name:  $")
	(rev "$Revision: 1.75 $"))
    (or (if (string-match "\\`[$]Name: *\\([^ ]+\\) *[$]\\'" name)
	    (match-string 1 name))
	(if (string-match "\\`[$]Revision: *\\([^ ]+\\) *[$]\\'" rev)
	    (format "CVS-%s" (match-string 1 rev)))
	"unknown")))
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
       preview-dvips-command
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
    (mapc #'fmakunbound preview-compatibility-macros)))

(makunbound 'preview-compatibility-macros)

(provide 'preview)
;;; preview.el ends here
