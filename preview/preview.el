;;; preview.el --- embed preview LaTeX images in source buffer

;; Copyright (C) 2001, 2002  Free Software Foundation, Inc.

;; Author: David Kastrup
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

;; $Id: preview.el,v 1.174 2002-12-06 18:20:24 dakas Exp $
;;
;; This style is for the "seamless" embedding of generated EPS images
;; into LaTeX source code.  Please see the README and INSTALL files
;; for further instruction.
;;
;; Please use the usual configure script for installation: more than
;; just Elisp files are involved: a LaTeX style, icon files, startup
;; code and so on.
;;
;; Quite a few things with regard to preview-latex's operation can be
;; configured by using
;; M-x customize-group RET preview RET
;;
;; Please report bugs with M-x preview-report-bug RET
;;

;;; Code:

(eval-when-compile
  (require 'tex-site)
  (require 'tex-buf)
  (require 'latex)
  (condition-case nil
      (require 'desktop)
    (file-error (message "Missing desktop package:
preview-latex buffers will not survive across sessions.")))
  (condition-case nil
      (require 'reporter)
    (file-error (message "Missing reporter library, probably from the mail-lib package:
preview-latex's bug reporting commands will probably not work.")))
  (require 'info)
  (defvar error))

;; we need the compatibility macros which do _not_ get byte-compiled.
(eval-when-compile
  (if (string-match "XEmacs" (emacs-version))
      (load-library "prv-xemacs.el")))

;; if the above load-library kicked in, this will not cause anything
;; to get loaded.
(require (if (string-match "XEmacs" (emacs-version))
	     'prv-xemacs 'prv-emacs))

(defgroup preview nil "Embed Preview images into LaTeX buffers."
  :group 'AUC-TeX
  :prefix "preview-"
  :link '(custom-manual "(preview-latex)Top")
  :link '(info-link "(preview-latex)The Emacs interface")
  :link '(url-link :tag "Homepage" "http://preview-latex.sourceforge.net"))

(defgroup preview-gs nil "Preview's GhostScript renderer."
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
    (tiff (open preview-gs-open tiff ("-sDEVICE=tiff12nc"))
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
arguments given to `preview-call-hook'.

Not all of these image types may be supported by your copy
of Ghostscript, or by your copy of Emacs."
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
		(delete '(const postscript)
			(mapcar (lambda (symbol) (list 'const (car symbol)))
				preview-image-creators))
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
  :group 'preview-gs :type 'integer)

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

(defcustom preview-prefer-TeX-bb nil
  "*Prefer TeX bounding box to EPS one if available.
If `preview-fast-conversion' is set, this option is not
 consulted since the TeX bounding box has to be used anyway."
  :group 'preview-gs
  :type 'boolean)

(defcustom preview-TeX-bb-border 0.5
  "*Additional space in pt around Bounding Box from TeX."
  :group 'preview-gs
  :type 'number)

(defvar preview-parsed-font-size nil
  "Font size as parsed from the log of LaTeX run.")
(make-variable-buffer-local 'preview-parsed-font-size)
(defvar preview-parsed-magnification nil
  "Magnification as parsed from the log of LaTeX run.")
(make-variable-buffer-local 'preview-parsed-magnification)

(defun preview-get-magnification ()
  "Get magnification from `preview-parsed-magnification'."
  (if preview-parsed-magnification
      (/ preview-parsed-magnification 1000.0) 1.0))

(defun preview-TeX-bb (list)
  "Calculate bounding box from (ht dp wd).
LIST consists of TeX dimensions in sp (1/65536 TeX point)."
  (and
   (consp list)
   (let ((dims (vconcat (mapcar
			 #'(lambda (x)
			     (/ x 65781.76)) list))))
     (vector
      (+ 72 (- preview-TeX-bb-border) (min 0 (aref dims 2)))
      (+ 720 (- preview-TeX-bb-border) (min (aref dims 0) (- (aref dims 1)) 0))
      (+ 72 preview-TeX-bb-border (max 0 (aref dims 2)))
      (+ 720 preview-TeX-bb-border (max (aref dims 0) (- (aref dims 1)) 0))))))

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

(defvar preview-gs-init-string nil
  "GhostScript setup string.")
(make-variable-buffer-local 'preview-gs-init-string)

(defvar preview-ps-file nil
  "PostScript file name for fast conversion.")
(make-variable-buffer-local 'preview-ps-file)

(defvar preview-gs-dsc nil
  "Parsed DSC information.")
(make-variable-buffer-local 'preview-gs-dsc)

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
	  (/ (* scale xres) (preview-get-magnification))
	  (/ (* scale yres) (preview-get-magnification))))

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

(defcustom preview-fast-conversion t
  "*Set this for single-file PostScript conversion.
This will have no effect when `preview-image-type' is
set to `postscript'."
  :group 'preview-latex
  :type 'boolean)

(defcustom preview-dvips-command
  "dvips -Pwww -i -E %d -o %m/preview.000"
  "*Command used for converting to separate EPS images."
  :group 'preview-latex
  :type 'string)

(defcustom preview-fast-dvips-command
  "dvips -Pwww %d -o %m/preview.ps"
  "*Command used for converting to a single PS file."
  :group 'preview-latex
  :type 'string)

(defun preview-gs-queue-empty ()
  "Kill off everything remaining in `preview-gs-queue'."
  (mapc #'preview-delete preview-gs-outstanding)
  (dolist (ov preview-gs-queue)
    (if (overlay-get ov 'queued)
	(preview-delete ov)))
  (setq preview-gs-outstanding nil)
  (setq preview-gs-queue nil))

(defvar preview-error-condition nil
  "Last error raised and to be reported.")

(defun preview-log-error (err context &optional process)
  "Log an error message to run buffer.
ERR is the caught error syndrome, CONTEXT is where it
occured, PROCESS is the process for which the run-buffer
is to be used."
  (when (or (null process) (buffer-name (process-buffer process)))
    (with-current-buffer (or (and process
				  (process-buffer process))
			     (current-buffer))
      (save-excursion
	(goto-char (or (and process
			    (process-buffer process)
			    (marker-buffer (process-mark process))
			    (process-mark process))
		       (point-max)))
	(insert-before-markers
	 (format "%s: %s\n"
		 context (error-message-string err))))))
  (setq preview-error-condition err))

(defun preview-reraise-error (&optional process)
  "Raise an error that has been logged.
Makes sure that PROCESS is removed from the \"Compilation\"
tag in the mode line."
  (when preview-error-condition
    (unwind-protect
	(signal (car preview-error-condition) (cdr preview-error-condition))
      (setq preview-error-condition nil
	    compilation-in-progress (delq process compilation-in-progress)))))

(defun preview-gs-sentinel (process string)
  "Sentinel function for rendering process.
Gets the default PROCESS and STRING arguments
and tries to restart GhostScript if necessary."
  (condition-case err
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
		    (goto-char (if (marker-buffer (process-mark process))
				   (process-mark process)
				 (point-max)))
		    (insert-before-markers preview-gs-command " "
					   (mapconcat #'shell-quote-argument
						      preview-gs-command-line
						      " ") "\n" err)))
		(delete-process process)
		(if (or (null ov)
			(eq status 'signal))
		    ;; if process was killed explicitly by signal, or if nothing
		    ;; was processed, we give up on the matter altogether.
		    (preview-gs-queue-empty)
		  
		  ;; restart only if we made progress since last call
		  (setq preview-gs-queue (nconc preview-gs-outstanding
						preview-gs-queue))
		  (setq preview-gs-outstanding nil)
		  (preview-gs-restart)))))))
    (error (preview-log-error err "GhostScript" process)))
  (preview-reraise-error process))

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
	(condition-case err
	    (preview-gs-transact process answer)
	  (error (preview-log-error err "GhostScript filter" process))))))
  (preview-reraise-error))

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
      (process-send-string process preview-gs-init-string)
      (setq mode-name "Preview-GhostScript")
      (push process compilation-in-progress)
      (TeX-command-mode-line process)
      (set-buffer-modified-p (buffer-modified-p))
      process)))

(defun preview-gs-open (imagetype gs-optionlist)
  "Start a GhostScript conversion pass.
IMAGETYPE specifies the Emacs image type for the generated
files, GS-OPTIONLIST is a list of options to pass into
GhostScript for getting that sort of image type, for
example \"-sDEVICE=png256\" will go well with 'png."
  (unless (preview-supports-image-type imagetype)
    (error "This Emacs lacks '%s image support" imagetype))
  (setq preview-gs-image-type imagetype)
  (setq preview-gs-command-line (append
				 preview-gs-options
				 gs-optionlist))
  (setq preview-gs-init-string
	(format "\
/preview-latex-do{{setpagedevice}stopped{handleerror quit}if save \
exch[count 2 roll]exch cvx \
systemdict/.runandhide known{.setsafe\
\(AFPL Ghostscript)product ne{<<>>setpagedevice}if{.runandhide}}if \
%s stopped{handleerror quit}if count 1 ne{quit}if \
aload pop restore}bind def "
		(mapconcat #'identity preview-gs-colors " ")))
  (preview-gs-queue-empty)
  (preview-parse-messages #'preview-gs-dvips-process-setup))

(defun preview-gs-dvips-process-setup ()
  "Set up Dvips process for conversions via gs."
  (setq preview-gs-command-line (append
				 preview-gs-command-line
				 (list (preview-gs-resolution
					(preview-hook-enquiry preview-scale)
					(car preview-resolution)
					(cdr preview-resolution)))))
  (let ((process (preview-start-dvips preview-fast-conversion)))
    (setq TeX-sentinel-function #'preview-gs-dvips-sentinel)
    (list process (current-buffer) TeX-active-tempdir preview-ps-file)))

(defun preview-dvips-abort ()
  "Abort a Dvips run."
  (preview-gs-queue-empty)
  (condition-case nil
      (delete-file
       (let ((gsfile preview-gs-file))
	 (with-current-buffer TeX-command-buffer
	   (funcall (car gsfile) "dvi"))))
    (file-error nil))
  (when preview-ps-file
      (condition-case nil
	  (preview-delete-file preview-ps-file)
	(file-error nil)))
  (setq TeX-sentinel-function nil))

(defun preview-gs-dvips-sentinel (process command &optional gsstart)
  "Sentinel function for indirect rendering DviPS process.
The usual PROCESS and COMMAND arguments for
`TeX-sentinel-function' apply.  Starts gs if GSSTART is set."
  (condition-case err
      (let ((status (process-status process))
	    (gsfile preview-gs-file))
	(cond ((eq status 'exit)
	       (delete-process process)
	       (setq TeX-sentinel-function nil)
	       (condition-case nil
		   (delete-file
		    (with-current-buffer TeX-command-buffer
		      (funcall (car gsfile) "dvi")))
		 (file-error nil))
	       (if preview-ps-file
		   (preview-prepare-fast-conversion))
	       (when gsstart
		 (when preview-ps-file
		   (condition-case nil
		       (preview-delete-file preview-ps-file)
		     (file-error nil)))
		 (if preview-gs-queue
		     (preview-gs-restart))))
	      ((eq status 'signal)
	       (delete-process process)
	       (preview-dvips-abort))))
    (error (preview-log-error err "DviPS sentinel" process)))
  (preview-reraise-error process))

(defun preview-gs-close (process closedata)
  "Clean up after PROCESS and set up queue accumulated in CLOSEDATA."
  (setq preview-gs-queue (nconc preview-gs-queue closedata))
  (if process
      (if preview-gs-queue
	  (if TeX-process-asynchronous
	      (if (and (eq (process-status process) 'exit)
		       (null TeX-sentinel-function))
		  ;; Process has already finished and run sentinel
		  (progn
		    (when preview-ps-file
		      (condition-case nil
			  (preview-delete-file preview-ps-file)
			(file-error nil)))
		    (preview-gs-restart))
		(setq TeX-sentinel-function (lambda (process command)
					      (preview-gs-dvips-sentinel
					       process
					       command
					       t))))
	    (TeX-synchronous-sentinel "Preview-DviPS" (cdr preview-gs-file)
				      process))
    ;; pathological case: no previews although we sure thought so.
	(delete-process process)
	(unless (eq (process-status process) 'signal)
	  (preview-dvips-abort)))))

(defun preview-eps-open ()
  "Place everything nicely for direct PostScript rendering."
  (preview-parse-messages #'preview-eps-dvips-process-setup))

(defun preview-eps-dvips-process-setup ()
  "Set up Dvips process for direct EPS rendering."
  (let* ((TeX-process-asynchronous nil)
	 (process (preview-start-dvips)))
    (TeX-synchronous-sentinel "Preview-DviPS" (cdr preview-gs-file)
			      process)
    (list process TeX-active-tempdir preview-scale)))
  
(defun preview-dsc-parse (file)
  "Parse DSC comments of FILE.
Returns a vector with offset/length pairs corresponding to
the pages.  Page 0 corresponds to the initialization section."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let ((last-pt (point-min))
	  trailer
	  pagelist
	  lastbegin
	  pt
	  case-fold-search
	  (level 0))
      (while (search-forward-regexp "\
%%\\(?:\\(BeginDocument:\\)\\|\
\\(EndDocument[\n\r]\\)\\|\
\\(Page:\\)\\|\
\\(Trailer[\n\r]\\)\\)" nil t)
	(setq pt (match-beginning 0))
	(cond ((null (memq (char-before pt) '(?\C-j ?\C-m nil))))
	      (trailer (error "Premature %%%%Trailer in `%s' at offsets %d/%d"
			      file trailer pt))
	      ((match-beginning 1)
	       (if (zerop level)
		   (setq lastbegin pt))
	       (setq level (1+ level)))
	      ((match-beginning 2)
	       (if (zerop level)
		   (error "Unmatched %%%%EndDocument in `%s' at offset %d"
			  file pt)
		 (setq level (1- level))))
	      ((> level 0))
	      ((match-beginning 3)
	       (push (list last-pt (- pt last-pt)) pagelist)
	       (setq last-pt pt))
	      ((match-beginning 4)
	       (setq trailer pt))))
      (unless (zerop level)
	(error "Unmatched %%%%BeginDocument in `%s' at offset %d"
	       file lastbegin))
      (push (list last-pt
		  (- (or trailer (point-max)) last-pt)) pagelist)
      (vconcat (nreverse pagelist)))))

(defun preview-gs-dsc-cvx (page dsc)
  "Generate PostScript code accessing PAGE in the DSC object.
The returned PostScript code will need the file on
top of the stack, and will replace it with an executable
object corresponding to the wanted page."
  (let ((curpage (aref dsc page)))
    (format "dup %d setfileposition %d()/SubFileDecode filter cvx"
	    (1- (car curpage)) (nth 1 curpage))))
  
(defun preview-ps-quote-filename (str)
  "Make a PostScript string from filename STR.
The file name is first made relative."
  (setq str (file-relative-name str))
  (let ((index 0))
    (while (setq index (string-match "[\\()]" str index))
      (setq str (replace-match "\\\\\\&" t nil str)
	    index (+ 2 index)))
    (concat "(" str ")")))

(defun preview-prepare-fast-conversion ()
  "This fixes up all parameters for fast conversion."
  (let ((file (if (consp (car preview-ps-file))
		  (caar preview-ps-file) (car preview-ps-file))))
    (setq preview-gs-dsc (preview-dsc-parse file))
    (setq preview-gs-init-string
	  (concat preview-gs-init-string
		  (format "%s(r)file dup %s exec "
			  (preview-ps-quote-filename file)
			  (preview-gs-dsc-cvx 0 preview-gs-dsc))))))

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


(defun preview-gs-place (ov snippet box run-buffer tempdir ps-file)
  "Generate an image placeholder rendered over by GhostScript.
This enters OV into all proper queues in order to make it render
this image for real later, and returns the overlay after setting
a placeholder image.  SNIPPET gives the number of the
snippet in question for the file to be generated.
BOX is a bounding box if we already know one via TeX.
RUN-BUFFER is the buffer of the TeX process,
TEMPDIR is the correct copy of `TeX-active-tempdir',
PS-FILE is a copy of `preview-ps-file'."
  (overlay-put ov 'filenames
	       (list
		(preview-make-filename
		 (or ps-file
		     (format "preview.%03d" snippet))
		 tempdir)
		(preview-make-filename
		 (format "prev%03d.%s" snippet preview-image-type)
		 tempdir)))
  (overlay-put ov 'queued
	       (vector box nil snippet))
  (overlay-put ov 'preview-image
	       (preview-nonready-copy))
  (preview-add-urgentization #'preview-gs-urgentize ov run-buffer)
  (list ov))

(defun preview-mouse-open-error (string)
  "Display STRING in a new view buffer on click."
  (let ((buff (get-buffer-create
	       "*Preview-GhostScript-Error*")))
    (with-current-buffer buff
      (kill-all-local-variables)
      (set (make-local-variable 'view-exit-action) #'kill-buffer)
      (setq buffer-undo-list t)
      (erase-buffer)
      (insert string)
      (goto-char (point-min)))
    (view-buffer-other-window buff)))

(defun preview-mouse-open-eps (file &optional position)
  "Display eps FILE in a view buffer on click.
Place point at POSITION, else beginning of file."
  (let ((default-major-mode
	  (or
	   (assoc-default "x.ps" auto-mode-alist #'string-match)
	   default-major-mode))
	(buff (get-file-buffer file)))
    (save-excursion
      (if buff
	  (pop-to-buffer buff)
	(view-file-other-window file))
      (goto-char (or position (point-min)))
      (if (eq major-mode 'ps-mode)          ; Bundled with GNU Emacs
	  (message "%s" (substitute-command-keys "\
Try \\[ps-run-start] \\[ps-run-buffer] and \
\\<ps-run-mode-map>\\[ps-run-mouse-goto-error] on error offset." )))
      (if (eq major-mode 'postscript-mode) ; Bundled with XEmacs, limited
	  (message "%s" (substitute-command-keys "\
Try \\[ps-shell] and \\[ps-execute-buffer]."))))))

(defun preview-gs-flag-error (ov err)
  "Make an eps error flag in overlay OV for ERR string."
  (let* ((file (car (nth 0 (overlay-get ov 'filenames))))
	 (str
	  (preview-make-clickable
	   nil
	   preview-error-icon
	   (if preview-ps-file
	       "%s views error message
%s views PS file"
	     "%s views error message
%s views EPS file")
	   `(lambda() (interactive "@")
	      (preview-mouse-open-error
	       ,(concat
		 (mapconcat #'shell-quote-argument
			    (cons preview-gs-command
				  preview-gs-command-line)
			    " ")
		 "\nGS>"
		 preview-gs-init-string
		 (aref (overlay-get ov 'queued) 1)
		 err)))
	   (if preview-ps-file
	       `(lambda() (interactive "@")
		  (preview-mouse-open-eps
		   ,(car file)
		   ,(nth 0 (aref preview-gs-dsc
				 (aref (overlay-get ov 'queued) 2)))))
	     `(lambda() (interactive "@")
		(preview-mouse-open-eps ,file))))))
    (overlay-put ov 'strings (cons str str))
    (preview-toggle ov)))

(defun preview-gs-transact (process answer)
  "Work off GhostScript transaction.
This routine is the action routine called via the process filter.
The GhostScript process buffer of PROCESS will already be selected, and
and the standard output of GhostScript up to the next prompt will be
given as ANSWER."
  (let ((ov (pop preview-gs-outstanding))
	(have-error (not
		     (string-match "\\`GS\\(<[0-9]+\\)?>\\'" answer ))))
    (when (and ov (overlay-buffer ov))
      (let ((queued (overlay-get ov 'queued)))
	(when queued
	  (let* ((bbox (aref queued 0))
		 (filenames (overlay-get ov 'filenames))
		 (oldfile (nth 0 filenames))
		 (newfile (nth 1 filenames)))
	    (if have-error
		(preview-gs-flag-error ov answer)
	      (condition-case nil
		  (preview-delete-file oldfile)
		(file-error nil))
	      (overlay-put ov 'filenames (cdr filenames))
	      (preview-replace-active-icon
	       ov
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
			     (or (and preview-prefer-TeX-bb
				      (aref queued 0))
				 (and (stringp (car oldfile))
				      (preview-extract-bb
				       (car oldfile)))
				 (aref queued 0)
				 (error "No bounding box"))))
		 (snippet (aref queued 2))
		 (gs-line
		  (format
		   "%s \
<</PageSize[%g %g]/PageOffset[%g %g[1 1 dtransform exch]\
{0 ge{neg}if exch}forall]/OutputFile%s>>preview-latex-do\n"
		   (if preview-ps-file
		       (concat "dup "
			       (preview-gs-dsc-cvx
				snippet
				preview-gs-dsc))
		     (format "%s(r)file cvx"
			     (preview-ps-quote-filename (car oldfile))))
		   (- (aref bbox 2) (aref bbox 0))
		   (- (aref bbox 3) (aref bbox 1))
		   (aref bbox 0) (aref bbox 1)
		   (preview-ps-quote-filename (car newfile)))))
	    (setq preview-gs-outstanding
		  (nconc preview-gs-outstanding
			 (list ov)))
	    (aset queued 1 gs-line)
	    ;; ignore errors because of dying processes: they will get
	    ;; caught by the sentinel, anyway.
	    (condition-case nil
		(process-send-string
		 process
		 gs-line)
	      (error nil))))))
    (unless preview-gs-outstanding
      (condition-case nil
	  (process-send-eof process)
	(error nil)))))

(defun preview-hook-enquiry (hook)
  "Gets a value from a configured hook.
HOOK is a list or single item, for which the first resolving to
non-nil counts.  Entries can be a callable function, or
a symbol that is consulted, or a value.  Lists are evaluated
recursively."
  (cond ((functionp hook)
	 (funcall hook))
	((consp hook)
	 (let (res)
	   (while (and (not res) hook)
	     (setq res (preview-hook-enquiry (car hook))
		   hook (cdr hook)))
	   res))
	((and (symbolp hook) (boundp hook))
	 (symbol-value hook))
	(t hook)))
			  
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

(defcustom preview-document-pt-list '(preview-parsed-font-size
  preview-auctex-font-size
  preview-default-document-pt)
  "*How `preview-document-pt' figures out the document size."
  :group 'preview-appearance
  :type
  '(repeat (choice
	    ;; This is a bug: type function seems to match variables, too.
	    (restricted-sexp :match-alternatives (functionp)
			     :tag "Function" :value preview-auctex-font-size)
	    (variable :value preview-parsed-font-size)
	    (number :value 11))))

(defun preview-auctex-font-size ()
  "Calculate the default font size of document.
If packages, classes or styles were called with an option
like 10pt, size is taken from the first such option if you
had let your document be parsed by AucTeX."
  (catch 'return (dolist (option (TeX-style-list))
		   (if (string-match "\\`\\([0-9]+\\)pt\\'" option)
		       (throw 'return
			      (string-to-number
			       (match-string 1 option)))))))

(defsubst preview-document-pt ()
  "Calculate the default font size of document."
  (preview-hook-enquiry preview-document-pt-list))

(defun preview-scale-from-face ()
  "Calculate preview scale from `preview-reference-face'.
This calculates the scale of EPS images from a document assumed
to have a default font size given by function `preview-document-pt'
so that they match the reference face in height."
  `(lambda nil
     (/ ,(/ (preview-inherited-face-attribute 'preview-reference-face :height
					      'default) 10.0)
	(preview-document-pt))))

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
  "Face to use for the preview source."
  :group 'preview-appearance)

(defface preview-reference-face '((t nil))
  "Face consulted for colors and scale of active previews.
Fallback to :inherit and 'default implemented."
  :group 'preview-appearance)

(defcustom preview-auto-reveal `(preview-arrived-via
				 ,(key-binding [left])
				 ,(key-binding [right]))
  "*Cause previews to open automatically when entered.
Possibilities are:
T autoopens,
NIL doesn't,
a symbol will have its value consulted if it exists,
defaulting to NIL if it doesn't.
A CONS-cell means to call a function for determining the value.
The CAR of the cell is the function to call which receives
the CDR of the CONS-cell in the rest of the arguments, while
point and current buffer point to the position in question.
All of the options show reasonable defaults."
  :group 'preview-appearance
  :type `(choice (const :tag "Off" nil)
		 (const :tag "On" t)
		 (symbol :tag "Indirect variable" :value reveal-mode)
		 (cons :tag "Function call"
		       :value (preview-arrived-via
			       ,(key-binding [left])
			       ,(key-binding [right]))
		       function (list :tag "Argument list"
				      (repeat :inline t sexp)))))
  
(defun preview-auto-reveal-p (mode)
  "Decide whether to auto-reveal.
Returns non-NIL if region should be auto-opened.
See `preview-auto-reveal' for definitions of MODE, which gets
set to `preview-auto-reveal'."
  (cond ((symbolp mode)
	 (and (boundp mode)
              (symbol-value mode)))
	((consp mode)
	 (apply (car mode) (cdr mode)))
	(t mode)))

(defun preview-arrived-via (&rest list)
  "Indicate auto-opening.
Returns non-NIL if called by one of the commands in LIST."
  (memq this-command list))

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

(defun preview-buffer ()
  "Run preview on current buffer."
  (interactive)
  (preview-region (point-min) (point-max)))

;; We have a big problem: When we are dumping preambles, diagnostics
;; issued in later runs will not make it to the output when the
;; predumped format skips the preamble.  So we have to place those
;; after \begin{document}.  This we can only do if regions never
;; include the preamble.  We could do this in our own functions, but
;; that would not extend to the operation of C-c C-r g RET.  So we
;; make this preamble skipping business part of TeX-region-create.
;; This will fail if the region is to contain just part of the
;; preamble -- a bad idea anyhow.

(defadvice TeX-region-create (before preview preactivate)
  "Skip preamble for the sake of predumped formats."
  (when (string-match TeX-header-end (ad-get-arg 1))
    (ad-set-arg 1
 		(prog1 (substring (ad-get-arg 1) (match-end 0))
 		  (ad-set-arg 3
			      (with-temp-buffer
				(insert (substring (ad-get-arg 1)
						   0 (match-end 0)))
				(+ (ad-get-arg 3)
				   (count-lines (point-min) (point-max))
				   (if (bolp) 0 -1))))))))

(defun preview-document ()
  "Run preview on master document."
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "Generate Preview" 'TeX-master-file))
		       
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
environments is selected."
  (interactive "p")
  (save-excursion
    (let (currenv)
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
      (preview-region
       (save-excursion (LaTeX-find-matching-begin) (point))
       (save-excursion (LaTeX-find-matching-end) (point))))))

(defun preview-section ()
  "Run preview on LaTeX section." (interactive)
  (save-excursion
    (LaTeX-mark-section)
    (preview-region (region-beginning) (region-end))))

(defun preview-next-border (backwards)
  "Search for the next interesting border for `preview-at-point'.
Searches backwards if BACKWARDS is non-nil."
  (let (history preview-state (pt (point)))
    (catch 'exit
      (while
	  (null
	   (memq
	    (setq preview-state
		  (if backwards
		      (if (> (setq pt
				   (previous-single-char-property-change
				    pt 'preview-state)) (point-min))
			  (get-char-property (1- pt) 'preview-state)
			(throw 'exit (or history (point-min))))
		    (if (< (setq pt
				 (next-single-char-property-change
				  pt 'preview-state)) (point-max))
			(get-char-property pt 'preview-state)
		      (throw 'exit (or history (point-max))))))
	    '(active inactive)))
	(setq history (and (not preview-state) pt)))
      (or history pt))))
	     
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
    (dolist (ovr (overlays-in (max (point-min) (1- (point)))
			      (min (point-max) (1+ (point)))))
      (let ((preview-state (overlay-get ovr 'preview-state)))
	(when preview-state
	  (if (eq preview-state 'disabled)
	      (preview-regenerate ovr)
	    (preview-toggle ovr 'toggle (selected-window)))
	  (throw 'exit t))))
    (preview-region (preview-next-border t)
		    (preview-next-border nil))))

(defun preview-disabled-string (ov)
  "Generate a before-string for disabled preview overlay OV."
  (concat (preview-make-clickable
	   (overlay-get ov 'preview-map)
	   preview-icon
	   "\
%s regenerates preview
%s kills preview"
	   `(lambda() (interactive) (preview-regenerate ,ov)))
;; icon on separate line only for stuff starting on its own line
	  (with-current-buffer (overlay-buffer ov)
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (overlay-start ov))
		(if (bolp) "\n" ""))))))

(defun preview-disable (ovr)
  "Change overlay behaviour of OVR after source edits."
  (overlay-put ovr 'queued nil)
  (preview-remove-urgentization ovr)
  (overlay-put ovr 'preview-image nil)
  (overlay-put ovr 'timestamp nil)
  (setcdr (overlay-get ovr 'strings) (preview-disabled-string ovr))
  (preview-toggle ovr)
  (overlay-put ovr 'preview-state 'disabled)
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
    (overlay-put ovr 'filenames nil)
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

(defun preview-clearout-at-point ()
  "Clearout any preview at point."
  (interactive)
  (preview-clearout (max (point-min) (1- (point)))
		    (min (point-max) (1+ (point)))))

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
on first use.")

(defun preview-dissect (ov timestamp)
  "Extract all persistent data from OV and TIMESTAMP it."
  (let ((filenames (butlast (nth 0 (overlay-get ov 'filenames)))))
    (overlay-put ov 'timestamp timestamp)
    (list (overlay-start ov)
	  (overlay-end ov)
	  (preview-export-image (overlay-get ov 'preview-image))
	  filenames)))

(defun preview-buffer-restore-internal (buffer-misc)
  "Restore previews from BUFFER-MISC if proper.
Remove them if they have expired."
  (let ((timestamp (visited-file-modtime)) tempdirlist files)
    (when (eq 'preview (pop buffer-misc))
      (if (equal (pop buffer-misc) timestamp)
	  (dolist (ovdata buffer-misc)
	    (setq tempdirlist
		  (apply #'preview-reinstate-preview tempdirlist
			 timestamp ovdata)))
	(dolist (ovdata buffer-misc)
	  (setq files (nth 3 ovdata))
	  (condition-case nil
	      (delete-file (nth 0 files))
	    (file-error nil))
	  (unless (member (nth 1 files) tempdirlist)
	    (push (nth 1 files) tempdirlist)))
	(dolist (dir tempdirlist)
	  (condition-case nil
	      (delete-directory dir)
	    (file-error nil)))))))


(defun preview-buffer-restore (buffer-misc)
  "At end of desktop load, reinstate previews.
This delay is so that minor modes changing buffer geometry
\(like `x-symbol-mode' does) will not wreak havoc.
BUFFER-MISC is the appropriate data to be used."
  (add-hook 'desktop-delay-hook `(lambda ()
				   (with-current-buffer ,(current-buffer)
				     (preview-buffer-restore-internal
				      ',buffer-misc)))))
  
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

(defcustom preview-auto-cache-preamble 'ask
  "*Whether to generate a preamble cache format automatically.
Possible values are NIL, T, and 'ask."
  :group 'preview-latex
  :type '(choice (const :tag "Cache" t)
		 (const :tag "Don't cache" nil)
		 (const :tag "Ask" ask)))

(defvar preview-dumped-alist nil
  "Alist of dumped masters.
The elements are (NAME . ASSOC).  NAME is the master file name
\(without extension), ASSOC is what to do with regard to this
format.  Possible values: NIL means no format is available
and none should be generated.  T means no format is available,
it should be generated on demand.  Other values mean that
a format has been generated and the Emacs-flavor specific
library keeping watch for a possible change of the preamble.")

(defun preview-cleanout-tempfiles ()
  "Clean out all directories and files with non-persistent data.
This is called as a hook when exiting Emacs."
  (mapc #'preview-kill-buffer-cleanup (buffer-list))
  (mapc #'preview-format-kill preview-dumped-alist))

(defun preview-inactive-string (ov)
  "Generate before-string for an inactive preview overlay OV.
This is for overlays where the source text has been clicked
visible.  For efficiency reasons it is expected that the buffer
is already selected and unnarrowed."
  (concat
   (preview-make-clickable (overlay-get ov 'preview-map)
			   preview-icon
			   "\
%s redisplays preview
%s kills preview")
;; icon on separate line only for stuff starting on its own line
   (with-current-buffer (overlay-buffer ov)
     (save-excursion
       (save-restriction
	 (widen)
	 (goto-char (overlay-start ov))
	 (if (bolp) "\n" ""))))))

(defun preview-eps-place (ov snippet box tempdir scale)
  "Generate an image via direct EPS rendering.
Since OV already carries all necessary information,
the argument SNIPPET passed via a hook mechanism is ignored.
BOX is a bounding box from TeX if we know one.  TEMPDIR is used
for creating the file name, and SCALE is a copy
of `preview-scale' necessary for `preview-ps-image."
  (let ((filename (preview-make-filename
		   (format "preview.%03d" snippet)
		   tempdir)))
    (overlay-put ov 'filenames (list filename))
    (overlay-put ov 'preview-image
		 (preview-ps-image (car filename)
				   scale
				   (and preview-prefer-TeX-bb box))))
  nil)

(defun preview-active-string (ov)
  "Generate before-string for active image overlay OV."
  (preview-make-clickable
   (overlay-get ov 'preview-map)
   (overlay-get ov 'preview-image)
   "%s opens text
%s kills preview"))

(defun preview-make-filename (file tempdir)
  "Generate a preview filename from FILE and TEMPDIR.
Filenames consist of a CONS-cell with absolute file name as CAR
and TEMPDIR as CDR.  TEMPDIR is a copy of `TeX-active-tempdir'
with the directory name, the reference count and its top directory
name elements.  If FILE is already in that form, the file name itself
gets converted into a CONS-cell with a name and a reference count."
  (if (consp file)
      (progn
	(if (consp (car file))
	    (setcdr (car file) (1+ (cdr (car file))))
	  (setcar file (cons (car file) 1)))
	file)
    (setcar (nthcdr 2 tempdir) (1+ (nth 2 tempdir)))
    (cons (expand-file-name file (nth 0 tempdir))
	  tempdir)))

(defun preview-delete-file (file)
  "Delete a preview FILE.
See `preview-make-filename' for a description of the data
structure.  If the containing directory becomes empty,
it gets deleted as well."
  (let ((filename
	 (if (consp (car file))
	     (and (zerop
		   (setcdr (car file) (1- (cdr (car file)))))
		  (car (car file)))
	   (car file))))
    (if filename
	(unwind-protect
	    (delete-file filename)
	  (let ((tempdir (cdr file)))
	    (when tempdir
	      (if (> (nth 2 tempdir) 1)
		  (setcar (nthcdr 2 tempdir) (1- (nth 2 tempdir)))
		(setcdr file nil)
		(delete-directory (nth 0 tempdir)))))))))

(defun preview-place-preview (snippet start end box tempdir place-opts)
  "Generate and place an overlay preview image.
This generates the filename for the preview
snippet SNIPPET in the current buffer, and uses it for the
region between START and END.  BOX is an optional preparsed
TeX bounding BOX passed on to the `place' hook.
TEMPDIR is a copy of `TeX-active-tempdir'.
PLACE-OPTS are additional arguments passed into
`preview-parse-messages'.  Returns
a list with additional info from the placement hook.
Those lists get concatenated together and get passed
to the close hook."
  (preview-clearout start end tempdir)
  (let ((ov (make-overlay start end nil nil nil)))
    (overlay-put ov 'preview-map
		 (preview-make-clickable
		  nil nil nil
		  `(lambda(event) (interactive "e")
		     (preview-toggle ,ov 'toggle event))
		  `(lambda() (interactive) (preview-delete ,ov))))
    (prog1 (apply #'preview-call-hook 'place ov snippet box
		  place-opts)
      (overlay-put ov 'strings
		   (list (preview-active-string ov)))
      (preview-toggle ov t))))

(defun preview-reinstate-preview (tempdirlist timestamp start end image filename)
  "Reinstate a single preview.
This gets passed TEMPDIRLIST, a list consisting of the kind
of entries used in `TeX-active-tempdir', and TIMESTAMP, the
time stamp under which the file got read in.  It returns an augmented
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
		    `(lambda(event) (interactive "e")
		       (preview-toggle ,ov 'toggle event))
		    `(lambda() (interactive) (preview-delete ,ov))))
      (overlay-put ov 'filenames (list filename))
      (overlay-put ov 'preview-image (preview-import-image image))
      (overlay-put ov 'strings
		   (list (preview-active-string ov)))
      (overlay-put ov 'timestamp timestamp)
      (preview-toggle ov t)))
  tempdirlist)

(defun preview-back-command (&optional nocomplex)
  "Move backward a TeX token.
If NOCOMPLEX is set, only basic tokens and no argument sequences
will be skipped over backwards."
  (let ((oldpos (point)) oldpoint)
    (condition-case nil
	(or (search-backward-regexp "\\(\\$\\$?\
\\|\\\\[^a-zA-Z@]\
\\|\\\\[a-zA-Z@]+\
\\|\\\\begin[ \t]*{[^}]+}\
\\)\\=" (line-beginning-position) t)
	    nocomplex
	    (if (eq ?\) (char-syntax (char-before)))
		(while
		    (progn
		      (setq oldpoint (point))
		      (backward-sexp)
		      (and (not (eq oldpoint (point)))
			   (eq ?\( (char-syntax (char-after))))))
	      (backward-char)))
      (error (goto-char oldpos)))))

(defcustom preview-default-option-list '("displaymath" "floats"
					 "graphics" "textmath" "sections")
  "*Specifies default options to pass to preview package.
These options are only used when the LaTeX document in question does
not itself load the preview package, namely when you use preview
on a document not configured for preview.  \"auctex\", \"active\",
\"dvips\" and \"delayed\" need not be specified here."
  :group 'preview-latex
  :type '(list (set :inline t :tag "Options known to work"
		    :format "%t:\n%v%h" :doc
"The above options are all the useful ones
at the time of the release of this package.
You should not need \"Other options\" unless you
upgraded to a fancier version of just the LaTeX style.
Please also note that `psfixbb' fails to have an effect if
`preview-fast-conversion' or `preview-prefer-TeX-bb'
are selected."
		    (const "displaymath")
		    (const "floats")
		    (const "graphics")
		    (const "textmath")
		    (const "sections")
		    (const "showlabels")
		    (const "psfixbb"))
	       (set :tag "Expert options" :inline t
		    :format "%t:\n%v%h" :doc
		    "Expert options should not be enabled permanently."
		    (const "noconfig")
		    (const "showbox")
		    (const "tracingall"))
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

(defcustom preview-LaTeX-command "%l \"\\nonstopmode\
\\PassOptionsToPackage{auctex,active,dvips}{preview}\
\\AtBeginDocument{\\ifx\\ifPreview\\undefined\
%D\\fi}\\input{%t}\""
  "*Command used for starting a preview.
See description of `TeX-command-list' for details."
  :group 'preview-latex
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (if (featurep 'latex)
	     (LaTeX-preview-setup)))
  :initialize #'custom-initialize-default)

(defun preview-goto-info-page ()
  "Read documentation for preview-latex in the info system."
  (interactive)
  (require 'info)
  (Info-goto-node "(preview-latex)"))

(eval-after-load 'info '(add-to-list 'Info-file-list-for-emacs
				     '("preview" . "preview-latex")))

(defvar preview-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-p" #'preview-at-point)
    (define-key map "\C-r" #'preview-region)
    (define-key map "\C-b" #'preview-buffer)
    (define-key map "\C-d" #'preview-document)
    (define-key map "\C-f" #'preview-cache-preamble)
    (define-key map "\C-c\C-f" #'preview-cache-preamble-off)
    (define-key map "\C-i" #'preview-goto-info-page)
    ;;  (define-key map "\C-q" #'preview-paragraph)
    (define-key map "\C-e" #'preview-environment)
    (define-key map "\C-s" #'preview-section)
    (define-key map "\C-c\C-p" #'preview-clearout-at-point)
    (define-key map "\C-c\C-r" #'preview-clearout)
    (define-key map "\C-c\C-b" #'preview-clearout-buffer)
    map))

;;;###autoload
(defun LaTeX-preview-setup ()
  "Hook function for embedding the preview package into Auc-TeX.
This is called by `LaTeX-mode-hook' and changes Auc-TeX variables
to add the preview functionality."
  (remove-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'preview-mode-setup)
  (require 'tex-buf)
  (require 'latex)
  (define-key LaTeX-mode-map "\C-c\C-p" preview-map)
  (easy-menu-define preview-menu LaTeX-mode-map
    "This is the menu for preview-latex."
    '("Preview"
      ["On/off at point" preview-at-point]
      ["Environment" preview-environment]
      ["Section" preview-section]
      ["Region" preview-region (preview-mark-active)]
      ["Buffer" preview-buffer]
      ["Document" preview-document]
      ["Clearout at point" preview-clearout-at-point]
      ["Clearout region" preview-clearout (preview-mark-active)]
      ["Clearout buffer" preview-clearout-buffer]
      ["Cache preamble" preview-cache-preamble]
      ["Cache preamble off" preview-cache-preamble-off]
      ("Customize"
       ["Browse options"
	(customize-group 'preview)]
       ["Generate custom menu"
	(easy-menu-add-item
	 nil '("Preview")
	 (customize-menu-create 'preview))])
      ["Read documentation" preview-goto-info-page]
      ["Report Bug" preview-report-bug]))
  (let ((preview-entry (list "Generate Preview" preview-LaTeX-command
			     #'TeX-inline-preview nil t)))
    (setq TeX-command-list
	  (nconc (delq
		  (assoc (car preview-entry) TeX-command-list)
		  TeX-command-list)
		 (list preview-entry))))
  (add-to-list 'TeX-error-description-list
	       '("\\(?:Package Preview Error\\|Preview\\):.*" .
"The auctex option to preview should not be applied manually.  If you
see this error message, either you did something too clever, or the
preview Emacs Lisp package something too stupid."))
  (add-to-list 'TeX-expand-list
	       '("%m" (lambda ()
			(shell-quote-argument
			 (file-relative-name
			  (preview-create-subdirectory))))) t)
  (add-to-list 'TeX-expand-list
	       '("%D" preview-make-preamble) t)
  (add-to-list 'TeX-expand-list
	       '("%P" preview-make-options) t)
  (if (eq major-mode 'latex-mode)
      (preview-mode-setup))
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

;; Hook into TeX immediately if it's loaded, use LaTeX-mode-hook if not.
(if (featurep 'latex)
    (LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup))

;;;###autoload (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(defvar preview-parse-variables
  '(("Fontsize" preview-parsed-font-size
     "\\` *\\([0-9.]+\\)pt\\'" 1 string-to-number)
    ("Magnification" preview-parsed-magnification
     "\\` *\\([0-9]+\\)\\'" 1 string-to-number)))

(defun preview-parse-messages (open-closure)
  "Turn all preview snippets into overlays.
This parses the pseudo error messages from the preview
document style for LaTeX.  OPEN-CLOSURE is called once
it is certain that we have a valid DVI file, and it has
to return in its CAR the PROCESS parameter for the CLOSE
call, and in its CDR the final stuff for the placement hook."
  (with-temp-message "locating previews..."
    (let (TeX-error-file TeX-error-offset snippet box
	  file line
	  (lsnippet 0) lstart (lfile "") lline lbuffer lpoint
	  string after-string error context-start
	  context offset
	  parsestate (case-fold-search nil)
	  (run-buffer (current-buffer))
	  (run-directory default-directory)
	  tempdir
	  close-data
	  open-data
	  fast-hook
	  slow-hook)
      ;; clear parsing variables
      (dolist (var preview-parse-variables)
	(set (nth 1 var) nil))
      (goto-char (point-min))
      (unwind-protect
	  (while
	      (re-search-forward "\
\\(^! \\)\\|\
\(\\([^()\r\n ]+\\))*\\(?: \\|\r?$\\)\\|\
)+\\( \\|\r?$\\)\\|\
 !\\(?:offset(\\([---0-9]+\\))\\|\
name(\\([^)]+\\))\\)\\|\
^Preview: \\([a-zA-Z]+\\) \\([^\n\r]*\\)\r?$" nil t)
;;; Ok, here is a line by line breakdown: match-alternative 1:
;;; \(^! \)
;;; exclamation point at start of line followed by blank: TeX error
;;; match-alternative 2:
;;; \(?:^\| \)(\([^()\n ]+\))*\(?: \|$\)
;;; Deep breath: an opening paren either at the start of the line or
;;; preceded by a space, followed by a file name (which we take to be
;;; consisting of anything but space, newline, or parens), followed
;;; immediately by 0 or more closing parens followed by
;;; either a space or the end of the line: a just opened file.
;;; Position for searching immediately after the file name so as to
;;; not miss closing parens or something.
;;; (match-string 2) is the file name.
;;; match-alternative 3:
;;; )+\( \|$\)
;;; a closing paren followed by the end of line or a space: a just
;;; closed file.
;;; match-alternative 4 (wrapped into one shy group with
;;; match-alternative 5, so that the match on first char is slightly
;;; faster):
;;; !offset(\([---0-9]+\))
;;; an AUC TeX offset message. (match-string 4) is the offset itself
;;; !name(\([^)]+\))
;;; an AUC TeX file name message.  (match-string 5) is the file name
;;; TODO: Actually, the latter two should probably again match only
;;; after a space or newline, since that it what \message produces.
;;;disabled in prauctex.def:
;;;\(?:Ov\|Und\)erfull \\.*[0-9]*--[0-9]*
;;;\(?:.\{79\}
;;;\)*.*$\)\|
;;; This would have caught overfull box messages that consist of
;;; several lines of context all with 79 characters in length except
;;; of the last one.  prauctex.def kills all such messages.
	    (cond
	     ((match-beginning 1)
	      (if (looking-at "\
\\(?:Preview\\|Package Preview Error\\): Snippet \\([---0-9]+\\) \\(started\\|ended\\(\
\\.? *(\\([---0-9]+\\)\\+\\([---0-9]+\\)x\\([---0-9]+\\))\\)?\\)\\.")
		  (progn
		    (setq snippet (string-to-int (match-string 1))
			  box (unless
				  (string= (match-string 2) "started")
				(if (match-string 4)
				    (mapcar #'(lambda (x)
						(* (preview-get-magnification)
						   (string-to-int x)))
					    (list
					     (match-string 4)
					     (match-string 5)
					     (match-string 6)))
				  t))
			  error (progn
				  (setq lpoint (point))
				  (end-of-line)
				  (buffer-substring lpoint (point)))
			  
			  ;; And the context for the help window.
			  context-start (point)
			  
			  ;; And the line number to position the cursor.
;;; variant 1: profiling seems to indicate the regexp-heavy solution
;;; to be favorable.
			  line (and (re-search-forward "\
^l\\.\\([0-9]+\\) \\(\\.\\.\\.\\)?\\([^\n\r]*?\\)\r?
\\([^\n\r]*?\\)\\(\\.\\.\\.\\)?\r?$" nil t)
				    (string-to-int (match-string 1)))
			  ;; And a string of the context to search for.
			  string (and line (match-string 3))
			  after-string (and line (buffer-substring
						  (+ (match-beginning 4)
						     (- (match-end 3)
							(match-beginning 0)))
						  (match-end 4)))
			  
			  ;; And we have now found to the end of the context.
			  context (buffer-substring context-start (point))
			  ;; We may use these in another buffer.
			  offset (car TeX-error-offset)
			  file (car TeX-error-file))
		    (when (and (stringp file) (TeX-match-extension file))
		      ;; if we are the first time round, check for fast hooks:
		      (when (null parsestate)
			(setq open-data
			      (save-excursion (funcall open-closure))
			      tempdir TeX-active-tempdir)
			(dolist
			    (lst (if (listp TeX-translate-location-hook)
				     TeX-translate-location-hook
				   (list TeX-translate-location-hook)))
			  (let ((fast
				 (and (symbolp lst)
				      (get lst 'TeX-translate-via-list))))
			    (if fast
				(setq fast-hook
				      (nconc fast-hook (list fast)))
			      (setq slow-hook
				    (nconc slow-hook (list lst)))))))
		      (condition-case err
			  (save-excursion (run-hooks 'slow-hook))
			(error (preview-log-error err "Translation hook")))
		      (push (vector file (+ line offset)
				  string after-string
				  snippet box) parsestate)))
		;; else normal error message
		(forward-line)
		(re-search-forward "^l\\.[0-9]" nil t)
		(forward-line 2)))
	     ((match-beginning 2)
	      ;; New file -- Push on stack
	      (push (match-string-no-properties 2) TeX-error-file)
	      (push 0 TeX-error-offset)
	      (goto-char (match-end 2)))
	     ((match-beginning 3)
	      ;; End of file -- Pop from stack
	      (pop TeX-error-file)
	      (pop TeX-error-offset)
	      (goto-char (1+ (match-beginning 0))))
	     ((match-beginning 4)
	      ;; Hook to change line numbers
	      (rplaca TeX-error-offset
		      (string-to-int (match-string 4))))
	     ((match-beginning 5)
	      ;; Hook to change file name
	      (rplaca TeX-error-file (match-string-no-properties 5)))
	     ((match-beginning 6)
	      (let ((var
		     (assoc (match-string-no-properties 6)
			    preview-parse-variables))
		    str)
		(when (and var
			   (string-match (nth 2 var)
					 (setq str (match-string 7))))
		  (set (nth 1 var)
		       (funcall (nth 4 var)
				(match-string-no-properties
				 (nth 3 var)
				 str))))))))
	(unwind-protect
	    (save-excursion
	      (if (null parsestate)
		  (error "LaTeX found no preview images"))
	      (setq parsestate (nreverse parsestate))
	      (condition-case err
		  (dolist (fun fast-hook)
		    (setq parsestate
			  (save-excursion (funcall fun parsestate))))
		(error (preview-log-error err "Fast translation hook")))
	      (setq snippet 0)
	      (dolist (state parsestate)
		(setq lsnippet snippet
		      file (aref state 0)
		      line (aref state 1)
		      string (aref state 2)
		      after-string (aref state 3)
		      snippet (aref state 4)
		      box (aref state 5))
		(unless (string= lfile file)
		  (set-buffer (find-file-noselect
			       (expand-file-name file run-directory)))
		  (setq lfile file))
		(save-excursion
		  (save-restriction
		    (widen)
		    ;; a fast hook might have positioned us already:
		    (if (number-or-marker-p string)
			(progn
			  (goto-char string)
			  (setq lpoint
				(if (number-or-marker-p after-string)
				    after-string
				  (line-beginning-position))))
		      (if (and (eq (current-buffer) lbuffer)
			       (<= lline line))
			  ;; while Emacs does the perfectly correct
			  ;; thing even when when the line differences
			  ;; get zero or negative, I don't trust this
			  ;; to be universally the case across other
			  ;; implementations.  Besides, if the line
			  ;; number gets smaller again, we are probably
			  ;; rereading the file, and restarting from
			  ;; the beginning will probably be faster.
			  (progn
			    (goto-char lpoint)
			    (if (/= lline line)
				(if (eq selective-display t)
				    (re-search-forward "[\n\C-m]" nil
						       'end
						       (- line lline))
				  (forward-line (- line lline)))))
			(goto-line line))
		      (setq lpoint (point))
		      (if (search-forward (concat string after-string)
					  (line-end-position) t)
			  (backward-char (length after-string))
			(search-forward string (line-end-position) t)))
		    (setq lline line
			  lbuffer (current-buffer))
		    (if box
			(progn
			  (if (and lstart (= snippet lsnippet))
			      (setq close-data
				    (nconc
				     (preview-place-preview
				      snippet
				      (save-excursion
					(preview-back-command
					 (= (prog1 (point)
					      (goto-char lstart))
					    lstart))
					(point))
				      (point)
				      (preview-TeX-bb box)
				      tempdir
				      (cdr open-data))
				     close-data))
			    (with-current-buffer run-buffer
			      (preview-log-error
			       (list 'error
				     (format
				      "End of Preview snippet %d unexpected"
				      snippet)) "Parser")))
			  (setq lstart nil))
		      ;; else-part of if box
		      (setq lstart (point))
		      ;; >= because snippets in between might have
		      ;; been ignored because of TeX-default-extension
		      (unless (>= snippet (1+ lsnippet))
			(with-current-buffer run-buffer
			  (preview-log-error
			   (list 'error
				 (format
				  "Preview snippet %d out of sequence"
				  snippet)) "Parser"))))))))
	  (preview-call-hook 'close (car open-data) close-data))))))

(defun preview-get-geometry (buff)
  "Transfer display geometry parameters from current display.
Those are put in local variables `preview-scale' and
`preview-resolution'.  Calculation is done in source buffer
specified by BUFF."
  (let (scale res colors)
    (condition-case err
	(with-current-buffer buff
	  (setq scale (preview-hook-enquiry preview-scale-function)
		res (cons (/ (* 25.4 (display-pixel-width))
			     (display-mm-width))
			  (/ (* 25.4 (display-pixel-height))
			     (display-mm-height)))
		colors (preview-gs-get-colors)))
      (error (error "Display geometry unavailable: %s"
		    (error-message-string err))))
    (setq preview-scale scale)
    (setq preview-resolution res)
    (setq preview-gs-colors colors)))

(defun preview-start-dvips (&optional fast)
  "Start a DviPS process.
If FAST is set, do a fast conversion."
  (let* ((file preview-gs-file)
	 tempdir
	 (command (with-current-buffer TeX-command-buffer
		    (prog1
			(TeX-command-expand (if fast
						preview-fast-dvips-command
					      preview-dvips-command)
					    (car file))
		      (setq tempdir TeX-active-tempdir))))
	 (name "Preview-DviPS"))
    (setq TeX-active-tempdir tempdir)
    (setq preview-ps-file (and fast
			       (preview-make-filename
				(preview-make-filename
				 "preview.ps" tempdir) tempdir)))
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
    (when (eq status 'exit)
	(condition-case err
	    (preview-call-hook 'open)
	  (error (preview-log-error err "LaTeX" process)))
	(preview-reraise-error process))))

(defcustom preview-format-extensions '(".fmt" ".efmt")
  "Possible extensions for format files.
Those are just needed for cleanup."
  :group 'preview-latex
  :type '(repeat string))

(defun preview-format-kill (format-cons)
  "Kill a cached format.
FORMAT-CONS is intended to be an element of `preview-dumped-alist'.
Tries through `preview-format-extensions'."
  (dolist (ext preview-format-extensions)
    (condition-case nil
	(delete-file (preview-dump-file-name (concat (car format-cons) ext)))
      (file-error nil))))

(defun preview-dump-file-name (file)
  "Make a file name suitable for dumping from FILE."
  (if file
      (concat (file-name-directory file)
	      "prv_"
	      (file-name-nondirectory file))
    "prv_texput"))

(defun preview-do-replacements (string replacements)
  "Perform replacements in string.
STRING is the input string, REPLACEMENTS is a list of replacements.
A replacement is a cons-cell, where the car is the match string,
and the cdr is a list of strings or symbols.  Symbols get dereferenced,
and strings get evaluated as replacement strings."
  (let (rep case-fold-search)
    (while replacements
      (setq rep (pop replacements))
      (cond ((symbolp rep)
	     (setq string (preview-do-replacements
			   string (symbol-value rep))))
	    ((string-match (car rep) string)
	     (setq string
		   (mapconcat (lambda(x)
				(if (symbolp x)
				    (symbol-value x)
				  (replace-match x t nil string)))
			      (cdr rep) ""))))))
  string)

(defcustom preview-LaTeX-command-replacements
  '(("\\`\\(pdf\\)?\\(.*\\)\\'" "\\2"))
  "Replacement for `preview-LaTeX-command'.
This is passed through `preview-do-replacements'."
  :group 'preview-latex
  :type '(repeat
	  (choice (symbol :tag "Named replacement")
		  (cons string (repeat (choice symbol string))))))

(defvar preview-format-name)

(defcustom preview-dump-replacements
  '(("\\`\\(pdf\\)?\\(e?\\)\\(la\\)?tex\
\\(\\( -\\([^ \"]\\|\"[^\"]*\"\\)*\\)*\\)\\(.*\\)\\'"
     . ("\\2initex\\4 \"&\\2\\3tex\" " preview-format-name ".ini \\7")))
  "Generate a dump command from the usual preview command."
  :group 'preview-latex
  :type '(repeat
	  (choice (symbol :tag "Named replacement")
		  (cons string (repeat (choice symbol string))))))

(defcustom preview-undump-replacements
  '(("\\`\\(e?\\)\\(la\\)?tex\\(\\( -\\([^ \"]\\|\"[^\"]*\"\\)*\\)*\\).*\
\\input{\\([^}]*\\)}.*\\'"
     . ("\\1virtex\\3 \"&" preview-format-name "\" \\6")))
  "Use a dumped format for reading preamble."
  :group 'preview-latex
  :type '(repeat
	  (choice (symbol :tag "Named replacement")
		  (cons string (repeat (choice symbol string))))))


(defun preview-cache-preamble ()
  "Dump a pregenerated format file.
For the rest of the session, this file is used when running
on the same master file."
  (interactive)
  (let* ((master (TeX-master-file))
	 (preview-format-name (preview-dump-file-name master))
	 (dump-file (concat preview-format-name ".ini"))
	 (format-name (expand-file-name master))
	 (master-file (TeX-master-file t))
	 (format-cons (assoc format-name preview-dumped-alist))
	 (preview-auto-cache-preamble nil))
    (if format-cons
	(progn
	  (preview-unwatch-preamble format-cons)
	  (preview-format-kill format-cons)
	  (setcdr format-cons nil))
      (setq format-cons (list format-name))
      (push format-cons preview-dumped-alist))
    ;; mylatex.ltx expects a file name to follow.  Bad. `.tex'
    ;; in the tools bundle is an empty file.
    (write-region "\\input mylatex.ltx \\relax\n" nil dump-file)
    (TeX-save-document master)
    (setq TeX-current-process-region-p nil)
    (prog1
	(TeX-inline-preview
	 "Cache preamble"
	 (preview-do-replacements
	  (TeX-command-expand preview-LaTeX-command 'TeX-master-file)
	  preview-dump-replacements)
	 master)
      (add-hook 'kill-emacs-hook #'preview-cleanout-tempfiles t)
      (setq TeX-sentinel-function
	    `(lambda (process string)
	       (condition-case err
		   (progn
		     (if (and (eq (process-status process) 'exit)
			      (zerop (process-exit-status process)))
			 (preview-watch-preamble
			  ,master-file
			  ',format-cons)
		       (preview-format-kill ',format-cons))
		     (delete-file ,dump-file))
		 (error (preview-log-error err "Dumping" process)))
	       (preview-reraise-error process))))))

(defun preview-cache-preamble-off (&optional old-format)
  "Clear the pregenerated format file.
The use of the format file is discontinued.
OLD-FORMAT may already contain a format-cons as
stored in `preview-dumped-alist'."
  (interactive)
  (unless old-format
    (setq old-format
	  (let ((format-file (expand-file-name (TeX-master-file))))
	    (or (assoc format-file preview-dumped-alist)
		(car (push (list format-file) preview-dumped-alist))))))
  (preview-unwatch-preamble old-format)
  (preview-format-kill old-format)
  (setcdr old-format nil))

(defun TeX-inline-preview (name command file)
  "Main function called by AUC TeX.
NAME, COMMAND and FILE are described in `TeX-command-list'."
  (setq command (preview-do-replacements command
					 preview-LaTeX-command-replacements))
  (let* ((commandbuff (current-buffer))
	 (pr-file (cons
		   (if TeX-current-process-region-p
		       'TeX-region-file
		     'TeX-master-file)
		   file))
	 (master (TeX-master-file))
	 (master-file (expand-file-name master))
	 (dumped-cons (assoc master-file
			     preview-dumped-alist)))
    (if (if dumped-cons
	    (eq (cdr dumped-cons) t)
	  (push (setq dumped-cons (cons master-file
					(if (eq preview-auto-cache-preamble 'ask)
					    (y-or-n-p "Cache preamble? ")
					  preview-auto-cache-preamble)))
		preview-dumped-alist)
	  (cdr dumped-cons))
	(prog1 (let (TeX-current-process-region-p)
		 (preview-cache-preamble))
	  (setq TeX-sentinel-function
		`(lambda (process string)
		   (funcall ,TeX-sentinel-function process string)
		   (TeX-inline-preview-internal
		    ,name ,command ,file
		    ',pr-file ,commandbuff
		    ',dumped-cons
		    ',master
		    (prog1
			(buffer-string)
		      (set-buffer ,commandbuff))))))
      (TeX-inline-preview-internal name command file
				   pr-file commandbuff dumped-cons master))))

(defun TeX-inline-preview-internal (name command file pr-file
					 commandbuff dumped-cons master
					 &optional str)
  "See doc of `TeX-inline-preview'.
Should explain meaning of NAME, COMMAND, FILE, and
PR-FILE, COMMANDBUFF, DUMPED-CONS and MASTER are
internal parameters, STR may be a log to insert into the current log."
  (let*
      ((preview-format-name (preview-dump-file-name master))
       (process
	(TeX-run-command
	 "Preview-LaTeX"
	 (if (cdr dumped-cons)
	     (preview-do-replacements
	      command preview-undump-replacements)
	   command) file)))
    (condition-case err
	(progn
	  (when str
	    (with-current-buffer (process-buffer process)
	      (save-excursion
		(goto-char (point-min))
		(insert str)
		(when (= (process-mark process) (point-min))
		  (set-marker (process-mark process) (point))))))
	  (preview-get-geometry commandbuff)
	  (setq preview-gs-file pr-file)
	  (setq TeX-sentinel-function 'preview-TeX-inline-sentinel)
	  (when (featurep 'mule)
	    (set-process-coding-system
	     process
	     (with-current-buffer commandbuff buffer-file-coding-system)))
	  (TeX-parse-reset)
	  (setq TeX-parse-function 'TeX-parse-TeX)
	  (if TeX-process-asynchronous
	      process
	    (TeX-synchronous-sentinel name file process)))
      (error (preview-log-error err "Preview" process)
	     (delete-process process)))
    (preview-reraise-error process)))

(defconst preview-version (eval-when-compile
  (let ((name "$Name:  $")
	(rev "$Revision: 1.174 $"))
    (or (if (string-match "\\`[$]Name: *\\([^ ]+\\) *[$]\\'" name)
	    (match-string 1 name))
	(if (string-match "\\`[$]Revision: *\\([^ ]+\\) *[$]\\'" rev)
	    (format "CVS-%s" (match-string 1 rev)))
	"unknown")))
  "Preview version.
If not a regular release, CVS revision of `preview.el'.")

(defconst preview-release-date
  (eval-when-compile
    (let ((date "$Date: 2002-12-06 18:20:24 $"))
      (string-match
       "\\`[$]Date: *\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)"
       date)
      (format "%s.%s%s" (match-string 1 date) (match-string 2 date)
	      (match-string 3 date))))
  "Preview release date.
In the form of yyyy.mmdd")

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
       preview-fast-conversion
       preview-prefer-TeX-bb
       preview-dvips-command
       preview-fast-dvips-command
       preview-scale-function
       preview-LaTeX-command
       preview-default-option-list
       preview-default-preamble)
     nil
     (lambda ()
       (insert (format "\nOutput from running `%s -h':\n"
		       preview-gs-command))
       (call-process preview-gs-command nil t nil "-h")
       (insert "\n"))
     "Remember to cover the basics.  Including a minimal LaTeX example
file exhibiting the problem might help."
     )))

(eval-when-compile
  (when (boundp 'preview-compatibility-macros)
    (mapc #'fmakunbound preview-compatibility-macros)))

(makunbound 'preview-compatibility-macros)

(provide 'preview)
;;; preview.el ends here
