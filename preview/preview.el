;;; preview.el --- embed preview LaTeX images in source buffer

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: David Kastrup <David.Kastrup@neuroinformatik.ruhr-uni-bochum.de>
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

;; $Id: preview.el,v 1.2 2001-09-14 14:43:44 dakas Exp $
;;
;; This style is for the "seamless" embedding of generated EPS images
;; into LaTeX source code.  The current usage is to put
;; (require 'preview)
;; into your .emacs file and the file somewhere into your load-path,
;; preferably byte-compiled.  Since this stuff is pre-alpha, no
;; customization and similar has been added up to now and
;; documentation is scarce.  It need a style file "preview.sty"
;; installed in your LaTeX path which probably should have been
;; distributed together with this file in the form of a file
;; "preview.dtx" and "preview.ins".  Running tex on "preview.ins" will
;; give you "preview.drv" which you can latex for the LaTeX
;; documentation and some files to put into your TeX path.  Of these,
;; "preview.sty" and "prauctex.def" are mandatory for this package to
;; function.

;;; Code:

(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer
      standard-output
      (call-process shell-file-name nil t nil shell-command-switch command))))


(defun preview-extract-bb (filename)
  "Extract EPS bounding box vector from FILENAME."
  (let ((str
	 (with-output-to-string
	   (with-current-buffer
	       standard-output
	     (call-process "grep" filename '(t nil) nil "^%%\\(HiRes\\)\\?BoundingBox:")))))
    (if (or (string-match   "^%%HiResBoundingBox:\
 +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\)"
			    str)
	    (string-match   "^%%BoundingBox:\
 +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\) +\\([-+]?[0-9.]+\\)"
			    str))
	(list
	 (string-to-number (match-string 1 str))
	 (string-to-number (match-string 2 str))
	 (string-to-number (match-string 3 str))
	 (string-to-number (match-string 4 str))
	 )
      )
    )
)

(defgroup preview nil "Embed Preview images into LaTeX buffers."
  :group 'AUC-TeX)

(defcustom preview-scale (function preview-scale-from-face)
  "*Scale factor for included previews.
This can be either a function to calculate the scale, or
a fixed number." 
  :group 'preview
  :type '(choice (function-item preview-scale-from-face)
		 (const 1.0)
		 (number :value 1.0)
		 (function :value preview-scale-from-face)))

(defun preview-document-pt ()
  "Calculate the default font size of document.
If packages, classes or styles were called with an option
like 10pt, size is taken from the first such option if you
had let your document be parsed by AucTeX.  Otherwise
the value is taken from `preview-default-document-pt'."
  (or (and (boundp 'TeX-auto-file)
	   (catch 'return (dolist (elt TeX-auto-file nil)
			    (if (string-match "\\`\\([0-9]+\\)pt\\'" elt)
				(throw 'return
				       (string-to-number
					(match-string 1 elt)))))) )
      preview-default-document-pt))

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

(defun preview-scale-from-face ()
  "Calculate preview scale from default face.
This calculates the scale of EPS images from a document assumed
to have a default font size given by function `preview-document-pt'
so that they match the current default face in height."
  (/ (face-attribute 'default :height) 10.0 (preview-document-pt)))


(defun preview-ps-image (filename)
  (let* ((filename (expand-file-name filename))
	 (bb (preview-extract-bb filename))
	 (scale (funcall preview-scale))
	 (ptw (- (elt bb 2) (elt bb 0)))
	 (pth (- (elt bb 3) (elt bb 1))))
    (create-image filename 'postscript nil
		  :pt-width (round (* scale ptw))
		  :pt-height (round (* scale pth))
		  :bounding-box bb :ascent (if (and (<= (elt bb 1) 720) (> (elt bb 3) 720)) (round (* 100.0 (/ (- (elt bb 3) 720.0) pth))) 100)
		  :heuristic-mask '(65535 65535 65535)
		  )
    ))

;; (defun preview-ps-image (filename)
;;   (let ((filename (expand-file-name filename))
;; 	(coding-system-for-read 'no-conversion)
;; 	(buf (generate-new-buffer " *JPEG image*")))
;;     (save-excursion
;;       (set-buffer buf)
;;       (set-buffer-multibyte nil)
;;       (unwind-protect
;; 	  (if (eq 0 (call-process "gs" filename (list buf nil)  nil "-q" "-dNOPAUSE" "-dSAFER" "-sDEVICE=jpeg" "-r100x100" "-sOutputFile=-" "-"))
;; 	      (create-image (buffer-string) 'jpeg t))
;; 	(kill-buffer buf)
;; 	)
;;       )
;;     )
;;   )



(setq preview-overlay nil)

(put 'preview-overlay
     'modification-hooks
     '(preview-disable)
     )

(put 'preview-overlay
     'insert-in-front-hooks
     '(preview-disable)
     )

(put 'preview-overlay 'invisible t)

(defun preview-toggle (ov &rest ignored)
     (overlay-put ov 'invisible (not (overlay-get ov 'invisible)))
     (let ((old-string (overlay-get ov 'before-string))
	   (new-string (overlay-get ov 'other-string)))
       (overlay-put ov 'before-string new-string)
       (overlay-put ov 'other-string old-string)))

(put 'preview-overlay 'isearch-open-invisible 'preview-toggle)
(put 'preview-overlay 'isearch-open-invisible-temporary 'preview-toggle)

(defun preview-make-map (ovr)
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] `(lambda () (interactive) (preview-toggle ,ovr)))
    (define-key map [mouse-3] `(lambda () (interactive) (preview-deleter ,ovr)))
    map))

(defun preview-regenerate (ovr)
  (let ((begin (overlay-start ovr))
	(end (overlay-end ovr)))
    (preview-deleter ovr)
    (TeX-region-create (TeX-region-file TeX-default-extension)
		       (buffer-substring begin end)
		       (file-name-nondirectory (buffer-file-name))
		       (count-lines (save-restriction (widen) (point-min))
				    begin)))
  (TeX-command "Graphic Preview" 'TeX-region-file))

(defun preview-disabled-string (ov)
  (propertize "x" 'display preview-icon
	          'local-map (overlay-get ov 'preview-map)
		  'help-echo "mouse-2 regenerates preview
mouse-3 kills preview"))

(defun preview-disable (ovr &rest ignored)
  (define-key (overlay-get ovr 'preview-map) [mouse-2]
    `(lambda () (interactive) (preview-regenerate ,ovr)))
  (overlay-put ovr 'before-string (preview-disabled-string ovr))
  (overlay-put ovr 'invisible nil)
  (let ((filename (overlay-get ovr 'filename)))
    (when filename
      (overlay-put ovr 'filename nil)
      (delete-file filename))))
    
(defun preview-deleter (ovr &rest ignored)
  "Delete preview overlay OVR, taking any associated file along.
   IGNORED arguments are ignored, making this function usable as
   a hook in some cases"
  (let ((filename (overlay-get ovr 'filename)))
    (delete-overlay ovr)
    (if filename (delete-file filename))))

(defun preview-clearout (&optional start end buffer)
  "Clear out all previews in the current region"
  (interactive "r")
  (save-excursion
    (if buffer (set-buffer buffer))
    (mapc '(lambda (ov)
	     (if (eq (overlay-get ov 'category) 'preview-overlay)
		 (preview-deleter ov)
	       ))
	  (overlays-in (or start 1)
		       (or end (1+ (buffer-size)))))))

(add-hook 'kill-buffer-hook 'preview-kill-buffer nil nil)
(defvar preview-temp-dirs nil)
(make-local-variable 'preview-temp-dirs)

(defun preview-kill-buffer ()
  (preview-clearout)
  (condition-case nil
      (mapc 'delete-directory preview-temp-dirs)
    (error nil)) )

(defimage preview-icon ((:type xpm :file "search.xpm")
			(:type pbm :file "search.pbm")))

(defun preview-inactive-string (ov)
  (concat
   (propertize "x" 'display preview-icon
	       'local-map (overlay-get ov 'preview-map)
	       'help-echo "mouse-2 opens preview
mouse-3 kills preview")
   "\n") )

(defun preview-active-string (ov)
  (propertize "x" 'display (preview-ps-image (overlay-get ov 'filename))
	      'local-map (overlay-get ov 'preview-map)
	      'help-echo "mouse-2 opens text
mouse-3 kills preview") )


(defun place-preview (filename source start end)
  (save-excursion
    (set-buffer source)
    (preview-clearout start end)
    (let ((ov (make-overlay start end nil nil nil)))
      (overlay-put ov 'category 'preview-overlay)
      (overlay-put ov 'preview-map (preview-make-map ov))
      (overlay-put ov 'filename filename)
      (overlay-put ov 'before-string
		   (preview-active-string ov))
      (overlay-put ov 'other-string
		   (preview-inactive-string ov)))))

(defun preview-back-command (&optional posn buffer)
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
		
(defun LaTeX-preview-setup ()
  (remove-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
  (require 'tex-buf)
  (setq TeX-command-list
       (append TeX-command-list
	       '(("Graphic Preview"
		  "%l '\\nonstopmode\\PassOptionsToPackage{auctex,active}{preview}\\input{%t}';dvips -Pwww -i -E %d -o %m/eps.000"
		  TeX-inline-preview nil))))
  (setq TeX-error-description-list
       (cons '("Package Preview Error.*" .
"The auctex option to preview should not be applied manually.  If you
see this error message, either you did something too clever, or the
preview Emacs Lisp package something too stupid.") TeX-error-description-list))
  (add-hook 'TeX-translate-location-hook 'preview-translate-location)
  (setq TeX-expand-list
	(append TeX-expand-list
		'(("%m" preview-create-subdirectory)) )))

(defun preview-clean-subdir (dir)
  (condition-case err
      (progn
	(mapc 'delete-file
	      (directory-files dir t "\\`eps\\.[0-9]\\{3,\\}\\'" t))
	(delete-directory dir))
    (error (message "Deletion of %s failed: %s" dir
		    (error-message-string err)))))


(defun preview-clean-topdir (topdir)
  (mapc 'preview-clean-subdir
	(directory-files topdir t "\\`tmp" t) ) )

(defvar TeX-active-tempdir)
(make-variable-buffer-local 'TeX-active-tempdir)

(defun preview-create-subdirectory ()
  (let ((topdir (expand-file-name (TeX-active-master "prv"))))
    (unless (member topdir preview-temp-dirs)
      (if (file-directory-p topdir)
	  (preview-clean-topdir topdir)
	(make-directory topdir))
      (setq preview-temp-dirs (cons topdir preview-temp-dirs)) )
    (setq TeX-active-tempdir
	  (make-temp-file (expand-file-name
			   "tmp" (file-name-as-directory topdir)) t))))

(defun preview-translate-location ()
  (if (string-match "Package Preview Error.*" error)
      (condition-case nil
	  (throw 'preview-error-tag t)
	(no-catch nil))))

(defun preview-parse-TeX (reparse)
  (while
      (catch 'preview-error-tag
	(TeX-parse-TeX reparse)
	nil
	)))

;; Hook into TeX immediately if it's loaded, use LaTeX-mode-hook if not.
(if (featurep 'tex)
    (LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))
      
(defvar preview-last-snippet)
(make-local-variable 'preview-last-snippet)
(defvar preview-last-snippet-point)
(make-local-variable 'preview-last-snippet-point)

(defun preview-parse-messages ()
  (let ((tempdir TeX-active-tempdir))
    (save-excursion
      (set-buffer (TeX-active-buffer))
      (TeX-parse-reset)
      (setq preview-last-snippet 0)
      (setq preview-last-snippet-point nil)
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
      (if (zerop preview-last-snippet)
	  (preview-clean-subdir tempdir)) )))

(defun preview-analyze-error (snippet startflag tempdir)
  "Analyze a preview diagnostic."
  

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
	(unless (eq snippet (1+ preview-last-snippet))
	  (message ("Preview snippet %d out of sequence" snippet)))
      (unless (eq snippet preview-last-snippet)
	(message ("End of Preview snippet %d unexpected" snippet))
	(setq preview-last-snippet-point nil)))
    (setq preview-last-snippet snippet)
    (setq preview-last-snippet-point
	  (let ((last-point preview-last-snippet-point)
		(psfilename (format "%s/eps.%03d" tempdir snippet)))
	    (save-excursion
	      (set-buffer (find-file-noselect file))
	      (goto-line (+ offset line))
	      (if (not (string= string " "))
		  (search-forward string (line-end-position) t))
	      (if startflag
		  (or (preview-back-command) (point))
		(if last-point
		    (progn
		      (place-preview psfilename
				     (current-buffer)
				     last-point
				     (point))
		      nil)
		  (message ("Unexpected end of Preview snippet %d" snippet)))))))))

(defun TeX-inline-sentinel (process name)
  (if process (TeX-format-mode-line process))
  (let (inhibit-quit)
    (save-excursion
      (set-buffer TeX-command-buffer)
      (preview-parse-messages))))
  
;;   (save-excursion
;;     (set-buffer TeX-command-buffer)
;;     (condition-case what-error
;; 	(load-file (TeX-master-file "prv"))
;;       (error (message "When loading %s: %s" (TeX-master-file "prv") (error-message-string what-error))
;; 	     ))))


(defun TeX-inline-preview (name command file)
  (let ((process (TeX-run-format name command file)))
    (setq TeX-sentinel-function 'TeX-inline-sentinel)
    (setq TeX-parse-function 'preview-parse-TeX)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))



(provide 'preview)
;;; preview.el ends here
