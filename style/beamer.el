;;; beamer.el --- AUCTeX style for the latex-beamer class

;; Copyright (C) 2003, 2004, 2005 Free Software Foundation
;; License: GPL, see the file COPYING in the base directory of AUCTeX

;; Author: Thomas Baumann <thomas.baumann@ch.tum.de>
;; Created: 2003-12-20
;; Version: $Id: beamer.el,v 1.4 2005-03-03 09:05:38 angeli Exp $
;; Keywords: tex

;;; Commentary:

;; This file adds support for the latex-beamer class.
;;
;; This file is intended to be used with the AUCTeX-Package.
;; Put this File into your TeX-style-path. You may also
;; byte-compile this file.

;;; Code:

(defvar LaTeX-beamer-section-labels-flag nil
  "If non-nil section labels are added")

(defcustom LaTeX-beamer-item-overlay-flag t
  "If non-nil do prompt for an overlay in itemize-like environments."
  :type 'boolean
  :group 'LaTeX-macro)

(TeX-add-style-hook
 "beamer"
 (lambda ()
   (unless LaTeX-beamer-section-labels-flag
     (make-local-variable 'LaTeX-section-hook)
     (setq LaTeX-section-hook
	   '(LaTeX-section-heading
	     LaTeX-section-title
	     LaTeX-section-section)))

   (setq LaTeX-item-list
	 (append '(("itemize" . LaTeX-item-beamer)
		   ("enumerate" . LaTeX-item-beamer))
		 LaTeX-item-list))

   (TeX-add-symbols
    '("alert" 1)
    '("alt" TeX-arg-beamer-overlay-spec 2)
    '("beamerbutton" 1)
    '("beamergotobutton" 1)
    '("beamerreturnbutton" 1)
    '("beamerskipbutton" 1)
    '("frame" TeX-arg-beamer-frametitle)
    '("frametitle" 1)
    '("hyperlink" TeX-arg-beamer-overlay-spec 2)
    '("hyperlinkslideprev" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkslidenext" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkframestart" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkframeend" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkframestartnext" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkframeendprev" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkpresentationstart" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkpresentationend" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkappendixstart" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkappendixend" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkdocumentstart" TeX-arg-beamer-overlay-spec 1)
    '("hyperlinkdocumentend" TeX-arg-beamer-overlay-spec 1)
    '("hypertarget" TeX-arg-beamer-overlay-spec 2)
    '("institute" 1)
    '("invisible" TeX-arg-beamer-overlay-spec 1)
    '("label" TeX-arg-beamer-overlay-spec 1)
    '("logo" 1)
    '("note" TeX-arg-beamer-note 1)
    '("only" TeX-arg-beamer-overlay-spec 1)
    '("onslide" TeX-arg-beamer-overlay-spec)
    '("partpage")
    '("pause")
    '("structure" TeX-arg-beamer-overlay-spec 1)
    '("temporal" TeX-arg-beamer-overlay-spec 3)
    '("titlepage")
    '("titlegraphic" 1)
    '("uncover" TeX-arg-beamer-overlay-spec 1)
    '("visible" TeX-arg-beamer-overlay-spec 1))
    
   (LaTeX-add-environments
    '("actionenv")
    '("alertblock" 1)
    '("beamerboxesrounded" 1)
    '("block" 1)
    '("column" "Width")
    "columns"
    "columnsonlytextwidth"
    '("exampleblock" 1)
    '("frame"  (lambda (env &rest ignore)
		 (let ((start (if (and (TeX-active-mark) (< (mark) (point)))
				  (mark)
				(point)))
		       (title (read-input "(Optional) Title: ")))
		   (LaTeX-insert-environment env)
		   (unless (zerop (length title))
		     (save-excursion
		       (goto-char start)
		       (end-of-line)
		       (LaTeX-newline)
		       (insert (format "\\frametitle{%s}" title))
		       (save-restriction
			 (narrow-to-region (point-min)
					   (line-beginning-position 2))
			 (LaTeX-fill-region (line-beginning-position)
					    (line-beginning-position 2))))))))
    '("onlyenv" (lambda (env &rest ignore)
		  (LaTeX-insert-environment
		   env
		   (let ((overlay (read-input "(Optional) Overlay: ")))
		     (unless (zerop (length overlay))
		       (format "<%s>" overlay))))))
    '("overlayarea" "Area width" "Area height")
    '("overprint"  (lambda (env &rest ignore)
		     (LaTeX-insert-environment
		      env
		      (let ((width (read-input "(Optional) Area width: ")))
			(unless (zerop (length width))
			  (format "[%s]" width)))))))))

(defun TeX-arg-beamer-overlay-spec (optional &optional prompt)
  "Prompt for overlay specification." 
  (let ((overlay (read-input "(Optional) Overlay: ")))
    (unless (zerop (length overlay))
      (insert "<" overlay ">"))
    (indent-according-to-mode)))

(defun TeX-arg-beamer-frametitle (optional &optional prompt)
  "Prompt for the frametitle."
  (let ((title (read-input "Title: ")))
    (if (not (zerop (length title)))
        (insert TeX-grop TeX-esc "frametitle" TeX-grop 
		title TeX-grcl TeX-grcl)
      (insert TeX-grop TeX-grcl))))

(defun LaTeX-item-beamer ()
  "Insert a new item with an optional overlay argument. You 
can turn off the prompt for the overlay argument by setting 
`LaTeX-beamer-item-overlay-flag' to nil. Calling the function
with a prefix argument prompts for the overlay specification
unconditionally."
  (if (listp current-prefix-arg)
      (setq current-prefix-arg (car current-prefix-arg))
    current-prefix-arg)
  (TeX-insert-macro "item")
  (delete-horizontal-space)
  (if (or current-prefix-arg LaTeX-beamer-item-overlay-flag)
      (TeX-arg-beamer-overlay-spec 0))
  (insert " ")
  (indent-according-to-mode))
  
(defun TeX-arg-beamer-note (optional &optional prompt)
  "Prompt for overlay specification and optional argument."
  (let ((overlay (read-input "(Optional) Overlay: "))
        (options (read-input "(Optional) Options: ")))
    (unless (zerop (length overlay))
      (insert "<" overlay ">"))
    (unless (zerop (length options))
      (insert "[" options "]"))
    (indent-according-to-mode)))
