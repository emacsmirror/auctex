;;; graphicx.el --- AUCTeX style file for graphicx.sty

;; Copyright (C) 2000, 2004 by Free Software Foundation, Inc.

;; Author: Ryuichi Arafune <arafune@debian.org>
;; Created: 1999/3/20
;; Keywords: tex

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

;;  This package supports the includegraphcics macro in graphicx style.

;; Acknowledgements
;;  Dr. Thomas Baumann <thomas.baumann@ch.tum.de>
;;  David Kastrup <David.Kastrup@t-online.de>
;;  Masayuki Akata <ataka@milk.freemail.ne.jp>

;;; Code:

(TeX-add-style-hook
 "graphicx"
 (function (lambda ()
	     (TeX-add-symbols
	      ;; Why do we need those?
	      "protect" "clip" "keepaspectratio"
	      "width" "height" "bb" "angle" "totalheight"
	      '("includegraphics" LaTeX-arg-includegraphics)))))

(defun LaTeX-includegraphics-extensions (&optional list)
  "Return appropriate extensions for input files to \\includegraphics."
  ;; FIXME: This function may check for latex/pdflatex later.
  (concat "\\."
	  (mapconcat 'identity
		     (or list LaTeX-includegraphics-extensions)
		     "$\\|\\.")
	  "$"))

(defun LaTeX-includegraphics-read-file ()
  "Read image file for \\includegraphics."
  ;; Drop latex/pdflatex differences for now.  Might be (re-)included later.
  (completing-read
   "Image file: "
   (mapcar 'list
	   (TeX-search-files nil LaTeX-includegraphics-extensions t t))
   nil nil nil))

(defun LaTeX-arg-includegraphics (prefix)
  "Ask for mandantory and optional arguments for the \\includegraphics command.

The extent of the optional arguments is determined by the prefix argument and
`LaTeX-includegraphics-options-alist'."
  (let* ((maybe-left-brace "[")
	 (maybe-comma "")
	 show-hint
	 (image-file (LaTeX-includegraphics-read-file))
	 (incl-opts
	  (cond
	   ((numberp
	     (if (listp current-prefix-arg)
		 (setq current-prefix-arg (car current-prefix-arg))
	       current-prefix-arg))
	    (cdr
	     (assq current-prefix-arg LaTeX-includegraphics-options-alist)))
	   ;; If no prefix is given, use `0' and tell the user about the
	   ;; prefix.
	   ((eq current-prefix-arg nil)
	    (setq show-hint t)
	    (cdr (assq 0 LaTeX-includegraphics-options-alist)))
	   (t
	    (cdr (assq 0 LaTeX-includegraphics-options-alist)))))
	 ;; Options from Table 1:
	 (totalheight
	  (TeX-arg-maybe
	   'totalheight incl-opts
	   '(read-input
	     (concat "Total Height (" TeX-default-unit-for-image "): "))))
	 (height
	  (TeX-arg-maybe
	   'height incl-opts
	   ;; Either totalheight or height make sense:
	   '(when (zerop (length totalheight))
	      (read-input
	       (concat "Figure height (" TeX-default-unit-for-image "): ")))))
	 (width
	  (TeX-arg-maybe
	   'width incl-opts
	   '(read-input
	     (concat "Figure width (" TeX-default-unit-for-image "): "))))
	 (scale
	  (TeX-arg-maybe
	   'angle incl-opts
	   ;; If size is already specified, don't ask for scale:
	   '(when (zerop (+ (length totalheight)
			    (length height)
			    (length width)))
	      (read-input "Scale: "))))
	 (angle
	  (TeX-arg-maybe
	   'angle incl-opts
	   '(read-input "Rotation angle: ")))
	 (origin
	  (TeX-arg-maybe
	   'origin incl-opts
	   '(read-input
	     (concat
	      "Origin (any combination of `lcr' (horizontal) "
	      "and `tcbB' (vertical)): "))))
	 (bb
	  (TeX-arg-maybe
	   'bb incl-opts
	   '(y-or-n-p "Set Bounding Box? ")))
	 ;; Table 2:
	 (viewport
	  (TeX-arg-maybe
	   'viewport incl-opts
	   '(y-or-n-p "Set viewport? ")))
	 (trim
	  (TeX-arg-maybe
	   'trim incl-opts
	   '(and (not viewport)
		 (y-or-n-p "Set trim? "))))
	 ;; Table 2:
	 (clip
	  (TeX-arg-maybe
	   'clip incl-opts
	   ;; If viewport, we also use clip.
	   '(or viewport
		(y-or-n-p "Clipping figure? "))))
	 (keepaspectratio
	  (TeX-arg-maybe
	   'keepaspectratio incl-opts
	   ;; If we have width and [total]height...
	   '(or (and (not (zerop (length width)))
		     (or (not (zerop (length totalheight)))
			 (not (zerop (length height)))))
		(y-or-n-p "Keep Aspectratio? "))))
	 ;; Used for bb, trim, viewport, ...:
	 llx lly urx ury)
    ;; Now insert stuff...
    (when (not (zerop (length totalheight)))
      (insert
       maybe-left-brace maybe-comma "totalheight="
       (car (TeX-string-divide-number-unit totalheight))
       (if (zerop
	    (length
	     (car (cdr (TeX-string-divide-number-unit totalheight)))))
	   TeX-default-unit-for-image
	 (car (cdr (TeX-string-divide-number-unit totalheight)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length height)))
      (insert maybe-left-brace maybe-comma
	      "height=" (car (TeX-string-divide-number-unit height))
	      (if (zerop
		   (length
		    (car (cdr (TeX-string-divide-number-unit height)))))
		  TeX-default-unit-for-image
		(car (cdr (TeX-string-divide-number-unit height)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length width)))
      (insert maybe-left-brace maybe-comma
	      "width=" (car (TeX-string-divide-number-unit width))
	      (if (zerop
		   (length
		    (car (cdr (TeX-string-divide-number-unit width)))))
		  TeX-default-unit-for-image
		(car (cdr (TeX-string-divide-number-unit width)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length scale)))
      (insert maybe-left-brace maybe-comma "scale=" scale)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length angle)))
      (insert maybe-left-brace maybe-comma "angle=" angle)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length origin)))
      (insert maybe-left-brace maybe-comma "origin=" origin)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when bb
      (setq llx (read-input "Bounding Box lower left x: "))
      (setq lly (read-input "Bounding Box lower left y: "))
      (setq urx (read-input "Bounding Box upper right x: "))
      (setq ury (read-input "Bounding Box upper right y: "))
      (insert maybe-left-brace maybe-comma
	      "bb=" llx " " lly " " urx " " ury)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    ;;
    (when viewport
      (setq llx (read-input "Viewport lower left x: "))
      (setq lly (read-input "Viewport lower left y: "))
      (setq urx (read-input "Viewport upper right x: "))
      (setq ury (read-input "Viewport upper right y: "))
      (insert maybe-left-brace maybe-comma
	      "viewport=" llx " " lly " " urx " " ury)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when trim
      (setq llx (read-input "Trim lower left x: "))
      (setq lly (read-input "Trim lower left y: "))
      (setq urx (read-input "Trim Upper right x: "))
      (setq ury (read-input "Trim Upper right y: "))
      (insert maybe-left-brace maybe-comma
	      "trim=" llx " " lly " " urx " " ury)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    ;;
    (when clip
      (insert maybe-left-brace maybe-comma "clip")
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when keepaspectratio
      (insert maybe-left-brace maybe-comma "keepaspectratio")
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    ;;
    (if (zerop (length maybe-left-brace))
	(insert "]"))
    (TeX-insert-braces 0)
    (insert
     (if LaTeX-includegraphics-strip-extension-flag
	 ;; We don't have `replace-regexp-in-string' in all (X)Emacs versions:
	 (with-temp-buffer
	   (insert image-file)
	   (goto-char (point-max))
	   (when (search-backward-regexp (LaTeX-includegraphics-extensions)
					 nil t 1)
	     (replace-match ""))
	   (buffer-string))
       image-file))
    (when show-hint
      (message
       (concat
	"Adding `C-u C-u' before the command asks for more optional arguments."
	"\nSee `LaTeX-includegraphics-options-alist' for details."))
      (sit-for 3))
    t))

;;; graphicx.el ends here
