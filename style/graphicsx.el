;;; -*- emacs-lisp -*-
;;; graphicsx.el - Support for the graphicx style option.

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: Ryuichi Arafune <arafune@ushioda.riec.tohoku.ac.jp>
;; Created: 1999/3/20
;; Version: $Id: graphicsx.el,v 1.1 2000-04-25 11:07:30 abraham Exp $
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
;;  This package suppors the includegraphcics macro in graphicx style (LaTeX2e)
;;  If you want to use bb, angle or totalheight as arguments of includegraphics,
;;  set TeX-include-graphics-simple nil (default t).

;;; Code:

(TeX-add-style-hook 
 "graphicx"
 (function (lambda () 
	     (TeX-add-symbols
	      "protect" "clip" "keepaspectratio" "width" "height" "bb" "angle" "totalheight"
	      '("includegraphics" TeX-arg-includegraphics)))))


(defvar TeX-include-graphics-simple t 
"if nil, AUC TeX asks the following arguments: Bounding box (bb), Rotation angle (angle), Total height (totalheight) in addition to normal arguments.")
 
(defun TeX-make-eps-completion-alist ()
  "Make a alist of eps files in the current directory"
  (let ( TeX-eps-completion-alist
	 (files-currentdirectory (directory-files "./" nil  "\\.eps$" nil )))
    (while files-currentdirectory
      (setq TeX-eps-completion-alist
	    (cons (list (car files-currentdirectory)) TeX-eps-completion-alist))
      (setq files-currentdirectory (cdr files-currentdirectory)))
    TeX-eps-completion-alist))

(defun TeX-arg-includegraphics (optional)
  "Ask for file name (eps file only), width, height, keepaspectratio, and crip. Insert includegraphics macro"
  (let ((width-flag nil) (height-flag nil) (left-brace-flag nil) 
	(psfile  (completing-read "PS (eps only) file: " (TeX-make-eps-completion-alist)
				  nil t (car (car (TeX-make-eps-completion-alist)))))
	(figwidth (read-input "Figure width (cm): "))
	(figheight (read-input "Figure height (cm): "))
	(keepaspectratio (y-or-n-p "Keep Aspectratio ? "))
	(crip (y-or-n-p "Cripping figure ? ")))
    (if (or (not (zerop (length figwidth))) (not (zerop (length figheight))) keepaspectratio crip)
	(progn (insert "[") (setq left-brace-flag t)))
    (if (not (zerop (length figwidth)))
	(progn (insert "width="figwidth"cm")
	       (setq width-flag t)))
    (if (and (not (zerop (length figheight))) width-flag)
	(progn (insert ",height="figheight"cm")
	       (setq height-flag t))
      (if (not (zerop (length figheight)))
	  (progn (insert "height="figheight"cm")
		 (setq height-flag t))))
    (if (not (and height-flag width-flag))
	(if (and keepaspectratio (or width-flag height-flag))
	    (insert ",keepaspectratio")
	  (if  keepaspectratio
	      (insert "keepaspectratio"))))
    (if (and crip (or width-flag height-flag keepaspectratio))
	(insert ",crip")
      (if crip 
	  (insert "crip")))
;;; Insert more arguments when TeX-include-graphics-simple is nil
    (if (not TeX-include-graphics-simple)
	(progn
	  (let ((bbset-flag (y-or-n-p "Set Bounding Box :"))
		(bbllx nil) (bblly nil) (bburx nil) (bbury nil)	(angle-flag nil) 
		(angle "") (totalheight ""))
	    (if bbset-flag
		(progn (setq  bbllx (read-input "Bounding Box Lower Left x :"))
		       (setq  bblly (read-input "Bounding Box Lower Left y :"))
		       (setq  bburx (read-input "Bounding Upper right x :"))
		       (setq  bbury (read-input "Bounding Upper right y :"))))
	    (if (and bbset-flag (or crip keepaspectratio width-flag height-flag))
		(insert ",bb="bbllx bblly bburx bbury)
	      (if bbset-flag
		  (inert "bb="bbllx bblly bburx bbury)))
	    (setq angle (read-input "Rotation Angle :"))
	    (if (not (zerop (length angle)))
		(progn (setq angle-flag t)
		       (if (or crip keepaspectratio width-flag height-flag bbset-flag)
			   (insert ",angle="angle)
			 (if angle-flag
			     (insert "angle="angle)))))
	    (setq totalheight (read-input "Total Height (cm):"))
	    (if (not (zerop (length totalheight)))
		(if (or crip keepaspectratio width-flag height-flag bbset-flag angle-flag)
		    (insert ",totalheight="totalheight)
		  (if (not zerop (length totalheight))
		      (insert "totalheight="totalheight)))))))
;;;
    (if left-brace-flag
	(insert "]"))
    (TeX-insert-braces 0)
    (insert psfile)
    ))

;;; graphicx.el ends here
