;;; tex-mik.el --- MikTeX support for AUCTeX.

;; Copyright (C) 1999, 2000, 2001 Per Abrahamsen
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: auc-tex@sunsite.dk
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;; This file contains variables customized for MikTeX.

;;; Code:

(unless (get 'TeX-command-list 'saved-value)
  (setq TeX-command-list
	(list (list "TeX" "tex %S \\nonstopmode\\input %t" 'TeX-run-TeX nil
		    (list 'plain-tex-mode))
	      (list "LaTeX" "%l \\nonstopmode\\input{%t}" 'TeX-run-TeX nil
		    (list 'latex-mode))
	      (list "PDFLaTeX" "pdflatex %S \\nonstopmode\\input{%t}"
		    'TeX-run-TeX nil (list 'latex-mode))
	      (list "View" "%v" 'TeX-run-discard nil t)
	      (list "Print" "gsview32 %f" 'TeX-run-command t t)
	      (list "File" "dvips %d -o %f " 'TeX-run-command t t)
	      (list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil t)
	      (list "Index" "makeindex %s" 'TeX-run-command nil t)
	      (list "Check" "lacheck %s" 'TeX-run-compile nil t)
	      (list "Other" "" 'TeX-run-command t t))))

(unless (get 'TeX-view-style 'saved-value)
  (setq TeX-view-style '(("^epsf$" "gsview32 %f")
			 ("." "yap -1 %dS %d"))))

(unless (get 'TeX-output-view-style 'saved-value)
  (setq TeX-output-view-style
	'(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && gsview %f")
	  ("^dvi$" "." "yap -1 %dS %d")
	  ("^pdf$" "." "AcroRd32 %o")
	  ("^html?$" "." "mozilla %o"))))

(unless (get 'TeX-source-specials-viewer-flags 'saved-value)
  (setq TeX-source-specials-viewer-flags "-s %n%b"))

(provide 'tex-mik)

;;; tex-mik.el ends here
