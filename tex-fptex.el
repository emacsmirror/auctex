;;; tex-fptex.el --- fpTeX support for AUCTeX.

;; Copyright (C) 2000 Fabrice Popineau
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Fabrice Popineau <Fabrice.Popineau@supelec.fr>
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
;; This file contains variables customized for fpTeX.
;; Borrowed from tex-mik.el .

;;; Code:

(defmacro parent-directory (f)
  "Return safe parent directory of the directory given as argument."
  `(directory-file-name
    (file-name-directory
     (directory-file-name ,f))))

(setq TeX-lisp-directory 
      (concat (parent-directory (invocation-directory))
	      "/site-lisp/auctex"))

;; The fpTeX commands.
(setq TeX-command-list
  (list (list "TeX" "tex %S \\nonstopmode\\input %t" 'TeX-run-TeX nil
              (list 'plain-tex-mode))
	(list "LaTeX" "%l \\nonstopmode\\input{%t}" 'TeX-run-TeX nil
              (list 'latex-mode))
	(list "PDFLaTeX" "pdflatex %S \\nonstopmode\\input{%t}"
	      'TeX-run-TeX nil (list 'latex-mode))
	(list "AmSTeX" "amstex %S \\nonstopmode\\input{%t}" 'TeX-run-TeX nil
              (list 'ams-tex-mode))
	(list "View" "%v" 'TeX-run-command t t)
	(list "View PS" "gsview32 %f" 'TeX-run-command t t)
	(list "View PDF" "start %t.pdf" 'TeX-run-command t t)
	(list "Print" "dvips %d" 'TeX-run-command t t)
	(list "File" "dvips %d -o %f " 'TeX-run-command t t)
	(list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil t)
	(list "Index" "makeindex %s" 'TeX-run-command nil t)
	(list "Check" "lacheck %s" 'TeX-run-compile nil t)
	(list "Spell" "<ignored>" 'TeX-run-ispell-on-document nil t)
	(list "Makeinfo" "makeinfo %t" 'TeX-run-compile nil t)
	(list "Other" "" 'TeX-run-command t t)))

(setq TeX-view-style '(("^a5$" "windvi %d -paper a5")
		       ("^landscape$" "windvi %d -paper a4r -s 4")
		       ("^epsf$" "gsview32 %f")
		       ("." "windvi -single %d")))

(provide 'tex-fptex)

;;; tex-fptex.el ends here
