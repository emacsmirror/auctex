;;; jurabib.el --- AUCTeX style for the `jurabib' package

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auc-tex@sunsite.dk
;; Created: 2004-10-05
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

;; This file adds support for the `jurabib' package.

;; Currently only the citation-related commands are supported.  Feel
;; free to complete the support and send the result to the AUCTeX
;; mailing list.  But be aware that the code can only be included if
;; you assign the copyright to the FSF.

;;; Code:

(TeX-add-style-hook
 "jurabib"
 (lambda ()
   ;; Taken from natbib.el and adapted.
   (let ((citecmds
	  '(("cite" . 2) ("cite*" . 2)
	    ("citetitle" . 2) ("fullcite" . 2)
	    ("citet" . 1) ("citealt" . 1)
	    ("citep" . 2) ("citealp" . 2)
	    ("citeauthor" . 2) ("citeyear" . 2)
	    ("footcite" . 2) ("footcite*" . 2)
	    ("footcitetitle" . 2) ("footfullcite" . 2)
	    ("footcitet" . 1) ("footcitealt" . 1)
	    ("footcitep" . 2) ("footcitealp" . 2)
	    ("footciteauthor" . 0) ("footciteauthor" . 2))))
     ;; Add these symbols
     (apply 
      'TeX-add-symbols
      (mapcar
       (lambda (cmd)
	 (cond 
	  ((= (cdr cmd) 0)
	   ;; No optional arguments
	   (list (car cmd) 'TeX-arg-cite))
	  ((= (cdr cmd) 1)
	   ;; Just one optional argument, the post note
	   (list
	    (car cmd)
	    '(TeX-arg-conditional TeX-arg-cite-note-p (["Post-note"]) nil)
	    'TeX-arg-cite))
	  ((= (cdr cmd) 2)
	   ;; Pre and post notes
	   (list
	    (car cmd)
	    '(TeX-arg-conditional TeX-arg-cite-note-p (natbib-note-args) nil)
	    'TeX-arg-cite))))
       citecmds))
     ;; Special cases
     (TeX-add-symbols
      ;; FIXME: Completing read for field.
      '("citefield" ; \citefield[]{}{}
	(TeX-arg-conditional TeX-arg-cite-note-p (["Post-note"]) nil)
	"Field" TeX-arg-cite)
      '("footcitefield" ; \footcitefield[]{}{}
	(TeX-arg-conditional TeX-arg-cite-note-p (["Post-note"]) nil)
	"Field" TeX-arg-cite))

     ;; Make an entry in TeX-complete-list
     (add-to-list
      'TeX-complete-list
      (list
       (concat "\\\\\\(" 
	       (mapconcat (lambda (x) (regexp-quote (car x)))
			  (append citecmds
				  '(("citefield") ("footcitefield"))) "\\|")
	       "\\)\\(\\[[^]\n\r\\%]*\\]\\)*{\\([^{}\n\r\\%,]*,\\)*"
	       "\\([^{}\n\r\\%,]*\\)")
       4 'LaTeX-bibitem-list "}"))

     ;; Add further symbols
     (TeX-add-symbols
      '("citenotitlefortype" 1)
      '("citeswithoutentry" 1)
      '("citetitlefortype" 1)
      '("nextcitefull" 1)
      '("nextcitenotitle" 1)
      '("nextcitereset" 1)
      '("nextciteshort" 1)
      '("jurabibsetup" 1))

     ;; Fontification
     (when (and (featurep 'font-latex)
		(eq TeX-install-font-lock 'font-latex-setup))
       (mapcar (lambda (item)
		 (add-to-list 'font-latex-match-reference-keywords-local
			      (car item)))
	       citecmds)
       ;; FIXME: Second argument does not get fontified.
       (add-to-list 'font-latex-match-reference-keywords-local "citefield")
       (add-to-list 'font-latex-match-reference-keywords-local "footcitefield")
       (font-latex-match-reference-make)
       (mapcar (lambda (keyword)
		 (add-to-list 'font-latex-match-function-keywords-local
			      keyword))
	       '("citeswithoutentry" "nextcitefull" "nextcitenotitle"
		 "nextcitereset" "nextciteshort"))
       (font-latex-match-function-make)
       (mapcar (lambda (keyword)
		 (add-to-list 'font-latex-match-variable-keywords-local
			      keyword))
	       '("citenotitlefortype" "citetitlefortype" "jurabibsetup"))
       (font-latex-match-variable-make))

     ;; Tell RefTeX (Thanks, Carsten)
     (when (and (fboundp 'reftex-set-cite-format)
		;; Is it `reftex-cite-format' customized?
		(not (get 'reftex-cite-format 'saved-value)))
       ;; Check if RefTeX supports jurabib.
       (if (assoc 'jurabib reftex-cite-format-builtin)
	   ;; Yes, use the provided default.
	   (reftex-set-cite-format 'jurabib)
	 ;; No, set it by hand.
	 (reftex-set-cite-format
	  '((?\C-m . "\\cite{%l}")
	    (?c    . "\\cite[?][]{%l}")
	    (?t    . "\\citet{%l}")
	    (?p    . "\\citep{%l}")
	    (?e    . "\\citep[e.g.][?]{%l}")
	    (?s    . "\\citep[see][?]{%l}")
	    (?u    . "\\fullcite{%l}")
	    (?i    . "\\citetitle{%l}")
	    (?a    . "\\citeauthor{%l}")
	    (?e    . "\\citefield{?}{%l}")
	    (?y    . "\\citeyear{%l}")
	    (?f    . "\\footcite{%l}")
	    (?F    . "\\footcite[?][]{%l}")
	    (?l    . "\\footfullcite{%l}"))))))))

;;; jurabib.el ends here
