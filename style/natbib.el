;; Support for the natbib package for AUC-TeX
;;      The natbib package is from Patrick W. Daly
;;      available on the CTAN servers

;; Version: $Id: natbib.el,v 1.1 1997-07-14 16:49:20 abraham Exp $

;; Copyright (C) 1997 Berwin Turlach <berwin.turlach@anu.edu.au>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA


;;; Code:

(require 'latex)

(TeX-add-style-hook "natbib"
 (function
  (lambda ()

    (TeX-add-symbols
     '("citet"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) nil)
       TeX-arg-cite)
     '("citet*"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) nil)
       TeX-arg-cite)
     '("citealt"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) nil)
       TeX-arg-cite)
     '("citealt*"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) nil)
       TeX-arg-cite)
     '("citep"
       natbib-arg-cite)
     '("citep*"
       natbib-arg-cite)
     '("citeauthor"
       TeX-arg-cite)
     '("citeyear"
       TeX-arg-cite)
     '("citefullauthor"
       TeX-arg-cite)
     )

    (setq TeX-complete-list
	  (append '(("\\\\citet\\(\\[[^]\n\r\\%]*\\]\\)+{\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list "}")
                    ("\\\\citet{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citet{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list)
                    ("\\\\citet\\*\\(\\[[^]\n\r\\%]*\\]\\)+{\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list "}")
                    ("\\\\citet\\*{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citet\\*{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list)
                    ("\\\\citealt\\(\\[[^]\n\r\\%]*\\]\\)+{\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list "}")
                    ("\\\\citealt{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citealt{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list)
                    ("\\\\citealt\\*\\(\\[[^]\n\r\\%]*\\]\\)+{\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list "}")
                    ("\\\\citealt\\*{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citealt\\*{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list)
                    ("\\\\citep\\(\\[[^]\\%]*\\]\\)+{\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list "}")
                    ("\\\\citep\\(\\[[^]\\%]*\\]\\)+{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
                     3 LaTeX-bibitem-list "}")
                    ("\\\\citep{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citep{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list)
                    ("\\\\citep\\*\\(\\[[^]\n\r\\%]*\\]\\)+{\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list "}")
                    ("\\\\citep\\*{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citep\\*{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
                     2 LaTeX-bibitem-list)
                    ("\\\\citeyear{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citeauthor{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}") 
                    ("\\\\citefullauthor{\\([^{}\n\r\\%,]*\\)" 1
                     LaTeX-bibitem-list "}"))
		  TeX-complete-list))
    )))

(defun natbib-arg-cite (optional &optional prompt definition)
  "Prompt for a BibTeX citation with completion for natbib style."
  (let ((< "[")
        (> "]"))
    (if TeX-arg-cite-note-p
        (progn
          (let ((note (read-string
                       (TeX-argument-prompt optional prompt "Pre-note"))))
            (if (string-equal note "")
                (natbib-argument-insert (read-string
                                         (TeX-argument-prompt optional prompt "Post-note" )))
              (natbib-argument-insert note)
              (natbib-argument-insert (read-string
                                       (TeX-argument-prompt optional prompt "Post-note" ))))))
      nil))
  (setq prompt (concat (if optional "(Optional) " "")
                       (if prompt prompt "Add key")
                       ": (default none) "))
  (let ((items (multi-prompt "," t prompt (LaTeX-bibitem-list))))
    (apply 'LaTeX-add-bibitems items)
    (TeX-argument-insert (mapconcat 'identity items ",") optional optional))
  )

(defun natbib-argument-insert (name)
  "Insert NAME surrounded by square brackets."
  (insert <)
  (insert name)
  (insert >))

;; natbib.el ends here
