;; harvard.el --- Support for Harvard Citation style package for AUC-TeX

;; Copyright (C) 1994 Berwin Turlach <berwin@core.ucl.ac.be>

;; Version: $Id: harvard.el,v 1.2 1994-03-02 14:20:32 amanda Exp $

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
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Harvard citation style is from Peter Williams available on the CTAN
;; servers

;;; Code:

(require 'ltx-env)
(require 'tex-cpl)

(defvar TeX-arg-cite-note-p nil
  "*If non-nil, ask for optional note in citations.")

;; add support for the useful macros of harvard.sty
;;
(TeX-add-style-hook "harvard"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("thebibliography" LaTeX-env-harvardbib ignore)
     )
    (TeX-add-symbols
     '("citeasnoun"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) () )
       TeX-arg-cite)
     '("citeyear"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) () )
       TeX-arg-cite)
     '("citename"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) () )
       TeX-arg-cite)
     '("citationstyle"      TeX-arg-harvard-citationstyle)
     '("bibliographystyle"  TeX-arg-harvard-bibliographystyle ignore)
     '("harvarditem" TeX-arg-harvard-bibitem)
     ))))

(defun TeX-arg-harvard-citationstyle (optional &optional prompt)
  "Prompt for a Harvard citationstyle with completion. (LaTeX)"
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Citationstyle")
		    '(("agsm") ("dcu")))
   optional))

(defun TeX-arg-harvard-bibliographystyle (optional &optional prompt)
  "Prompt for a Harvard bibliographystyle with completion. (LaTeX)"
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Bibliographystyle")
    '(("agsm") ("kluwer") ("dcu")))
   optional))

(defun TeX-arg-harvard-bibitem(optional)
  "Prompt for a Harvarditem. (LaTeX)"
  (let ((< LaTeX-optop)
        (> LaTeX-optcl))
    (TeX-argument-insert
     (read-string "Short citation: ")
     optional))
  (TeX-argument-insert
   (read-string "Complete citation: ")
   optional)
  (TeX-argument-insert
   (read-string "year: ")
   optional)
  (TeX-arg-define-cite
   optional))

;; change LaTeX-item-list, so that LaTeX-item-harvardbib instead of
;; LaTeX-item-bib is called if we insert a "thebibliography"
;; environment.
;;
;; Tip how to get a local copy and pick out the correct item in
;; LaTeX-item-list from:
;;      Daniel Polani <polani@informatik.mathematik.uni-mainz.de
;;
(make-variable-buffer-local 'LaTeX-item-list)
(setq LaTeX-item-list (copy-alist LaTeX-item-list))
(setcdr (assoc '"thebibliography" LaTeX-item-list) 'LaTeX-item-harvardbib)

;; analog to LaTeX-env-bib from ltx-env.el
;; the optional ignore is needed to make sure that
;; LaTeX-env-harvardbib is called instead of LaTeX-env-bib if we
;; insert a "thebibliography" environment.
;;
(defun LaTeX-env-harvardbib (environment &optional ignore)
  "Insert ENVIRONMENT with label for harvarditem."
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    "xx"
				    TeX-grcl))
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

;; analog to LaTeX-item-bib from ltx-env.el
;;
(defun LaTeX-item-harvardbib ()
  "Insert a new harvarditem."
  (TeX-insert-macro "harvarditem"))

;; harvard.el ends here
