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

(require 'tex-site)

(unless (get 'TeX-command-list 'saved-value)
  ;; Remove the Queue entry from the default, and make a non-Unix
  ;; specific print entry, assuming that dvips will print by default.
  ;; Actually, stuff like that should rather be done by fiddling with
  ;; the respective TeX-printer* variables, but the code is pretty
  ;; opaque.
  (setq TeX-command-list
	(delq (assoc "Queue" TeX-command-list)
	      TeX-command-list))
  (when (assoc "Print" TeX-command-list)
       (setcar (cdr (assoc "Print" TeX-command-list))
	       "gsview32 %f")))

(unless (get 'TeX-view-style 'saved-value)
  (setq TeX-view-style '(("^epsf$" "gsview32 %f")
			 ("." "yap -1 %dS %d"))))

(unless (get 'TeX-output-view-style 'saved-value)
  (setq TeX-output-view-style
	'(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && gsview32 %f")
	  ("^dvi$" "." "yap -1 %dS %d")
	  ("^pdf$" "." "AcroRd32 %o") ; Use "start %o" instead?
	  ("^html?$" "." "mozilla %o"))))

(unless (get 'TeX-source-specials-viewer-flags 'saved-value)
  (setq TeX-source-specials-viewer-flags "-s %n%b"))

(provide 'tex-mik)

;;; tex-mik.el ends here
