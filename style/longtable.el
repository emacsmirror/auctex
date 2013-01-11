;;; longtable.el --- AUCTeX style for `longtable.sty'.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `longtable.sty'.

;;; Code:

(add-to-list 'LaTeX-label-alist
	     '("longtable" . LaTeX-table-label))

(TeX-add-style-hook
 "longtable"
 (lambda ()
   (LaTeX-add-environments
    '("longtable" (lambda (environment)
		    (let ((pos (completing-read (TeX-argument-prompt t nil "Position")
						'(("l") ("r") ("c"))))
			  (fmt (read-string "Format: " LaTeX-default-format))
			  (caption (read-string "Caption: ")))
		      (setq LaTeX-default-format fmt)
		      (LaTeX-insert-environment environment
						(concat
						 (unless (zerop (length pos))
						   (concat LaTeX-optop pos LaTeX-optcl))
						 (concat TeX-grop fmt TeX-grcl)))
		      ;; top caption -- do nothing if user skips caption
		      (unless (zerop (length caption))
			;; the longtable `\caption' is equivalent to a
			;; `\multicolumn', so it needs a `\\' at the
			;; end of the line
			(insert TeX-esc "caption" TeX-grop caption TeX-grcl " \\\\")
			(LaTeX-newline)
			(indent-according-to-mode)
			;; ask for a label and insert a new line only
			;; if a label is actually inserted
			(when (LaTeX-label environment)
			  (LaTeX-newline)
			  (indent-according-to-mode)))))))
   (TeX-add-symbols
    ;; Parameters
    '("LTleft" 0)
    '("LTright" 0)
    '("LTpre" 0)
    '("LTpost" 0)
    '("LTcapwidth" 0)
    '("LTchunksize" 0)
    ;; Commands to end table rows
    '("endhead" 0)
    '("endfirsthead" 0)
    '("endfoot" 0)
    '("endlastfoot" 0)
    ;; Caption commands
    '("caption*" 1))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     ;; Actually, `\caption*{}' macro takes only one mandatory
     ;; argument, not an optional one, the following is a workaround
     ;; to fontify correctly also the standard `\caption[]{}' macro.
     (font-latex-add-keywords '(("caption" "*[{"))
			      'textual))))

;; longtable.el ends here
