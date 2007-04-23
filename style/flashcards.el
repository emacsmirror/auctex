;;; flashcards.el --- AUCTeX style for the flashcards class.

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2007-04-23
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
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for the flashcards class.

;;; Code:

(TeX-add-style-hook
 "flashcards"
 (lambda ()
   (TeX-add-symbols
    '("cardfrontstyle" ["Format"] "Style")
    '("cardfrontfoot" "Footer text")
    '("cardbackstyle" "Style")
    '("cardfrontheadstyle" ["Format"] "Style")
    '("cardfrontfootstyle" ["Format"] "Style")
    "cardmargin"
    "cardpaper"
    "cardpapermode"
    "cardrows"
    "cardcolumns"
    "cardheight"
    "cardwidth")
   (LaTeX-add-environments '("flashcard" ["Header"] "Front side"))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (mapcar (lambda (keyword)
	       (add-to-list 'font-latex-match-variable-keywords-local keyword))
	     '("cardfrontstyle" "cardfrontfoot" "cardbackstyle"
	       "cardfrontheadstyle" "cardfrontfootstyle"))
     (font-latex-match-variable-make)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

;;; flashcards.el ends here
