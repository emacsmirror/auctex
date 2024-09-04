;;; danish.el --- Setup AUCTeX for editing Danish text.  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-quotes
                  "font-latex"
                  (quotes))

(TeX-add-style-hook
 "danish"
 (lambda ()
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language `("danish" "\"`" "\"'" ,TeX-quote-after-quote)))
   (setq LaTeX-babel-hyphen-language "danish")
   ;; Fontification of quotation marks.
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\">" "\"<" german)))
   (run-hooks 'TeX-language-dk-hook))
 TeX-dialect)

;;; danish.el ends here
