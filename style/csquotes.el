;;; csquotes.el --- AUCTeX style for `csquotes.sty'

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auc-tex@sunsite.dk
;; Created: 2004-11-29
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

;; This file adds support for `csquotes.sty'.

;;; Code:

(defcustom LaTeX-csquotes-quote-after-quote nil
  "Initial value of `TeX-quote-after-quote' for `csquotes.el'"
  :type 'boolean)

(defcustom LaTeX-csquotes-open-quote ""
  "Opening quotation mark to be used with the csquotes package.
The specified string will be used for `TeX-open-quote' (and override
the value of `LaTeX-german-open-quote' if the german or ngerman
package is used) only if both `LaTeX-csquotes-open-quote' and
`LaTeX-csquotes-close-quote' are non-empty strings."
  :type 'string)

(defcustom LaTeX-csquotes-close-quote ""
  "Closing quotation mark to be used with the csquotes package.
The specified string will be used for `TeX-close-quote' (and override
the value of `LaTeX-german-close-quote' if the german or ngerman
package is used) only if both `LaTeX-csquotes-open-quote' and
`LaTeX-csquotes-close-quote' are non-empty strings."
  :type 'string)

(TeX-add-style-hook
 "csquotes"
 (lambda ()
   (let ((quote-style-variant-list '(("quotes") ("guillemets") ("american")
				     ("british") ("oldstyle") ("imprimerie")
				     ("swiss")))
	 (quote-style-name-list '(("danish") ("dutch") ("english") ("finnish")
				  ("french") ("german") ("italian")
				  ("norwegian") ("swedish"))))
     ;; New symbols
     (TeX-add-symbols
      '("enquote" 1)
      '("enquote*" 1)
      '("foreignquote" 2)
      '("foreignquote*" 2)
      '("hyphenquote" 2)
      '("hyphenquote*" 2)
      '("blockquote" ["Line threshold"] 2)
      '("foreignblockquote" t ["Line threshold"] nil nil)
      '("hyphenblockquote" t ["Line threshold"] nil nil)
      `("setquotestyle"
	[ (TeX-arg-eval completing-read "Quote style variant: "
			',quote-style-variant-list) ]
	(TeX-arg-eval completing-read "Quote style name or alias: "
		      ',quote-style-name-list))
      '("MakeInnerQuote" "Character")
      '("MakeOuterQuote" "Character")
      '("MakeAutoQuote" "Opening quotation mark" "Closing quotation mark")
      '("MakeForeignQuote" "Babel's language name"
	"Opening quotation mark" "Closing quotation mark")
      '("MakeHyphenQuote" "Babel's language name"
	"Opening quotation mark" "Closing quotation mark")
      "RestoreQuotes"
      `("DeclareQuoteStyle"
	[ (TeX-arg-eval completing-read "Quote style variant: "
			',quote-style-variant-list) ]
	(TeX-arg-eval completing-read "Quote style name: "
		      ',quote-style-name-list)
	["Outer quote initialization"] ["Inner quote initialization"]
	"Opening outer quotation mark" ["Middle outer quotation mark"]
	"Closing outer quotation mark" ["Kerning between adjoining marks"]
	"Opening inner quotation mark" ["Middle inner quotation mark"]
	"Closing inner quotation mark")
      `("DeclareQuoteAlias"
	[ (TeX-arg-eval completing-read "Quote style variant: "
			',quote-style-variant-list) ]
	(TeX-arg-eval completing-read "Quote style name: "
		      ',quote-style-name-list)
	"Alias name")
    '("DeclareQuoteOption" 1)
    '("DeclarePlainStyle" "Outer quotation mark" "Inner quotation mark")
    '("setblockthreshold" "Number of lines")
    '("setblockenvironment" "Environment")
    '("blockcite" 1))
   ;; New environments
   (LaTeX-add-environments
    "quoteblock")
   ;; Quotation marks
   (when (and (> (length LaTeX-csquotes-open-quote) 0)
	      (> (length LaTeX-csquotes-close-quote) 0))
     (make-local-variable 'TeX-open-quote)
     (setq TeX-open-quote LaTeX-csquotes-open-quote)
     (make-local-variable 'TeX-close-quote)
     (setq TeX-close-quote LaTeX-csquotes-close-quote)
     (make-local-variable 'TeX-quote-after-quote)
     (setq TeX-quote-after-quote LaTeX-csquotes-quote-after-quote))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (add-to-list 'font-latex-match-function-keywords-local "RestoreQuotes")
     (font-latex-match-function-make)
     (add-to-list 'font-latex-match-reference-keywords-local "blockcite")
     (font-latex-match-reference-make)
     (mapcar (lambda (keyword)
	       (add-to-list 'font-latex-match-textual-keywords-local keyword))
	     '("enquote"
	       "foreignquote"
	       "hyphenquote"
	       "blockquote"
	       "foreignblockquote"
	       "hyphenblockquote"))
     (font-latex-match-textual-make)
     (mapcar (lambda (keyword)
	       (add-to-list 'font-latex-match-variable-keywords-local keyword))
	     '("setquotestyle"
	       "MakeOuterQuote"
	       "MakeInnerQuote"
	       "MakeAutoQuote"
	       "MakeForeignQuote"
	       "MakeHyphenQuote"
	       "DeclareQuoteStyle"
	       "DeclareQuoteAlias"
	       "DeclareQuoteOption"
	       "DeclarePlainStyle"))
     (font-latex-match-variable-make)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults)))))

;;; csquotes.el ends here
