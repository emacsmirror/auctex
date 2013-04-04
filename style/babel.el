;;; babel.el --- AUCTeX style for `babel.sty'

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2005-05-29
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

;; This file adds support for `babel.sty'.

;;; Code:

(defvar LaTeX-babel-language-list
  '("acadian" "albanian" "afrikaans" "american" "australian" "austrian" "bahasa"
    "indonesian" "indon" "bahasai" "malay" "meyalu" "bahasam" "basque" "brazil"
    "brazilian" "breton" "british" "bulgarian" "canadian" "canadien" "catalan"
    "croatian" "czech" "danish" "dutch" "english" "esperanto" "estonian" "finnish"
    "francais" "frenchb" "french" "galician" "german" "germanb" "greek"
    "polutonikogreek" "hebrew" "hungarian" "icelandic" "interlingua" "irish"
    "italian" "latin" "lowersorbian" "magyar" "naustrian" "newzealand" "ngerman"
    "norsk" "samin" "nynorsk" "polish" "portuges" "portuguese" "romanian"
    "russian" "scottish" "serbian" "slovak" "slovene" "spanish" "swedish" "turkish"
    "ukrainian" "uppersorbian" "welsh" "UKenglish" "USenglish")
  "List of languages supported by the babel LaTeX package.")

(defvar LaTeX-babel-package-options
  (append LaTeX-babel-language-list '("activeacute" "activegrave"
				      "KeepShorthandsActive"))
  "Package options for the babel package.")

(defun LaTeX-babel-active-languages ()
  "Return a list of languages used in the document."
  (let (active-languages)
    ;; Loop over options provided to class and `babel' package at load time.
    (dolist (elt (append
		  ;; In most cases there is only one element in the alist, if
		  ;; there is more than one element, the first one should
		  ;; contain the class options of the current buffer.  So we can
		  ;; take the car of `LaTeX-provided-class-options'.
		  (cdr (car LaTeX-provided-class-options))
		  (cdr (assoc "babel" LaTeX-provided-package-options))))
      (when (member elt LaTeX-babel-language-list)
	;; Append element to `active-languages' to respect loading order.
	;; `babel' package uses as default language the last loaded one.
	(add-to-list 'active-languages elt t)))
  active-languages))

(defun TeX-arg-babel-lang (optional &optional prompt)
  "Prompt for a language with completion and insert it as an argument."
  (TeX-argument-insert
   (completing-read "Language: " (LaTeX-babel-active-languages)) nil))

(defun LaTeX-env-babel-lang (env)
  "Prompt for a language and insert it as an argument of ENV."
  (LaTeX-insert-environment
   env (format "{%s}" (completing-read "Language: "
				       (LaTeX-babel-active-languages)))))

(TeX-add-style-hook
 "babel"
 (lambda ()
   ;; Run style hooks for every active language in loading order, so
   ;; `TeX-quote-language' will be correctly set.
   (mapc 'TeX-run-style-hooks (LaTeX-babel-active-languages))
   ;; New symbols
   (TeX-add-symbols
    '("selectlanguage" TeX-arg-babel-lang)
    '("foreignlanguage" TeX-arg-babel-lang t)
    "languagename"
    '("iflanguage" TeX-arg-babel-lang t nil)
    '("useshorthands" t)
    '("defineshorthand" t nil)
    '("aliasshorthand" t nil)
    '("languageshorthands" TeX-arg-babel-lang)
    '("shorthandon" t)
    '("shorthandoff" t)
    '("languageattribute" TeX-arg-babel-lang t))
   ;; New environments
   (LaTeX-add-environments
    '("otherlanguage" LaTeX-env-babel-lang)
    '("otherlanguage*" LaTeX-env-babel-lang)
    '("hyphenrules" LaTeX-env-babel-lang))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("selectlanguage" "{")
				("foreignlanguage" "{{")
				("iflanguage" "{{{")
				("languagename" "")
				("useshorthands" "{")
				("languageshorthands" "{")
				("shorthandon" "{")
				("shorthandoff" "{"))
			      'function)
     (font-latex-add-keywords '(("defineshorthand" "{{")
				("aliasshorthand" "{{")
				("languageattribute" "{{"))
			      'variable))))

;;; babel.el ends here
