;;; scrkbase.el --- AUCTeX style for KOMA scrkbase.sty    -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2026-10-26
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a helper style loaded by KOMA classes.  scrkbase.sty provides
;; general macros like \KOMAoptions or \setkomafont; the options used
;; are defined elsewhere.  We add the options also here in order to
;; avoid code duplications.  This style corresponds to `scrkbase.sty'
;; v3.49.3 from 2026-10-19.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))
(defvar LaTeX-typearea-package-options-list)

(defvar LaTeX-KOMA-key-val-options
  (let ((bools '("true" "on" "yes" "false" "off" "no")))
    `(("draft" ,bools)
      ("overfullrule" ,bools)
      ("fontsize" ("10pt" "11pt" "12pt"))
      ("sfdefaults" ,bools)
      ;; 3.7. Document Titles
      ("titlepage" (,@bools  "firstiscover"))
      ;; 3.8. Abstract
      ("abstract" ,bools)
      ;; 3.9. Table of Contents
      ("toc" ("bibliography" "bib"
              "bibliographynumbered" "bibnumbered" "numberedbibliography" "numberedbib"
              "chapterentrywithdots" "chapterentrydotfill"
              "chapterentrywithoutdots" "chapterentryfill"
              "flat" "left"
              "graduated" "indent" "indented"
              "indenttextentries" "indentunnumbered" "numberline"
              "index" "idx"
              "indexnumbered" "idxnumbered" "numberedindex" "numberedidx"
              "leftaligntextentries" "leftalignunnumbered" "nonumberline"
              "listof"
              "listofnumbered" "numberedlistof"
              "nobibliography" "nobib"
              "noindex" "noidx"
              "nolistof"
              "sectionentrywithdots" "sectionentrydotfill"
              "sectionentrywithoutdots" "sectionentryfill"))
      ;; 3.10. Marking Paragraphs
      ("parskip" (,@bools "full" "full-" "full+" "full*"
                          "half" "half-" "half+" "half*" "never"))
      ;; 3.12 Headers and Footers Using Predefined Page Styles
      ("headsepline" ,bools)
      ("footsepline" ,bools)
      ;; 3.14. Footnotes
      ("footnotes" ("multiple" "nomultiple"))
      ;; 3.16. Document Structure
      ("headings" ("big" "normal"
                   "onelineappendix" "noappendixprefix" "appendixwithoutprefix"
                   "appendixwithoutprefixline"
                   "onelinechapter" "nochapterprefix" "chapterwithoutprefix"
                   "chapterwithoutprefixline"
                   "openany" "openleft" "openright"
                   "optiontohead" "optiontoheadandtoc" "optiontotocandhead"
                   "optiontotoc"
                   "small" "standardclasses"
                   "twolineappendix" "appendixprefix" "appendixwithprefix"
                   "appendixwithprefixline"
                   "twolinechapter" "chapterprefix" "chapterwithprefix"
                   "chapterwithprefixline"))
      ("numbers" ("autoendperiod" "autoenddot" "auto"
                  "endperiod" "withendperiod" "periodatend" "enddot" "withenddot"
                  "dotatend"
                  "noendperiod" "noperiodatend" "noenddot" "nodotatend"))
      ;; 3.20. Floating Environments for Tables and Figures
      ("captions" ("bottombeside" "besidebottom"
                   "centeredbeside" "besidecentered" "middlebeside" "besidemiddle"
                   "figureheading" "figureabove" "abovefigure" "topatfigure"
                   "figuresignature" "belowfigure" "bottomatfiggure"
                   "heading" "above" "top"
                   "innerbeside" "besideinner"
                   "leftbeside" "besideleft"
                   "nooneline" "oneline"
                   "outerbeside" "besideouter"
                   "rightbeside" "besideright"
                   "signature" "below" "bot" "bottom"
                   "tableheading" "tableabove" "abovetable" "abovetabular" "topattable"
                   "tablesignature" "belowtable" "belowtabular" "bottomattable"
                   "topbeside" "besidetop"))
      ("listof" ("chapterentry" "withchapterentry"
                 "chaptergapline" "onelinechaptergap"
                 "chaptergapsmall" "smallchaptergap"
                 "entryprefix"
                 "flat" "left"
                 "graduated" "indent" "indented"
                 "leveldown"
                 "indenttextentries" "indentunnumbered" "numberline"
                 "leftaligntextentries" "leftalignunnumbered" "nonumberline"
                 "nochaptergap" "ignorechapter"
                 "notoc" "nottotoc" "plainheading"
                 "numbered" "totocnumbered" "tocnumbered" "numberedtoc" "numberedtotoc"
                 "standardlevel"
                 "totoc" "toc" "notnumbered"))
      ;; 3.23. Bibliography
      ("bibliography" ("leveldown"
                       "notoc" "nottotoc" "plainheading"
                       "numbered" "tocnumbered" "totocnumbered" "numberedtoc"
                       "numberedtotoc"
                       "oldstyle"
                       "openstyle"
                       "standardlevel"
                       "totoc" "toc" "notnumbered"))
      ;; 3.24. Index
      ("index" ("leveldown"
                "notoc" "nottotoc" "plainheading"
                "numbered" "tocnumbered" "totocnumbered" "numberedtoc"
                "numberedtotoc"
                "standardlevel"
                "totoc" "toc" "notnumbered"))))
  "Key=val options for macros from KOMA-script.")

(defun LaTeX-KOMA-key-val-options ()
  "Return key=val options for macros from KOMA-script."
  (let ((bools '("true" "on" "yes" "false" "off" "no"))
        (styles (TeX-style-list)))
    (append
     (when (cl-intersection '("scrartcl" "scrreprt") styles :test #'string-equal)
       `(("abstract" ,bools)))
     (when (cl-intersection '("scrbook" "scrreprt") styles :test #'string-equal)
       `(("chapterentrydot" ,bools)
         ;; 3.16. Document Structure
         ("open" ("any" "left" "right"))
         ("chapterprefix" ,bools)
         ("appendixprefix" ,bools)
         ("chapteratlists" ("entry"))))
     (when (member "scrartcl" styles)
       `(("sectionentrydot" ,bools)))
     `(("cleardoublepage" ("current" ,@(mapcar #'car (LaTeX-pagestyle-list)))))
     LaTeX-typearea-package-options-list
     LaTeX-KOMA-key-val-options)))

(defvar-local LaTeX-KOMA-font-elements
  '("author"
    "caption"
    "captionlabel"
    "chapter"
    "chapterentry"
    "chapterentrydots"
    "chapterentrypagenumber"
    "chapterprefix"
    "date"
    "dedication"
    "descriptionlabel"
    "dictum"
    "dictumauthor"
    "dictumtext"
    "disposition"
    "enumeratelabel"
    "footnote"
    "footnotelabel"
    "footnotereference"
    "footnoterule"
    "itemizelabel"
    "labelenumi"
    "labelenumii"
    "labelenumiii"
    "labelenumiv"
    "labelinglabel"
    "labelingseparator"
    "labelitemi"
    "labelitemii"
    "labelitemiii"
    "labelitemiv"
    "minisec"
    "pagefoot"
    "pagehead"
    "pageheadfoot"
    "pagenumber"
    "pagination"
    "paragraph"
    "part"
    "partentry"
    "partentrypagenumber"
    "partnumber"
    "publishers"
    "section"
    "sectionentry"
    "sectionentrydots"
    "sectionentrypagenumber"
    "sectioning"
    "subject"
    "subparagraph"
    "subsection"
    "subsubsection"
    "title"
    "titlehead")
  "List of KOMA-Script's font elements.
This variable is defined as a buffer-local one so that other style hooks
can add elements without using `make-local-variable' on it.")

(TeX-add-style-hook
 "scrkbase"
 (lambda ()
   (TeX-run-style-hooks "scrbase" "typearea")

   (TeX-add-symbols
    ;; 3.1. Early or Late Selection of Options
    '("KOMAoptions"
      (TeX-arg-key-val (LaTeX-KOMA-key-val-options)))

    `("KOMAoption"
      (TeX-arg-completing-read (LaTeX-KOMA-key-val-options) "Key")
      (TeX-arg-completing-read
       ,(lambda ()
          (let ((key (save-excursion
                       (re-search-backward "\\\\KOMAoption{\\([^}]+\\)}"
                                           (line-beginning-position) t)
                       (match-string-no-properties 1))))
            (when key
              (cadr (assoc key (LaTeX-KOMA-key-val-options))))))
       "Value"))

    ;; 3.6. Text Markup
    "maybesffamily"
    '("textmaybesf" t)

    '("setkomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element")
      (TeX-arg-font-switches "Commands"))
    '("addtokomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element")
      (TeX-arg-font-switches "Commands"))

    '("usekomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element"))
    '("usefontofkomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element"))
    '("useencodingofkomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element"))
    '("usesizeofkomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element"))
    '("usefamilyofkomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element"))
    '("useseriesofkomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element"))
    '("useshapeofkomafont"
      (TeX-arg-completing-read LaTeX-KOMA-font-elements "Element")))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textmaybesf"         "{"))
                              'type-command)
     (font-latex-add-keywords '("maybesffamily")
                              'type-declaration)
     (font-latex-add-keywords '(("addtokomafont"     "{{")
                                ("setkomafont"       "{{"))
                              'variable)
     (font-latex-add-keywords '(("KOMAoption"        "{{")
                                ("KOMAoptions"       "{"))
                              'function)))

 TeX-dialect)

;;; scrkbase.el ends here
