;;; font-latex.el --- LaTeX fontification for Font Lock mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 1996-2025  Free Software Foundation, Inc.

;; Authors:    Peter S. Galbraith <psg@debian.org>
;;             Simon Marshall <Simon.Marshall@esrin.esa.it>
;; Maintainer: auctex-devel@gnu.org
;; Created:    06 July 1996
;; Keywords:   tex, text, faces

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

;; This package enhances font-lock fontification patterns for LaTeX.
;; font-lock mode is a minor mode that causes your comments to be
;; displayed in one face, strings in another, reserved words in
;; another, and so on.

;;; Code:

(require 'font-lock)
(require 'tex)

(defgroup font-latex nil
  "Font-latex text highlighting package."
  :prefix "font-latex-"
  :group 'faces
  :group 'tex
  :group 'AUCTeX)

(defgroup font-latex-keywords nil
  "Keywords for highlighting text in font-latex."
  :prefix "font-latex-"
  :group 'font-latex)

(defgroup font-latex-highlighting-faces nil
  "Faces for highlighting text in font-latex."
  :prefix "font-latex-"
  :group 'font-latex)

(defvar font-latex-multiline-boundary 5000
  "Size of region to search for the start or end of a multiline construct.")

(defvar-local font-latex-quote-regexp-beg nil
  "Regexp used to find quotes.")

(defvar font-latex-quote-list '(("``" "''") ("<<" ">>" french) ("«" "»" french))
  "List of quote specifiers for quotation fontification.

Each element of the list is either a list consisting of two
strings to be used as opening and closing quotation marks
independently of the value of `font-latex-quotes' or a list with
three elements where the first and second element are strings for
opening and closing quotation marks and the third element being
either the symbol `german' or `french' describing the order of
quotes.

If `font-latex-quotes' specifies a different state, order of the
added quotes will be reversed for fontification.  For example if
\\='(\"\\\"<\" \"\\\">\" french) is given but `font-latex-quotes'
specifies `german', quotes will be used like \">foo\"< for
fontification.")

(defvar-local font-latex-quotes-control nil
  "Internal variable for keeping track if `font-latex-quotes' changed.")

(defvar-local font-latex-quotes-internal nil
  "Internal variable for tracking outcome of automatic detection.
If automatic detection is not enabled, it is assigned the value
of `font-latex-quotes'.")

(defvar font-latex-quotes-fallback 'french
  "Fallback value for `font-latex-quotes' if automatic detection fails.")

(defvar font-latex-quote-style-list-french
  '("french" "frenchb" "frenchle" "frenchpro" "francais" "canadien"
    "acadian" "italian")
  "List of styles for which French-style quote matching should be activated.")

(defvar font-latex-quote-style-list-german
  '("austrian" "german" "germanb" "naustrian" "ngerman")
  "List of styles for which German-style quote matching should be activated.")

(defcustom font-latex-quotes 'auto
  "Whether to fontify << French quotes >> or >>German quotes<<.
Also selects \"<quote\"> versus \">quote\"<.

If value `auto' is chosen, an attempt is being made in deriving
the type of quotation mark matching from document settings like
the language option supplied to the babel package.

If nil, quoted content will not be fontified."
  :type '(choice (const auto) (const french) (const german) (const nil))
  :group 'font-latex
  :safe (lambda (x) (memq x '(auto french german nil))))

(defun font-latex-add-quotes (quotes)
  "Add QUOTES to `font-latex-quote-list'.
QUOTES has to be a list adhering to the format of an element of
`font-latex-quote-list'."
  (setq font-latex-quotes-control nil)
  (add-to-list (make-local-variable 'font-latex-quote-list) quotes))

(defun font-latex-quotes-set-internal ()
  "Set `font-latex-quotes-internal' according to `font-latex-quotes'.
If `font-latex-quotes' is set to `auto', try to derive the
correct value from document properties."
  (setq font-latex-quotes-internal
        (if (eq font-latex-quotes 'auto)
            (or (when (TeX-elt-of-list-member
                       font-latex-quote-style-list-french TeX-active-styles)
                  'french)
                (when (TeX-elt-of-list-member
                       font-latex-quote-style-list-german TeX-active-styles)
                  'german)
                font-latex-quotes-fallback)
          font-latex-quotes)))
;; Update the value of `font-latex-quotes-internal' when the list of
;; styles changes.
(add-hook 'TeX-update-style-hook #'font-latex-quotes-set-internal)

;; The definitions of the title faces were originally taken from
;; info.el (Copyright (C) 1985, 86, 92, 93, 94, 95, 96, 97, 98, 99,
;; 2000, 2001 Free Software Foundation, Inc.) and adapted to the needs
;; of font-latex.el.

(defconst font-latex-sectioning-max 5
  "Highest number for font-latex-sectioning-N-face")
(defface font-latex-sectioning-5-face
  '((((type tty pc) (class color) (background light))
     (:foreground "blue4" :weight bold))
    (((type tty pc) (class color) (background dark))
     (:foreground "yellow" :weight bold))
    (((class color) (background light))
     (:weight bold :inherit variable-pitch :foreground "blue4"))
    (((class color) (background dark))
     (:weight bold :inherit variable-pitch :foreground "yellow"))
    (t (:weight bold :inherit variable-pitch)))
  "Face for sectioning commands at level 5."
  :group 'font-latex-highlighting-faces)

(defcustom font-latex-fontify-sectioning 1.1
  "Whether to fontify sectioning macros with varying height or a color face.

If it is a number, use varying height faces.  The number is used
for scaling starting from `font-latex-sectioning-5-face'.  Typically
values from 1.05 to 1.3 give best results, depending on your font
setup.  If it is the symbol `color', use `font-lock-type-face'.

Caveats: Customizing the scaling factor applies to all sectioning
faces unless those face have been saved by customize.  Setting
this variable directly does not take effect unless you call
`font-latex-update-sectioning-faces' or restart Emacs.

Switching from `color' to a number or vice versa does not take
effect unless you call \\[font-lock-fontify-buffer] or restart
Emacs."
  :type '(choice (number :tag "Scale factor")
                 (const color))
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (unless (eq value 'color)
           (font-latex-update-sectioning-faces font-latex-sectioning-max value)))
  :group 'font-latex)

(defun font-latex-update-sectioning-faces (&optional max height-scale)
  "Update sectioning commands faces."
  (unless height-scale
    (setq height-scale (if (numberp font-latex-fontify-sectioning)
                           ;; Make sure `height-scale' is a floating point
                           ;; number because `set-face-attribute' treats
                           ;; integers differently from floating points.
                           (float font-latex-fontify-sectioning)
                         1.1)))
  (unless max
    (setq max font-latex-sectioning-max))
  (dotimes (num max)
    (let* (;; reverse for XEmacs:
           (num (- max (1+ num)))
           (face-name (intern (format "font-latex-sectioning-%s-face" num))))
      (unless (get face-name 'saved-face) ; Do not touch customized faces.
        (set-face-attribute face-name nil :height  height-scale)))))

(defun font-latex-make-sectioning-faces (max &optional height-scale)
  "Build the faces used to fontify sectioning commands."
  (unless max (setq max font-latex-sectioning-max))
  (unless height-scale
    (setq height-scale (if (numberp font-latex-fontify-sectioning)
                           ;; Make sure `height-scale' is a floating point
                           ;; number because the integer type is treated
                           ;; differently.
                           (float font-latex-fontify-sectioning)
                         1.1)))
  (dotimes (num max)
    (let* ((num (- max (1+ num)))
           (face-name (intern (format "font-latex-sectioning-%s-face" num)))
           (f-inherit (intern (format "font-latex-sectioning-%s-face" (1+ num)))))
      (eval
       `(defface ,face-name
          '((t (:height ,height-scale :inherit ,f-inherit)))
          (format "Face for sectioning commands at level %s.

Probably you don't want to customize this face directly.  Better
change the base face `font-latex-sectioning-5-face' or customize the
variable `font-latex-fontify-sectioning'." ',num)
          :group 'font-latex-highlighting-faces)
       t))))

(font-latex-make-sectioning-faces font-latex-sectioning-max)


;;; Keywords

(eval-and-compile
(defconst font-latex-built-in-keyword-classes
  '(("warning"
     ("nopagebreak" "pagebreak" "newpage" "clearpage" "cleardoublepage"
      "enlargethispage" "nolinebreak" "linebreak" "newline" "-" "\\" "\\*"
      "appendix" "displaybreak" "allowdisplaybreaks" "tabularnewline"
      "backmatter" "frontmatter" "mainmatter"
      "makeatletter" "makeatother" "newblock" "suppressfloats" "endinput"
      "reversemarginpar")
     font-latex-warning-face 1 noarg)
    ("variable"
     (("setlength" "|{\\|{\\") ("addtolength" "|{\\|{\\")
      ("settowidth" "|{\\|{\\") ("settoheight" "|{\\|{\\") ("settodepth" "|{\\|{\\")
      ("setcounter" "{|{\\") ("addtocounter" "{|{\\")
      ("stepcounter" "{") ("refstepcounter" "{")
      ("counterwithin" "*[{{") ("counterwithout" "*[{{")
      ("arabic" "{") ("roman" "{") ("Roman" "{") ("alph" "{") ("Alph" "{")
      ("fnsymbol" "{"))
     font-lock-variable-name-face 2 command)
    ("biblatexnoarg"
     ("newrefsegment" "mancite" "pno" "ppno" "nopp" "psq" "psqq")
     font-lock-variable-name-face 2 noarg)
    ("biblatex"
     (;; 3.2.2 Setting Package Options
      ("ExecuteBibliographyOptions" "[{")
      ;; 3.7.1 Resources
      ("addbibresource" "[{") ("addglobalbib" "[{") ("addsectionbib" "[{")
      ;; 3.7.2 The Bibliography
      ("printbibliography" "[") ("bibbysection"    "[") ("bibbysegment" "[")
      ("bibbycategory"     "[") ("printbibheading" "[")
      ;; 3.7.3 Bibliography Lists
      ("printbiblist" "[{") ("printshorthands" "[")
      ;; 3.7.4 Bibliography Sections
      ("newrefsection" "[")
      ;; 3.7.6 Bibliography Categories
      ("DeclareBibliographyCategory" "{") ("addtocategory" "{{")
      ;; 3.7.7 Bibliography Headings and Environments
      ("defbibenvironment" "{{{{") ("defbibheading" "{[{")
      ;; 3.7.8 Bibliography Notes
      ("defbibnote" "{{")
      ;; 3.7.9 Bibliography Filters and Checks
      ("defbibfilter" "{{") ("defbibcheck" "{{")
      ;; 3.7.10 Reference Contexts
      ("DeclareRefcontext"       "{{")  ("newrefcontext"        "[{")
      ("assignrefcontextkeyws"   "*[{") ("assignrefcontextcats" "*[{")
      ("assignrefcontextentries" "*[{")
      ;; 3.7.11 Dynamic Entry Sets
      ("defbibentryset" "{{")
      ;; 3.8.1 Standard Commands
      ("Cite" "[[{")
      ("parencite" "*[[{") ("Parencite"    "[[{")
      ("footcite"  "[[{")  ("footcitetext" "[[{")
      ;; 3.8.2 Style-specific Commands
      ("textcite"  "[[{") ("Textcite"  "[[{")
      ("smartcite" "[[{") ("Smartcite" "[[{")
      ("supercite" "{")
      ;; 3.8.3 Qualified Citation Lists
      ;; For qualified lists, fontify at least 2 mandatory arguments
      ("cites"      "(([[{[[{") ("Cites"         "(([[{[[{")
      ("parencites" "(([[{[[{") ("Parencites"    "(([[{[[{")
      ("footcites"  "(([[{[[{") ("footcitetexts" "(([[{[[{")
      ("smartcites" "(([[{[[{") ("Smartcites"    "(([[{[[{")
      ("textcites"  "(([[{[[{") ("Textcites"     "(([[{[[{")
      ("supercites" "(([[{[[{")
      ;; 3.8.4 Style-independent Commands
      ("autocite" "*[[{")      ("Autocite" "*[[{")
      ("autocites" "(([[{[[{") ("Autocites" "(([[{[[{")
      ;; 3.8.5 Text Commands
      ("citeauthor" "*[[{") ("Citeauthor" "*[[{") ("citetitle" "*[[{")
      ("citeyear"   "*[[{") ("citedate" "*[[{")
      ("citeurl"    "[[{")  ("parentext" "{")
      ("brackettext" "{")
      ;; 3.8.6 Special Commands
      ("fullcite"  "[[{")         ("footfullcite" "[[{")
      ("volcite"   "[{[{")        ("Volcite"      "[{[{")
      ("volcites"  "(([{[{[{[{")  ("Volcites"     "(([{[{[{[{")
      ("pvolcite"  "[{[{")        ("Pvolcite"     "[{[{")
      ("pvolcites" "(([{[{[{[{")  ("Pvolcites"    "(([{[{[{[{")
      ("fvolcite"  "[{[{")        ("ftvolcite"    "[{[{")
      ("fvolcites" "(([{[{[{[{")  ("Fvolcites"    "(([{[{[{[{")
      ("svolcite"  "[{[{")        ("Svolcite"     "[{[{")
      ("svolcites" "(([{[{[{[{")  ("Svolcites"    "(([{[{[{[{")
      ("tvolcite"  "[{[{")        ("Tvolcite"     "[{[{")
      ("tvolcites" "(([{[{[{[{")  ("Tvolcites"    "(([{[{[{[{")
      ("avolcite"  "[{[{")        ("Avolcite"     "[{[{")
      ("avolcites" "(([{[{[{[{")  ("Avolcites"    "(([{[{[{[{")
      ("notecite"  "[[{")         ("Notecite"     "[[{")
      ("pnotecite" "[[{")         ("Pnotecite"    "[[{")
      ("fnotecite" "[[{")
      ;; 3.8.7 Low-level Commands
      ("citename" "[[{[{") ("citelist" "[[{[{") ("citefield" "[[{[{")
      ;; 3.8.8 Miscellaneous Commands
      ("citereset" "*") ("RN" "{") ("Rn" "{")
      ;; 3.9 Localization Commands
      ("DefineBibliographyStrings" "{{")  ("DefineBibliographyExtras" "{{")
      ("UndefineBibliographyExtras" "{{") ("DefineHyphenationExceptions" "{{")
      ("NewBibliographyString" "{"))
     font-lock-constant-face 2 command)
    ("reference"
     (("nocite" "*{") ("cite" "*[[{") ("label" "{") ("pageref" "{")
      ("vref" "*{") ("eqref" "{") ("ref" "{") ("Ref" "{")
      ("footref" "{") ("include" "{") ("input" "{")
      ("bibliography" "{") ("index" "{") ("glossary" "{")
      ("footnote" "[{") ("footnotemark" "[") ("footnotetext" "[{")
      ("marginpar" "[{") ("chaptermark" "{") ("sectionmark" "{")
      ("subsectionmark" "{") ("subsubsectionmark" "{")
      ("paragraphmark" "{") ("subparagraphmark" "{"))
     font-lock-constant-face 2 command)
    ("function"
     (("begin" "{") ("end" "{") ("pagenumbering" "{")
      ("thispagestyle" "{") ("pagestyle" "{") ("nofiles" "")
      ("includeonly" "{") ("bibliographystyle" "{") ("documentstyle" "[{")
      ("documentclass" "[{[") ("newenvironment" "*{[[{{")
      ("newcommand" "*|{\\[[{") ("newlength" "|{\\")
      ("newtheorem" "{[{[")
      ("providecommand" "*|{\\[[{")
      ("newcounter" "{[") ("renewenvironment" "*{[[{{")
      ("renewcommand" "*|{\\[[{") ("renewtheorem" "{[{[")
      ("usepackage" "[{[") ("RequirePackage" "[{[")
      ("fbox" "{") ("mbox" "{") ("rule" "[{{")
      ("framebox" "|[([{") ("makebox" "|[([{") ("newsavebox" "|{\\")
      ("parbox" "[[[{{") ("savebox" "|{\\|[([{") ("sbox" "|{\\{")
      ("usebox" "|{\\")
      ("cline" "{") ("extracolsep" "{") ("multicolumn" "{{{")
      ("linethickness" "{") ("multiput" "(({{") ("put" "({")
      ("qbezier" "[(((") ("raisebox" "{[[{")
      ("addvspace" "{") ("vspace" "*{") ("hspace" "*{")
      ("addcontentsline" "{{{") ("addtocontents" "{{")
      ("labelformat" "{{") ("linespread" "{")
      ("AddToHook" "{[") ("RemoveFromHook" "{[") ("AddToHookNext" "{")
      ("ProvidesClass" "{[") ("ProvidesPackage" "{[") ("ProvidesFile" "{[")
      ("NewMarkClass" "{")
      ("DeclareDocumentCommand" "|{\\{{")
      ("NewDocumentCommand"     "|{\\{{")
      ("ProvideDocumentCommand" "|{\\{{")
      ("RenewDocumentCommand"   "|{\\{{")
      ("DeclareExpandableDocumentCommand" "|{\\{{")
      ("NewExpandableDocumentCommand"     "|{\\{{")
      ("ProvideExpandableDocumentCommand" "|{\\{{")
      ("RenewExpandableDocumentCommand"   "|{\\{{")
      ("DeclareDocumentEnvironment" "{{{{")
      ("NewDocumentEnvironment"     "{{{{")
      ("ProvideDocumentEnvironment" "{{{{")
      ("RenewDocumentEnvironment"   "{{{{")
      ("NewCommandCopy"     "|{\\|{\\")
      ("RenewCommandCopy"   "|{\\|{\\")
      ("DeclareCommandCopy" "|{\\|{\\")
      ("ShowCommand"        "|{\\")
      ("NewEnvironmentCopy"     "{{")
      ("RenewEnvironmentCopy"   "{{")
      ("DeclareEnvironmentCopy" "{{")
      ("ShowEnvironment"        "{")
      ("listfiles" "[") ("hyphenation" "{") ("DocumentMetadata" "{"))
     font-lock-function-name-face 2 command)
    ("function-noarg"
     ("enspace" "enskip" "quad" "qquad" "nonumber"
      "bigskip" "medskip"  "smallskip"
      "thinspace"  "negthinspace"
      "thicklines" "thinlines"
      "noindent" "hline" "ldots"
      "centering" "raggedright" "raggedleft"
      "raggedbottom" "flushbottom"
      "TeX" "LaTeX" "LaTeXe"
      "normalfont" "normalshape"
      "tableofcontents" "listoffigures" "listoftables"
      "maketitle" "makeindex" "makeglossary"
      "sloppy" "fussy" "par")
     font-lock-keyword-face 2 noarg)
    ("sectioning-0"
     (("part" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
         'font-lock-type-face
       'font-latex-sectioning-0-face)
     2 command)
    ("sectioning-1"
     (("chapter" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
         'font-lock-type-face
       'font-latex-sectioning-1-face)
     2 command)
    ("sectioning-2"
     (("section" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
         'font-lock-type-face
       'font-latex-sectioning-2-face)
     2 command)
    ("sectioning-3"
     (("subsection" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
         'font-lock-type-face
       'font-latex-sectioning-3-face)
     2 command)
    ("sectioning-4"
     (("subsubsection" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
         'font-lock-type-face
       'font-latex-sectioning-4-face)
     2 command)
    ("sectioning-5"
     (("paragraph" "*[{") ("subparagraph" "*[{")
      ("subsubparagraph" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
         'font-lock-type-face
       'font-latex-sectioning-5-face)
     2 command)
    ("slide-title" () font-latex-slide-title-face 2 command)
    ("textual"
     (("item" "[") ("bibitem" "[{") ("title" "{") ("author" "{") ("date" "{")
      ("thanks" "{") ("address" "{") ("caption" "[{")
      ("textsuperscript" "{") ("textsubscript" "{") ("verb" "*"))
     font-lock-type-face 2 command)
    ("bold-command"
     (("textbf" "{") ("textsc" "{") ("textssc" "{") ("textulc" "{")
      ("textup" "{") ("textsw" "{") ("boldsymbol" "{") ("pmb" "{")
      ("mathbf" "{"))
     font-latex-bold-face 1 command)
    ("italic-command"
     (("emph" "{") ("textit" "{") ("textsl" "{") ("mathit" "{"))
     font-latex-italic-face 1 command)
    ("underline-command"
     (("underline" "{"))
     font-latex-underline-face 1 command)
    ("math-command"
     (("ensuremath" "|{\\"))
     font-latex-math-face 1 command)
    ("type-command"
     (("texttt" "{") ("textsf" "{") ("textrm" "{") ("textmd" "{")
      ("textnormal" "{") ("oldstylenums" "{") ("legacyoldstylenums" "{")
      ("mathrm" "{") ("mathsf" "{") ("mathtt" "{"))
     font-lock-type-face 1 command)
    ("bold-declaration"
     ("bf" "bfseries" "sc" "scshape" "sscshape" "ulcshape" "upshape" "swshape")
     font-latex-bold-face 1 declaration)
    ("italic-declaration"
     ("em" "it" "itshape" "sl" "slshape")
     font-latex-italic-face 1 declaration)
    ("type-declaration"
     ("tt" "ttfamily" "sf" "sffamily" "rm" "rmfamily" "mdseries"
      "tiny" "scriptsize" "footnotesize" "small" "normalsize"
      "large" "Large" "LARGE" "huge" "Huge")
     font-lock-type-face 1 declaration))
  "Built-in keywords and specifications for font locking.

The first element of each item is the name of the keyword class.

The second element is a list of keywords (macros without an
escape character) to highlight or, if the fifth element is the
symbol `command', a list of lists where the first element of each
item is a keyword and the second a string specifying the macro
syntax.  It can contain \"*\" if the macro has a starred variant,
\"[\" for an optional argument, \"{\" for a mandatory argument,
and \"\\\" for a macro.  A \"|\" means the following two tokens
should be regarded as alternatives.

The third element is the symbol of a face to be used or a Lisp
form returning a face symbol.

The fourth element is the fontification level.

The fifth element is the type of construct to be matched.  It can
be one of `noarg' which will match simple macros without
arguments (like \"\\foo\"), `declaration' which will match macros
inside a TeX group (like \"{\\bfseries foo}\"), or `command' which
will match macros of the form \"\\foo[bar]{baz}\"."))

(defcustom font-latex-deactivated-keyword-classes nil
  "List of strings for built-in keyword classes to be deactivated.

Valid entries are \"warning\", \"variable\", \"biblatexnoarg\",
\"biblatex\", \"reference\", \"function\", \"function-noarg\",
\"sectioning-0\", \"sectioning-1\", \"sectioning-2\",
\"sectioning-3\", \"sectioning-4\", \"sectioning-5\",
\"slide-title\", \"textual\", \"bold-command\",
\"italic-command\", \"underline-command\", \"math-command\",
\"type-command\", \"bold-declaration\", \"italic-declaration\" or
\"type-declaration\".

You have to restart Emacs for a change of this variable to take effect."
  :group 'font-latex-keywords
  :type `(set ,@(mapcar
                 (lambda (spec)
                   `(const :tag ,(concat
                                  ;; Name of the keyword class
                                  (let ((name (split-string (car spec) "-")))
                                    (setcar name (capitalize (car name)))
                                    (mapconcat #'identity name " "))
                                  " keywords in `"
                                  ;; Name of the face
                                  (symbol-name
                                   (let ((face (nth 2 spec)))
                                     (if (symbolp face) face (eval face t))))
                                  "'.\n"
                                  ;; List of keywords
                                  (with-temp-buffer
                                    (insert "  Keywords: "
                                            (mapconcat (lambda (x)
                                                         (if (listp x)
                                                             (car x)
                                                           x))
                                                       (nth 1 spec) ", "))
                                    (fill-paragraph nil)
                                    (buffer-string)))
                           ,(car spec)))
                 font-latex-built-in-keyword-classes)))

(eval-and-compile
(defun font-latex--make-match-defun (prefix name face type)
  "Return a function definition for keyword matching.
The variable holding the keywords to match are determined by the
strings PREFIX and NAME.  The type of matcher is determined by
the symbol TYPE.

This is a helper function for `font-latex-make-built-in-keywords'
and `font-latex-make-user-keywords' and not intended for general
use."
  ;; FIXME: Is the cond-clause possible inside of the defun?

  ;; In an earlier version of font-latex the type could be a list like
  ;; (command 1).  This indicated a macro with one argument.  Provide
  ;; a match function in this case but don't actually support it.
  (cond ((or (eq type 'command) (listp type))
         `(defun ,(intern (concat prefix name)) (limit)
            ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex--make-match-defun'.")
            (when ,(intern (concat prefix name))
              (font-latex-match-command-with-arguments
               ,(intern (concat prefix name))
               (append
                (when (boundp ',(intern (concat prefix name
                                                "-keywords-local")))
                  ,(intern (concat prefix name "-keywords-local")))
                ,(intern (concat prefix name "-keywords")))
               ;; `face' can be a face symbol, a form returning
               ;; a face symbol, or a list of face attributes.
               ,(if (and (listp face) (fboundp (car face)))
                    face
                  `',face)
               limit))))
        ((eq type 'declaration)
         `(defun ,(intern (concat prefix name)) (limit)
            ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex--make-match-defun'.")
            (when ,(intern (concat prefix name))
              (font-latex-match-command-in-braces
               ,(intern (concat prefix name)) limit))))
        ((eq type 'noarg)
         `(defun ,(intern (concat prefix name)) (limit)
            ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex--make-match-defun'.")
            (when ,(intern (concat prefix name))
              (re-search-forward
               ,(intern (concat prefix name)) limit t))))))

(defun font-latex-keyword-matcher (prefix name face type)
  "Return a matcher and highlighter as required by `font-lock-keywords'.
PREFIX and NAME are strings which are concatenated to form the
respective match function.  FACE is a face name or a list of face
attributes that will be applied to the respective part of the
match returned by the match function.  A lisp form returning a
face name or a list of face attributes is also valid for FACE.
TYPE is the type of construct to be highlighted.  Currently the
symbols `command', `declaration' and `noarg' are valid.

This is a helper function for `font-latex-make-built-in-keywords'
and `font-latex-make-user-keywords' and not intended for general
use."
  ;; Quote a list of face attributes and a face symbol
  ;; but do not quote a form returning such value.
  (unless (and (listp face) (fboundp (car face)))
    (setq face `',face))

  ;; In an earlier version of font-latex the type could be a list like
  ;; (command 1).  This indicated a macro with one argument.  Provide
  ;; a matcher in this case but don't actually support it.
  (cond ((or (eq type 'command) (listp type))
         `(,(intern (concat prefix name))
           (0 (font-latex-matched-face 0) append t)
           (1 (font-latex-matched-face 1) append t)
           (2 (font-latex-matched-face 2) append t)
           (3 (font-latex-matched-face 3) append t)
           (4 (font-latex-matched-face 4) append t)
           (5 (font-latex-matched-face 5) append t)
           (6 (font-latex-matched-face 6) append t)
           (7 (font-latex-matched-face 7) append t)
           (8 (font-latex-matched-face 8) append t)
           (9 (font-latex-matched-face 9) append t)
           (10 (font-latex-matched-face 10) append t)
           (11 (font-latex-matched-face 11) append t)))
        ((eq type 'noarg)
         `(,(intern (concat prefix name))
           (0 ,face)))
        ((eq type 'declaration)
         `(,(intern (concat prefix name))
           (0 'font-latex-warning-face t t)
           (1 'font-lock-keyword-face append t)
           (2 ,face append t))))))

(defmacro font-latex-make-built-in-keywords ()
  "Build defuns, defvars and defcustoms for built-in keyword fontification."
  (let ((flks '())
        (defs '()))
    (dolist (item font-latex-built-in-keyword-classes)
    (let ((prefix "font-latex-match-")
          (name (nth 0 item))
          (keywords (nth 1 item))
          (face (nth 2 item))
          (level (nth 3 item))
          (type (nth 4 item)))

      ;; defvar font-latex-match-*-keywords-local
      (push `(defvar-local ,(intern (concat prefix name "-keywords-local"))
               ',keywords
               ,(concat "Buffer-local keywords to add to `"
                        prefix name "-keywords'.\n\n"
                        (if (eq type 'command)
                            "\
This must be a list where each element is a list consisting of a
keyword string (not a regular expression) omitting the leading
backslash and a format specifier as described in the doc string of
`font-latex-user-keyword-classes'."
                          "\
This must be a list where each element is a keyword string (not a
regular expression) omitting the leading backslash.")

                        "\n\n\
This is an internal variable which should not be set directly.
Use `font-latex-add-keywords' instead.

Generated by `font-latex-make-built-in-keywords'."))
            defs)

      ;; defvar font-latex-match-*
      ;; We make this variable buffer local later, but don't use
      ;; `defvar-local' here because it shouldn't have nil as its
      ;; default value.  Its true default value is set through
      ;; font-latex-match-*-make in :set specification of defcustom of
      ;; font-latex-match-*-keywords below.  It's only after that this
      ;; variable can be buffer local.
      (push `(defvar ,(intern (concat prefix name)) nil
               ,(concat "Regular expression to match " name
                        " keywords.

Generated by `font-latex-make-built-in-keywords'"))
            defs)

      ;; This defvar (without value) is here just to suppress compiler
      ;; warnings.  Its true definition is done by defcustom following
      ;; the next defun because its :set function depends on the
      ;; function defined by that defun.
      (push `(defvar ,(intern (concat prefix name "-keywords")))
            defs)

      ;; defun font-latex-match-*-make
      (push `(defun ,(intern (concat prefix name "-make")) ()
               ,(concat "Make or remake the variable `" prefix name "'.

Generated by `font-latex-make-built-in-keywords'.")
               (let ((keywords
                      (append
                       (unless (member ,name
                                       font-latex-deactivated-keyword-classes)
                         ,(intern (concat prefix name "-keywords-local")))
                       ,(intern (concat prefix name "-keywords"))))
                     multi-char-macros single-char-macros)
                 (dolist (elt keywords)
                   (let ((keyword (if (listp elt) (car elt) elt)))
                     (if (string-match "^[A-Za-z]" keyword)
                         (push keyword multi-char-macros)
                       (push keyword single-char-macros))))
                 (when (or multi-char-macros single-char-macros)
                   (setq ,(intern (concat prefix name))
                         (concat
                          "\\\\\\("
                          (when multi-char-macros
                            (concat
                             "\\(?:" (regexp-opt multi-char-macros) "\\)\\>"))
                          (when single-char-macros
                            (concat
                             (when multi-char-macros "\\|")
                             "\\(?:" (regexp-opt single-char-macros) "\\)"))
                          "\\)")))))
            defs)

      ;; defcustom font-latex-match-*-keywords
      (push `(defcustom ,(intern (concat prefix name "-keywords")) nil
               ,(concat "List of keywords "
                        (when (eq type 'command) "and formats ")
                        "for " name " face.\n"
                        (if (eq type 'command)
                            "\
Each element has to be a list consisting of the name of a macro
omitting the leading backslash and a format specifier as
described in the doc string of `font-latex-user-keyword-classes'."
                          "\
Each element has to be the name of a macro as a string, omitting
the leading backslash.")
                        "\n\n\
Setting this variable directly does not take effect; restart
Emacs.

Generated by `font-latex-make-built-in-keywords'.")
               :type '(repeat ,(if (eq type 'command)
                                   '(list (string :tag "Keyword")
                                          (string :tag "Format"))
                                 '(string :tag "Keyword")))
               :set (lambda (symbol value)
                      (set-default symbol value)
                      (funcall ',(intern (concat prefix name "-make"))))
               :group 'font-latex-keywords)
            defs)

      ;; Now that font-latex-match-* has attained proper default
      ;; value, make it buffer local.
      (push `(make-variable-buffer-local ',(intern (concat prefix name)))
            defs)

      ;; defun font-latex-match-*
      (push (font-latex--make-match-defun prefix name face type) defs)

      ;; Add matchers and highlighters to `font-latex-keywords-{1,2}'.
      (let ((keywords-entry (font-latex-keyword-matcher
                             prefix name face type)))
        (push (cons level keywords-entry) flks))))
    `(progn
       ,@(nreverse defs)
       (defvar font-latex-keywords-1
         ',(nreverse (delq nil (mapcar (lambda (x) (if (eq 1 (car x)) (cdr x)))
                                       flks)))
         "Subdued level highlighting for LaTeX modes.")
       (defvar font-latex-keywords-2
         ',(nreverse (mapcar #'cdr flks))
         "High level highlighting for LaTeX modes."))))

(font-latex-make-built-in-keywords)

(defcustom font-latex-user-keyword-classes nil
  "List of user-defined keyword classes for font locking.

Every keyword class consists of four parts, a name, a list of
keywords, a face and a specifier for the type of macro to be
highlighted.

When adding new entries, you have to use unique values for the
class names, that is, they must not clash with names of the
built-in keyword classes or other names given by you.
Additionally the names must not contain spaces.

The list of keywords defines which commands and declarations
should be covered by the keyword class.  A keyword can either be
a simple command name omitting the leading backslash or a list
consisting of the command name and a string specifying the syntax
of the command.  The latter is useful if you want to match LaTeX
macros with arguments (see below).  You can specify the occurence
and order of optional (\"[\") and mandatory (\"{\") arguments for
each keyword.  For example for \"documentclass\" you'd use \"[{\"
because the macro has one optional followed by one mandatory
argument.  Optionally starred macros can be indicated with \"*\".
In case an argument is an unbraced macro, use \"\\\".  You can
also specify two alternative arguments by prefixing them with
\"|\".  As an example, the specifier for \\newcommand is
\"*|{\\=\\[[{\".

The face argument can either be an existing face or a face
attribute.

There are three alternatives for the class type:

A value of `command' indicates commands with arguments
\(\"\\foo[bar]{baz}\").  The mandatory arguments in curly braces
will get the face you specified.

A value of `declaration' indicates declarations inside of TeX
groups (\"{\\foo bar}\").  The content inside the braces,
excluding the command, will get the face you specified.  In case
the braces are missing, the face will be applied to the command
itself.

A value of `noarg' indicates commands without arguments
\(\"\\foo\").  The command itself will get the face you
specified.

Setting this variable directly does not take effect;
restart Emacs."
  :group 'font-latex-keywords
  :type '(repeat (list (string :tag "Name")
                       (choice (repeat :tag "Keywords" (string :tag "Keyword"))
                               (repeat
                                :tag "Keywords with specs"
                                (group (string :tag "Keyword")
                                       (string :tag "Format specifier"))))
                       (choice (face :tag "Face name")
                                 (custom-face-edit :tag "Face attributes"))
                       (choice :tag "Type"
                               ;; Maps to
                               ;;`font-latex-match-command-with-arguments'
                               (const :tag "Command with arguments"
                                      command)
                               ;; Maps to
                               ;;`font-latex-match-command-in-braces'
                               (const :tag "Declaration inside TeX group"
                                      declaration)
                               ;; Maps to `re-search-forward'
                               (const :tag "Command without arguments"
                                      noarg))))
  :set (lambda (symbol value)
         (dolist (item value)
           (when (string-match " " (car item))
             (error "No spaces allowed in name")))
         (let (names names-uniq)
           (dolist (item (append font-latex-built-in-keyword-classes value))
             (setq names (append names (list (car item)))))
           (setq names (TeX-sort-strings names))
           (setq names-uniq (TeX-delete-duplicate-strings names))
           (dotimes (i (safe-length names-uniq))
             (unless (string= (nth i names) (nth i names-uniq))
               (error "Name %S already exists" (nth i names)))))
         (set-default symbol value)
         (let ((prefix "font-latex-match-"))
           (dolist (elt value)
             (unless (boundp (intern (concat prefix (car elt))))
               ;; defvar font-latex-match-*
               (eval `(defvar ,(intern (concat prefix (car elt))) nil
                        ,(concat "Regular expression to match " (car elt)
                                 " keywords.

Generated by `font-latex-user-keyword-classes'"))))
             (let ((keywords (nth 1 elt))
                   single-char-macro-flag)
               (setq keywords (if (listp (car keywords))
                                  (mapcar #'car keywords)
                                keywords))
               (catch 'single-char
                 (dolist (keyword keywords)
                   (unless (string-match "^[A-Za-z]" keyword)
                     (setq single-char-macro-flag t)
                     (throw 'single-char nil))))
               (set (intern (concat prefix (car elt)))
                    (when (> (safe-length keywords) 0)
                    (concat "\\\\" (regexp-opt keywords t)
                            (unless single-char-macro-flag "\\>")))))))))

(defun font-latex-make-user-keywords ()
  "Build defuns and defvars for user keyword fontification."
  (let ((keyword-specs font-latex-user-keyword-classes))
    (dolist (item keyword-specs)
      (let ((prefix "font-latex-match-")
            (name (nth 0 item))
            (keywords (nth 1 item))
            (face (nth 2 item))
            (type (nth 3 item)))

        ;; defvar font-latex-match-*-keywords
        (eval `(defvar ,(intern (concat prefix name "-keywords")) ',keywords
                 ,(concat "Font-latex keywords for " name " face.

Generated by `font-latex-make-user-keywords'.")))

        ;; defun font-latex-match-*
        (eval (font-latex--make-match-defun prefix name face type) t)

        ;; Add the matcher to `font-latex-keywords-2'.
        (add-to-list 'font-latex-keywords-2
                     (font-latex-keyword-matcher prefix name face type) t))))

  ;; Add the "fixed" matchers and highlighters.
  (dolist (item
           '(("\\(^\\|[^\\]\\)\\(&+\\)" 2 'font-latex-warning-face)
             (font-latex-match-dollar-math 0 'font-latex-math-face keep)
             (font-latex-match-quotation
              (0 'font-latex-string-face append)
              (1 'font-latex-warning-face))
             ;; Hack to remove the verbatim face from the \ in
             ;; \end{verbatim} and similar.  The same hack is used in
             ;; tex-mode.el.
             ("\\(\\\\\\)end"
              (1 (get-text-property (match-end 1) 'face) t))))
    (add-to-list 'font-latex-keywords-1 item)
    (add-to-list 'font-latex-keywords-2 item))
  (dolist (item
           '((font-latex-match-math-env
              (0 'font-latex-warning-face t t)
              (1 'font-latex-math-face append t))
             (font-latex-match-math-envII
              (1 'font-latex-math-face append t))
             (font-latex-match-simple-command
              (0 'font-latex-sedate-face append))
             (font-latex-match-script
              (1 (font-latex-script (match-beginning 0)) append))
             (font-latex-match-script-chars
              (1 (font-latex-script-char (match-beginning 1)) prepend))))
    (add-to-list 'font-latex-keywords-2 item t)))
(font-latex-make-user-keywords)

(defun font-latex-add-keywords (keywords class)
  "Add KEYWORDS to CLASS.
KEYWORDS is a list of keywords or keywords with syntax specs.
CLASS corresponds to a keyword class and can be one of the
symbols `warning', `variable', `reference', `biblatexnoarg',
`biblatex', `function', `function-noarg', `sectioning-1',
`sectioning-2', `sectioning-3', `sectioning-4', `sectioning-5',
`slide-title', `textual', `bold-command', `italic-command',
`underline-command', `math-command', `type-command',
`bold-declaration', `italic-declaration' or `type-declaration'.

The keywords will be added to the buffer-local list of keywords
of the respective keyword class and necessary updates of the font
locking machinery will be triggered."
  (let* ((class (symbol-name class))
         (list (intern (format "font-latex-match-%s-keywords-local" class))))
    (dolist (elt keywords)
      (add-to-list list elt))
    (funcall (intern (format "font-latex-match-%s-make" class)))
    ;; Trigger refontification.
    (when (fboundp 'font-lock-flush)
      (font-lock-flush))))

(defvar font-latex-keywords font-latex-keywords-1
  "Default expressions to highlight in TeX mode.")


;;; Subscript and superscript

(defcustom font-latex-fontify-script t
  "If non-nil, fontify subscript and superscript strings.

By default, super/subscripts are raised/lowered if this variable
is non-nil.  This fontification only affects one level of
scripts, for example in x^{y^z}, the y and the z have the same
size and are equally raised over x.

If this variable is set to the symbol `multi-level', then y is
raised above x, and z is raised above y.  With many script
levels, the text might become too small to be readable, thus
there is the option `font-latex-fontify-script-max-level'.  (The
factors for text shrinking are defined in the faces
`font-latex-superscript-face' and `font-latex-subscript-face' and
the raise/lower factor in `font-latex-script-display'.)

If this variable is set to the symbol `invisible', then the
effect is essentially like `multi-level' but additionally the
script operators ^ and _ are not displayed."
  :type '(choice (boolean :tag "Enabled")
                 (const :tag "Multiple levels" multi-level)
                 (const :tag "Hide ^ and _" invisible))
  :group 'font-latex
  :safe (lambda (val)
          (or (booleanp val)
              (memq val '(multi-level invisible)))))

(defcustom font-latex-fontify-script-max-level 3
  "Maximum scriptification level for which script faces are applied.
The faces `font-latex-superscript-face' and
`font-latex-subscript-face' define custom :height values < 1.0.
Therefore, scripts are displayed with a slightly smaller font
than normal math text.  If `font-latex-fontify-script' is
`multi-level' or `invisible', the font size becomes too small to
be readable after a few levels.  This option allows to specify
the maximum level after which the size of the script text won't
be shrunken anymore.

For example, see this expression:

  \\( x^{y^{z^a_b}} \\)

x has scriptification level 0, y has level 1, z has level 2, and
both a and b have scriptification level 3.

If `font-latex-fontify-script-max-level' was 2, then z, a, and b
would have the same font size.  If it was 3 or more, then a and b
were smaller than z just in the same way as z is smaller than y
and y is smaller than x."
  :group 'font-latex
  :type 'integer)

(defcustom font-latex-script-display '((raise -0.5) . (raise 0.5))
  "Display specification for subscript and superscript content.
The car is used for subscript, the cdr is used for superscripts."
  :group 'font-latex
  :type '(cons (choice (sexp :tag "Subscript form")
                       (const :tag "No lowering" nil))
               (choice (sexp :tag "Superscript form")
                       (const :tag "No raising" nil))))


;;; Syntactic keywords

(defvar-local font-latex-syntactic-keywords nil
  "Syntactic keywords used by `font-latex'.")

(defvar-local font-latex-syntactic-keywords-extra nil
  "List of syntactic keywords to add to `font-latex-syntactic-keywords'.
The form should be the same as in `font-lock-syntactic-keywords'.")

;; Set and updated in `font-latex-set-syntactic-keywords'.
(defvar font-latex-doctex-syntactic-keywords nil)

(defun font-latex-set-syntactic-keywords ()
  "Set the variable `font-latex-syntactic-keywords'.
This function can be used to refresh the variable in case other
variables influencing its value, like `LaTeX-verbatim-environments',
have changed."
  ;; Checks for non-emptiness of lists added in order to cater for
  ;; installations where `(regexp-opt-group nil)' would enter a loop.
  (let ((verb-envs (and (fboundp 'LaTeX-verbatim-environments)
                        (LaTeX-verbatim-environments)))
        (verb-macros-with-delims
         (and (fboundp 'LaTeX-verbatim-macros-with-delims)
              (LaTeX-verbatim-macros-with-delims)))
        (verb-macros-with-braces
         (and (fboundp 'LaTeX-verbatim-macros-with-braces)
              (LaTeX-verbatim-macros-with-braces))))
    (setq verb-envs (and verb-envs (regexp-opt verb-envs))
          verb-macros-with-delims (and verb-macros-with-delims
                                       (regexp-opt verb-macros-with-delims))
          verb-macros-with-braces (and verb-macros-with-braces
                                       (regexp-opt verb-macros-with-braces))
          font-latex-syntactic-keywords nil)
    (unless (= (length verb-envs) 0)
      (add-to-list
       'font-latex-syntactic-keywords
       `(,(concat
           "^[ \t]*\\\\begin *{\\(?:" verb-envs "\\)}"
           ;; Some environments accept an optional and/or mandatory
           ;; argument that can span over more lines.  Between
           ;; "\begin{<envname>}" and the optional argument there can
           ;; be whitespaces and the newline can be commented by a "%"
           ;; character.
           "[ \t]*\\(?:%.*\n[ \t]*\\)?"
           ;; The following line of the regexp matches the optional
           ;; argument and allows for up to one level of brackets
           ;; inside the argument (e.g., the dialect of a language in
           ;; the `lstlisting' environment by the `listings' package).
           "\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)?"
           ;; After the optional argument, there may also be another
           ;; mandatory argument(s) (e.g. with VerbatimOut or the
           ;; minted envs or defined with `lstnewenvironment').  Use
           ;; the same trick as above in order to allow one level of
           ;; braces in the argument.
           "\\(?:{[^{}]*\\(?:{[^{}]*}[^{}]*\\)*}\\)*"
           ;; Now match the final newline.  The "." alternative
           ;; catches the case where verbatim content is written
           ;; immediately after the \begin{verbatim}.
           "\\(\n\\|.\\)")
         (1 "|" t)))
      (add-to-list
       'font-latex-syntactic-keywords
       ;; Using the newline character for the syntax property often
       ;; resulted in fontification problems when text was inserted at
       ;; the end of the verbatim environment.  That's why we now use
       ;; the starting backslash of \end.  There is a hack in
       ;; `font-latex-make-user-keywords' to remove the spurious
       ;; fontification of the backslash.
       `(,(concat "\\(\\\\\\)end *{\\(?:" verb-envs "\\)}")
         (1 "|" t))))
    (unless (= (length verb-macros-with-delims) 0)
      (add-to-list
       'font-latex-syntactic-keywords
       `(,(concat "\\\\\\(?:" verb-macros-with-delims "\\)"
                  ;; Some macros take an optional argument.  This is
                  ;; the same line as above for environments.
                  "\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)?"
                  ;; An opening curly brace as delimiter is valid, but
                  ;; allowing it might screw up fontification of stuff
                  ;; like "\url{...} foo \textbf{<--!...}".  Also
                  ;; disallow an opening square bracket which produces
                  ;; confusion in "\Verb[key-val]{foo[<--!}"
                  "\\([^a-z@*\n\f{[]\\).*?"
                  ;; Give an escape char at the end of the verbatim
                  ;; construct punctuation syntax.  Prevents wrong
                  ;; fontification of stuff like "\verb|foo\|".
                  "\\(" (regexp-quote TeX-esc) "*\\)\\(\\1\\)")
         (1 "\"") (2 ".") (3 "\""))))
    (unless (= (length verb-macros-with-braces) 0)
      (add-to-list
       'font-latex-syntactic-keywords
       `(,(concat "\\\\\\(?:" verb-macros-with-braces "\\)"
                  ;; Some macros take an optional argument.  This is
                  ;; the same line as above for environments.
                  "\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)?"
                  ;; Within verb macros with braces, only balanced
                  ;; pairs of braces are allowed; so we respect this
                  ;; and allow one level of balanced braces.  Give
                  ;; escape char(s) at the end of the verbatim
                  ;; construct punctuation syntax.
                  "\\({\\)[^}{]*?"
                  "\\(?:{[^}{]*}[^}{]*?\\)*"
                  "\\(" (regexp-quote TeX-esc) "*\\)"
                  "\\(}\\)")
         (1 "|") (2 ".") (3 "|")))))
  (when font-latex-syntactic-keywords-extra
    (nconc font-latex-syntactic-keywords font-latex-syntactic-keywords-extra))
  ;; ;; Cater for docTeX mode.
  ;; (setq font-latex-doctex-syntactic-keywords
  ;;       (append font-latex-syntactic-keywords
  ;;               ;; For docTeX comment-in-doc.
  ;;               '(("\\(\\^\\)\\^A" (1 (font-latex-doctex-^^A))))))
  ;; Finally, compute our `syntax-propertize-function' anew.
  (setq-local syntax-propertize-function
              (font-latex--make-syntax-propertize-function)))


;;; Syntactic fontification

(defun font-latex-syntactic-face-function (state)
  (if (nth 3 state)
      'font-latex-verbatim-face
    'font-lock-comment-face))

;;; Faces

(defface font-latex-bold-face
  (let ((font '(:inherit bold)))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab2" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-latex-highlighting-faces)

(defface font-latex-italic-face
  (let ((font '(:inherit italic)))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab2" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-latex-highlighting-faces)

(defface font-latex-underline-face
  (let ((font '(:inherit underline)))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab2" ,@font))
      (t (,@font))))
  "Face used to highlight text to be underlined."
  :group 'font-latex-highlighting-faces)

(defface font-latex-math-face
  (let ((font '(:inherit underline)))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-latex-highlighting-faces)

(defface font-latex-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-latex-highlighting-faces)

(defface font-latex-string-face
  (let ((font '(:inherit italic)))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown4"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-latex-highlighting-faces)

(defface font-latex-warning-face
  (let ((font '(:inherit bold)))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-latex-highlighting-faces)

(defface font-latex-verbatim-face
  (let ((font '(:inherit fixed-pitch)))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown" ,@font))
      (((class color) (background dark))
       (:foreground "burlywood" ,@font))
      (t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-latex-highlighting-faces)

(defface font-latex-superscript-face
  '((t (:height 0.85)))
  "Face used for superscripts."
  :group 'font-latex-highlighting-faces)

(defface font-latex-subscript-face
  '((t (:height 0.85)))
  "Face used for subscripts."
  :group 'font-latex-highlighting-faces)

(defface font-latex-script-char-face
  (let ((font '(:inherit underline)))
    `((((class grayscale) (background light))
       (:foreground "gray25" ,@font))
      (((class grayscale) (background dark))
       (:foreground "gray" ,@font))
      (((class color) (background light))
       (:foreground "DarkRed"))
      (((class color) (background dark))
       (:foreground "salmon"))
      (t (,@font))))
  "Face used for the script chars ^ and _."
  :group 'font-latex-highlighting-faces)

(defface font-latex-slide-title-face
  '((t (:inherit (variable-pitch font-lock-type-face)
                   :weight bold :height 1.2)))
  "Face for slide titles."
  :group 'font-latex-highlighting-faces)

;;; Setup

(defvar font-latex-syntax-alist
  ;; Use word syntax for @ because we use \> for matching macros and
  ;; we don't want \foo@bar to be found if we search for \foo.
  '((?\( . ".") (?\) . ".") (?@ . "w"))
  "List of specifiers for the syntax alist of `font-lock-defaults'.")

(defun font-latex-add-to-syntax-alist (list)
  "Activate syntactic font locking for the entries in LIST.
The entries are added to `font-latex-syntax-alist' and eventually
end up in `font-lock-defaults'.  Each entry in LIST should be a
cons pair as expected by `font-lock-defaults'.  The function also
triggers Font Lock to recognize the change."
  (setq-local font-latex-syntax-alist
              (append font-latex-syntax-alist list))
  ;; We modify the `font-lock-syntax-table' directly but also call
  ;; `font-latex-setup' in order to have `font-lock-defaults' be in sync.
  (font-latex-setup)
  (dolist (elt list)
    (modify-syntax-entry (car elt) (cdr elt) font-lock-syntax-table))
  ;; Trigger refontification.
  (when (fboundp 'font-lock-flush)
    (font-lock-flush)))

(defun font-latex--make-syntax-propertize-function ()
  "Return a `syntax-propertize-function' for (La|Doc)TeX documents."
  (let* ((kws ;; (if (derived-mode-p 'docTeX-mode)
              ;;     font-latex-doctex-syntactic-keywords
               font-latex-syntactic-keywords) ;; )
         (func (syntax-propertize-via-font-lock kws)))
    (lambda (start end)
      ;; Initialize font lock variables even when font lock is disabled.
      ;; This treatment is necessary because syntax propertize depends
      ;; on font lock facility.  (bug#71164)
      (or font-lock-set-defaults (font-lock-set-defaults))
      (funcall func start end))))

;;;###autoload
(defun font-latex-setup ()
  "Setup this buffer for LaTeX font-lock.  Usually called from a hook."
  (font-latex-set-syntactic-keywords)

  ;; Activate multi-line fontification facilities.
  (setq-local font-lock-multiline t)

  ;; The test for `major-mode' currently only works with docTeX mode
  ;; because `TeX-install-font-lock' is called explicitly in
  ;; `docTeX-mode'.  In case other modes have to be distinguished as
  ;; well, remove the call to `TeX-install-font-lock' from `TeX-mode'
  ;; and place it in the different `xxx-mode' calls instead, but
  ;; _after_ `major-mode' is set.
  (let ((defaults
         `((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
           nil nil ,font-latex-syntax-alist nil))
        (variables
         '((font-lock-mark-block-function . mark-paragraph)
           (font-lock-fontify-region-function
            . font-latex-fontify-region)
           (font-lock-unfontify-region-function
            . font-latex-unfontify-region)
           (font-lock-extend-region-functions
            font-lock-extend-region-wholelines
            font-lock-extend-region-multiline
            font-latex-extend-region-backwards-command-with-args
            font-latex-extend-region-backwards-command-in-braces
            font-latex-extend-region-backwards-quotation
            font-latex-extend-region-backwards-math)
           (syntax-propertize-extend-region-functions
            syntax-propertize-wholelines
            font-latex-sp-extend-region-backwards-verb-env))))
    ;; Add the mode-dependent stuff to the basic variables defined above.
    (if (eq major-mode 'docTeX-mode)
        (progn
          (setcar defaults (append (car defaults)
                                   '(font-latex-doctex-keywords)))
          (setq variables
                (append variables
                        '((font-lock-syntactic-face-function
                           . font-latex-doctex-syntactic-face-function)))))
      (setq variables
            (append variables
                    '((font-lock-syntactic-face-function
                       . font-latex-syntactic-face-function)))))
    ;; Set the defaults.
    (setq font-lock-defaults (append defaults variables)))

  ;; Make sure fontification will be refreshed if a user sets variables
  ;; influencing fontification in her file-local variables section.
  (add-hook 'hack-local-variables-hook #'font-latex-after-hacking-local-variables t t)

  ;; We may be using the mode programmatically to extract data, and we
  ;; then need this to be set up first so that a command like
  ;; `xref-find-references' doesn't bug out when matching hits in
  ;; files that Emacs isn't visiting. (bug#65912)
  ;; We need this treatment because the current syntax propertize
  ;; facility depends on font lock machinery.  We can remove this
  ;; when syntax propertization decouples font lock.
  (unless buffer-file-truename
    (font-lock-set-defaults)))

(defun font-latex-update-font-lock (&optional _syntactic-kws)
  "Tell font-lock about updates of fontification rules.
If SYNTACTIC-KWS is non-nil, also update
`font-latex-syntactic-keywords'."
  (display-warning
   'auctex
   (concat "`font-latex-update-font-lock' should not be called.
It is obsolete and going to be removed.
If you have called `font-latex-add-keywords' and want to refresh fontification,
call `font-lock-flush' instead.
If you changed syntactic fontification, for example, one of the variables
- `LaTeX-verbatim-macros-with-delims'
- `LaTeX-verbatim-macros-with-delims-local'
- `LaTeX-verbatim-macros-with-braces'
- `LaTeX-verbatim-macros-with-braces-local'
- `LaTeX-verbatim-environments'
- `LaTeX-verbatim-environments-local'
- `font-latex-syntactic-keywords-extra'
then call `font-latex-set-syntactic-keywords'.")))

(make-obsolete 'font-latex-update-font-lock nil "12.2.4")

(defvar font-latex--updated-region-end nil
;; During font lock operation, matched range sometimes exceeds the
;; given end limit.  So record the actual end in this variable to
;; notify the font lock machinery.
;; Match functions should do the following two if the end of the
;; actual match goes beyond the limit:
;; 1. If the value of this variable is smaller than limit, set this
;;    variable to that limit.
;; 2. When the end of the actual match exceeds this variable,
;;    - apply `font-lock-unfontify-region' between the value of this
;;      variable and the end of the actual match
;;    - update this variable to the end of the actual match
;; See implementation of `font-latex-match-math-env' for actual usage.
  "Record the end of fontification.")
(defun font-latex-fontify-region (beg end &optional verbose)
  "Fontify region from BEG to END.
Take care when the actually fonfified region was extended beyond END."
  (setq font-latex--updated-region-end end)
  (let ((res (font-lock-default-fontify-region beg end verbose)))
    `(jit-lock-bounds ,(cadr res) .
                      ,(max (cddr res) font-latex--updated-region-end))))

;; Copy and adaption of `tex-font-lock-unfontify-region' from
;; tex-mode.el in GNU Emacs on 2004-08-04.
;; (XEmacs passes a third argument to the function.)
(defun font-latex-unfontify-region (beg end &rest _ignored)
  "Unfontify region from BEG to END."
  (font-lock-default-unfontify-region beg end)
  (remove-list-of-text-properties beg end '(script-level invisible))
  (while (< beg end)
    (let ((next (next-single-property-change beg 'display nil end))
          (prop (get-text-property beg 'display)))
      (if (and (eq (car-safe prop) 'raise)
               (null (cddr prop)))
          (put-text-property beg next 'display nil))
      (setq beg next))))

(defun font-latex-after-hacking-local-variables ()
  "Refresh fontification if required by updates of file-local variables.
This function is added to `hack-local-variables-hook' and
recomputes fontification if variables affecting fontification are
modified.  Such variables include
`LaTeX-verbatim-environments-local',
`LaTeX-verbatim-macros-with-braces-local',
`LaTeX-verbatim-macros-with-delims-local'."
  (when
      ;; In Emacs we know if the value came from file or directory
      ;; locals.  Note to self: directory-local variables are also added
      ;; to file-local-variables-alist.
      (let ((hacked-local-vars (mapcar #'car file-local-variables-alist)))
        (or (memq 'LaTeX-verbatim-environments-local hacked-local-vars)
            (memq 'LaTeX-verbatim-macros-with-braces-local hacked-local-vars)
            (memq 'LaTeX-verbatim-macros-with-delims-local hacked-local-vars)))
    ;; Ok, we need to refresh syntactic fontification.
    (font-latex-set-syntactic-keywords)))

;;; Utility functions

(defun font-latex-find-matching-close (openchar closechar)
  "Skip over matching pairs of OPENCHAR and CLOSECHAR.
OPENCHAR is the opening character and CLOSECHAR is the closing
character.  Character pairs are usually { } or [ ].  Comments are
ignored during the search."
  (let ((parse-sexp-ignore-comments
         (not (eq major-mode 'docTeX-mode))) ; scan-sexps ignores comments
        (init-point (point))
        (mycount 1)
        (esc-char (or (and (boundp 'TeX-esc) TeX-esc) "\\"))
        ;; XXX: Do not look up syntax-table properties since they may
        ;; be misleading, e.g. in the case of "{foo}^^A" where the
        ;; closing brace gets a comment end syntax.
        ;; (2022 Mar) The latter half of the above paragraph no longer
        ;; applies since we changed the way to fontify ^^A comment.
        (parse-sexp-lookup-properties nil)
        (syntax (TeX-search-syntax-table openchar closechar)))
    (or
     (condition-case nil
         (progn
           ;; It is possible to have an opt. arg like \foo[key={]}].
           ;; Since braces are always balanced in opt. arguments, we
           ;; change the syntax to "generic comment delimiter".  For the
           ;; backslash, we switch to "/" in order to ignore things like
           ;; \{ and \}:
           (unless (and (= openchar ?\{) (= closechar ?\}))
             (modify-syntax-entry ?\{ "|" syntax)
             (modify-syntax-entry ?\} "|" syntax)
             (modify-syntax-entry ?\\ "/" syntax))
           (goto-char (with-syntax-table syntax
                        (scan-sexps (point) 1)))
           ;; No error code.  See if closechar is unquoted
           (save-excursion
             (backward-char 1)
             (zerop (mod (skip-chars-backward (regexp-quote esc-char)) 2))))
       (error nil))
     (save-match-data
       (goto-char (1+ init-point))
       (while (and (> mycount 0)
                   (re-search-forward
                    (string ?\[
                            ;; closechar might be ]
                            ;; and therefor must be first in regexp
                            closechar openchar
                            ?\])
                    nil t))
         (cond
          ((font-latex-commented-outp)
           (forward-line 1))
          ((save-excursion
             (backward-char 1)
             (zerop (mod (skip-chars-backward (regexp-quote esc-char))
                         2)))
           (setq mycount (+ mycount
                            (if (= (preceding-char) openchar) 1 -1)))))))
     (if (= mycount 0)
         t
       (goto-char init-point)
       nil))))

(defun font-latex-commented-outp ()
  "Return t if comment character is found between bol and point."
  (save-excursion
    (let ((limit (point))
          (esc-char (if (and (boundp 'TeX-esc) TeX-esc) TeX-esc "\\")))
      (forward-line 0)
      (if (and (eq (char-after) ?\%)
               (not (font-latex-faces-present-p 'font-latex-verbatim-face)))
          (not (eq major-mode 'docTeX-mode))
        (catch 'found
          (while (progn (skip-chars-forward "^%" limit)
                        (< (point) limit))
            (when (and (save-excursion
                         (zerop (mod (skip-chars-backward
                                      (regexp-quote esc-char)) 2)))
                       (not (font-latex-faces-present-p
                             'font-latex-verbatim-face)))
              (throw 'found t))
            (forward-char)))))))

(defun font-latex-faces-present-p (faces &optional pos)
  "Return t if FACES are present at position POS.
FACES may be a single face or a list of faces.
If POS is omitted, the current position of point is used."
  (let* ((faces (if (listp faces) faces (list faces)))
         (pos (or pos (point)))
         (prop (get-text-property pos 'face))
         (prop-list (if (listp prop) prop (list prop))))
    (catch 'member
      (dolist (item prop-list)
        (when (memq item faces)
          (throw 'member t))))))

(defun font-latex-forward-comment ()
  "Like `forward-comment' but with special provisions for docTeX mode.
In docTeX mode \"%\" at the start of a line will be treated as whitespace."
  (if (eq major-mode 'docTeX-mode)
      ;; XXX: We should probably cater for ^^A as well.
      (progn
        (while (progn (if (bolp) (skip-chars-forward "%"))
                      (> (skip-chars-forward " \t\n") 0)))
        (when (eq (char-after) ?%)
          (beginning-of-line 2)
          t))
    (forward-comment 1)))

;;; Match functions

(defvar font-latex-matched-faces nil
  "List of faces corresponding to matches in match data.")

(defun font-latex-matched-face (pos)
  "Return face at position POS in `font-latex-matched-faces'."
  (nth pos font-latex-matched-faces))

(defvar font-latex-command-with-args-default-spec nil ; "*[{"
  "Default specifier for keywords without syntax description.
Set this to nil if verification of command syntax is unwanted.")

(defvar font-latex-command-with-args-opt-arg-delims
  '((?\[ . ?\]) (?< . ?>) (?\( . ?\)))
  "List character pairs used as delimiters for optional arguments.")

(defvar font-latex-syntax-error-modes '(LaTeX-mode)
  "List of modes where syntax errors in macros should be indicated.")

(defun font-latex-match-command-with-arguments (regexp keywords face limit)
  "Search for regexp command KEYWORDS[opt]{arg} before LIMIT.
Returns nil if none of KEYWORDS is found."
  (setq font-latex-matched-faces nil)
  (catch 'match
    (while (re-search-forward regexp limit t)
      (unless (font-latex-faces-present-p '(font-lock-comment-face
                                            font-latex-verbatim-face)
                                          (match-beginning 0))
        (let* ((beg (match-beginning 0))
               end                 ; Used for multiline text property.
               (match-data (list beg))
               match-beg syntax-error alternative spec
               error-indicator-pos
               (spec-list (string-to-list
                           (or (cadr (assoc (match-string 1) keywords))
                               font-latex-command-with-args-default-spec)))
               (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
          (goto-char (match-end 0))
          ;; Check for starred macro if first spec is an asterisk or a
          ;; plus sign in case of \defaultfontfeatures+ provided by
          ;; fontspec.sty
          (when (or (eq (car spec-list) ?*)
                    (eq (car spec-list) ?+))
            (setq spec-list (cdr spec-list))
            (skip-chars-forward "*+" (1+ (point))))
          ;; Add current point to match data and use keyword face for
          ;; region from start to point.
          (nconc match-data (list (point)))
          (add-to-list 'font-latex-matched-faces 'font-lock-keyword-face)
          (setq end (point))
          (catch 'break
            ;; Walk the list of specs.
            (while spec-list
              (setq spec (pop spec-list)
                    error-indicator-pos beg)
              (while (and (not (eobp)) (font-latex-forward-comment)))
              ;; Alternative
              (when (eq spec ?|)
                (setq alternative t)
                (setq spec (pop spec-list)))
              (cond
               ;; Macros: \foo
               ((eq spec ?\\)
                (if (eq (char-after) spec)
                    (progn
                      (nconc match-data
                             (list (point)
                                   (progn
                                     (forward-char)
                                     (if (zerop (skip-syntax-forward "_w"))
                                         (forward-char) ; Single-char macro.
                                       (skip-chars-forward "*+"))
                                     (point))))
                      (nconc font-latex-matched-faces (list face))
                      (setq end (max end (point)))
                      (when alternative (pop spec-list)))
                  (setq syntax-error t)
                  (throw 'break nil)))
               ;; Mandatory arguments: {...}
               ((eq spec ?{)
                (if (and (eq (char-after) spec)
                         (setq match-beg (point))
                         (font-latex-find-matching-close ?{ ?}))
                    (progn
                      (nconc match-data (list (1+ match-beg) (1- (point))))
                      (nconc font-latex-matched-faces (list face))
                      (setq end (max end (1- (point))))
                      (when alternative (pop spec-list)))
                  (unless alternative
                    (setq syntax-error t)
                    (when (and match-beg (= match-beg (point)))
                      (setq error-indicator-pos match-beg))
                    (throw 'break nil))))
               ;; Optional arguments: [...] and others
               ((eq (char-after) spec)
                (setq match-beg (point))
                (if (font-latex-find-matching-close
                     spec (cdr (assq
                                spec
                                font-latex-command-with-args-opt-arg-delims)))
                    (progn
                      (nconc match-data (list (1+ match-beg) (1- (point))))
                      (nconc font-latex-matched-faces
                             (list 'font-lock-variable-name-face))
                      (setq end (max end (1- (point)))))
                  (setq syntax-error t
                        error-indicator-pos match-beg)
                  (throw 'break nil))))
              (setq alternative nil)))
          (when (and syntax-error (memq major-mode
                                        font-latex-syntax-error-modes))
            ;; Add the warning face at the front of the list because
            ;; the matcher uses 'append and the face would otherwise
            ;; be overridden by the keyword face.
            (setq match-data (append (list error-indicator-pos
                                           (1+ error-indicator-pos))
                                     match-data))
            (push 'font-latex-warning-face font-latex-matched-faces))
          (store-match-data match-data)
          (throw 'match t))))))

;; Those are dynamically bound by font-lock.
(defvar font-lock-beg)
(defvar font-lock-end)

(defun font-latex-extend-region-backwards-command-with-args ()
  "Extend region backwards for commands with args."
  (save-excursion
    (goto-char font-lock-end)
    (catch 'extend
      (while (TeX-search-backward-unescaped "}" font-lock-beg t)
        (let ((macro-start
               (TeX-find-macro-start
                (max (point-min)
                     (- font-lock-beg font-latex-multiline-boundary)))))
          (when (and macro-start
                     (< macro-start font-lock-beg))
            (setq font-lock-beg macro-start)
            (throw 'extend t)))))))

(defun font-latex-match-command-in-braces (keywords limit)
  "Search for command like {\\bfseries fubar} before LIMIT.
Sets `match-data' so that:
 subexpression 0 is a warning indicator,
 subexpression 1 is the keyword, and
 subexpression 2 is the rest in the TeX group.
Return nil if no command is found."
  (catch 'match
    (while (re-search-forward keywords limit t)
      (unless (font-latex-faces-present-p '(font-lock-comment-face
                                            font-latex-verbatim-face)
                                          (match-beginning 0))
        (let ((kbeg (match-beginning 0)) (kend (match-end 1))
              (beg  (match-end 0))
              cbeg cend
              (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
          (goto-char kbeg)
          (if (not (eq (preceding-char) ?\{))
              ;; Fontify only the keyword (no argument found).
              (progn
                (setq cbeg kbeg cend kend)
                (goto-char (match-end 0))
                (store-match-data (list (point) (point)
                                        (point) (point)
                                        cbeg cend))
                (throw 'match t))
            ;; There's an opening bracket
            (save-restriction
              ;; Restrict to LIMIT.
              (narrow-to-region (point-min) limit)
              (forward-char -1)         ; Move on the opening bracket
              (if (font-latex-find-matching-close ?\{ ?\})
                  (store-match-data (list kbeg kbeg
                                          kbeg kend
                                          beg (1- (point))))
                (goto-char kend)
                (store-match-data (list (1- kbeg) kbeg
                                        kbeg kend
                                        kend kend)))
              (throw 'match t))))))))

(defun font-latex-extend-region-backwards-command-in-braces ()
  "Extend region backwards for commands in braces."
  (save-excursion
    (goto-char font-lock-end)
    (catch 'extend
      (while (TeX-search-backward-unescaped "}" font-lock-beg t)
        (let ((group-start
               (TeX-find-opening-brace
                nil (max (point-min)
                         (- font-lock-beg font-latex-multiline-boundary)))))
          (when group-start
            ;; XXX: Actually we'd have to check if any of the
            ;; declaration-type macros can be found right after the
            ;; brace.  If we don't do this (like now) large regions
            ;; may be refontified for no good reason.  For checking
            ;; the built-in `font-latex-match-*' variables for
            ;; declaration-type macros as well as the respective
            ;; user-defined variables could be concatenated.
            (goto-char group-start)
            (when (< group-start font-lock-beg)
              (setq font-lock-beg group-start)
              (throw 'extend t))))))))

(defvar font-latex-match-simple-exclude-list
  '("-" "," "/" "&" "#" "_" "`" "'" "^" "~" "=" "." "\"")
  "List of characters directly after \"\\\" excluded from fontification.
Each character is a string.")

(defvar-local font-latex-match-simple-include-list '("@")
  "List of characters allowed in a macro for fontification.
Each character is a string.  This variable is initialized to
\"@\" since internal LaTeX commands are very often redefined in a
.tex file and the fontification should work correctly in those
cases.")

(defun font-latex-match-simple-command (limit)
  "Search for command like \\foo before LIMIT."
  ;; \s_ matches chars with symbol syntax, \sw chars with word syntax,
  ;; \s. chars with punctuation syntax.  We must exclude matches where
  ;; the first character after the \ is a reserved character and
  ;; should not be fontified (e.g. \, in foo\,bar or \- in foo\-bar).
  ;; These characters are stored in
  ;; `font-latex-match-simple-exclude-list'.  In docTeX mode, we
  ;; remove "_" from this list to get correct fontification for macros
  ;; like `\__module_foo:nnn'
  (let* ((search (lambda ()
                   (TeX-re-search-forward-unescaped
                    (concat
                     ;; Chars directly after backslash
                     "\\\\\\(\\s_\\|\\sw\\|\\s.\\)"
                     ;; Start group of the following chars
                     "\\(?:["
                     ;; a-zA-Z are always allowed:
                     "a-zA-Z"
                     ;; Additional characters added by AUCTeX styles
                     (mapconcat #'identity
                                font-latex-match-simple-include-list
                                "")
                     ;; End group
                     "]\\)*")
                    limit t)))
         (pos (funcall search)))
    (while (and pos
                (member (match-string 1)
                        (if (eq major-mode 'docTeX-mode)
                            (remove "_" font-latex-match-simple-exclude-list)
                          font-latex-match-simple-exclude-list)))
      (setq pos (funcall search)))
    pos))

(defun font-latex-match-math-env (limit)
  "Match math pattern up to LIMIT.
Used for patterns like:
\\( F = ma \\)
\\=\\[ F = ma \\] but not \\\\=\\[len]"
  (catch 'match
    (while (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
      (unless (save-excursion
                (goto-char (match-beginning 0))
                ;; \\[ does not start a math environment
                (/= (mod (skip-chars-backward "\\\\") 2) 0))
        (let ((beg (match-beginning 0))
              (open-tag (if (match-beginning 1) "\\(" "\\["))
              (close-tag (if (match-beginning 1) "\\)" "\\]")))
          ;; Search for both opening and closing tags in order to be
          ;; able to avoid erroneously matching stuff like "\(foo \(bar\)".
          (if (and (re-search-forward (concat "[^\\]\\(?:\\\\\\\\\\)*\\("
                                              (regexp-quote open-tag) "\\|"
                                              (regexp-quote close-tag) "\\)")
                                      (+ limit font-latex-multiline-boundary)
                                      'move)
                   (string= (match-string 1) close-tag))
              ;; Found closing tag.
              (let ((p (point)))
                (if (< font-latex--updated-region-end limit)
                    ;; *-extend-region-functions have extended the
                    ;; limit already.
                    (setq font-latex--updated-region-end limit))
                ;; If the closing tag is beyond the current end of
                ;; region, take care of it.
                (when (< font-latex--updated-region-end p)
                  ;; FIXME: Why?  Should this use `font-lock-flush'?
                  (font-lock-unfontify-region font-latex--updated-region-end p)
                  (setq font-latex--updated-region-end p))
                (store-match-data (list beg beg beg p)))
            ;; Did not find closing tag.
            (goto-char (+ beg 2))
            (store-match-data (list beg (point) (point) (point))))
          (throw 'match t))))))

(require 'texmathp)
(defcustom font-latex-math-environments nil
  "List of math environment names for font locking.
It is no longer recommended to customize this option.  You should
customize `texmathp-tex-commands' instead because it is important
for stable operation of font lock that this option is coherent
with that option in addition to `texmathp-tex-commands-default'.
See info node `(auctex)Fontification of math' to convert your
customization into `texmathp-tex-commands'."
  ;; This option is now used only through
  ;; `font-latex--match-math-envII-regexp'.
  :type '(repeat string)
  :group 'font-latex)

(defvar font-latex--match-math-envII-regexp nil
  "Regular expression to match math environments.
Set by `font-latex--update-math-env' and used in
`font-latex-match-math-envII'.")

(defun font-latex-update-math-env ()
  "Update regexp to search for math environments.
Extract environments marked as `env-on' in
`texmathp-tex-commands1' except starred variants.  Then build
`font-latex--match-math-envII-regexp' from them, appending the
environments in `font-latex-math-environments'."
  ;; Make sure `texmathp-tex-commands1' is up to date.
  (texmathp-compile)
  (let (envs)
    (dolist (entry texmathp-tex-commands1)
      (if (and (eq 'env-on (cadr entry))
               (not (string= "*" (substring (car entry) -1))))
          (cl-pushnew (car entry) envs :test #'equal)))
    (setq font-latex--match-math-envII-regexp
          (concat "\\\\begin[ \t]*{"
                  ;; Take user additions also into account.
                  (regexp-opt (append font-latex-math-environments envs) t)
                  ;; Subexpression 2 is used to build the \end{<env>}
                  ;; construct later.
                  "\\(\\*?}\\)"
                  ;; Match an optional and possible mandatory
                  ;; argument(s) as long as they are on the same line
                  ;; with no spaces in-between. The content of optional
                  ;; argument can span multiple lines.
                  "\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)?"
                  "\\(?:{[^}]*}\\)*"))))

;; Initialize.
(font-latex-update-math-env)

(defun font-latex-match-math-envII (limit)
  "Match math patterns up to LIMIT.
Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation} or
\\begin{empheq}[X=Y\\Rightarrow]{alignat=3}
 fontified stuff
\\end{empheq}
The \\begin{equation} incl. arguments in the same line and
\\end{equation} are not fontified here."
  (when (re-search-forward font-latex--match-math-envII-regexp limit t)
    (let ((beg (match-end 0)) end
          (beg-of-begin (match-beginning 0)))
      (if (re-search-forward (concat "\\\\end[ \t]*{"
                                     (regexp-quote
                                      (buffer-substring-no-properties
                                       (match-beginning 1)
                                       (match-end 2))))
                             (+ limit font-latex-multiline-boundary) 'move)
          (progn
            (setq end (match-beginning 0))
            ;; FIXME: Duplicate of code in `font-latex-match-math-env'.
            (if (< font-latex--updated-region-end limit)
                (setq font-latex--updated-region-end limit))
            (when (< font-latex--updated-region-end end)
              (font-lock-unfontify-region font-latex--updated-region-end end)
              (setq font-latex--updated-region-end end)))
        (goto-char beg)
        (setq end beg
              beg-of-begin beg))
      ;; Store the position of "\begin{foo}" as (match-beginnig 0) so
      ;; that `font-lock-multiline' text property covers it.  This
      ;; keeps editing inside multi-line optional argument sane.
      (store-match-data (list beg-of-begin end beg end))
      t)))

(defun font-latex-match-dollar-math (limit)
  "Match inline math $...$ or display math $$...$$ before LIMIT."
  (catch 'match
    (let (beg num)
      (while (font-latex-find-dollar-math limit)
        ;; Found "$" which starts $...$ or $$...$$.
        (setq beg (point)
              ;; Go inside the math expression.
              num (skip-chars-forward "$" limit))
        ;; If those are three or more consecutive $, ignore them and
        ;; search again.
        (if (< num 3)
            (if ;; Let's find the same number of live dollar signs.
                (font-latex-find-dollar-math
                 ;; Hope that limit+font-latex-multiline-boundary
                 ;; doesn't fall just inside single "$$".
                 (min (point-max) (+ limit font-latex-multiline-boundary))
                 num)
                ;; Found.
                (progn
                  (forward-char num)
                  (let ((p (point)))
                    ;; FIXME: Duplicate of code in `font-latex-match-math-env'.
                    (if (< font-latex--updated-region-end limit)
                        (setq font-latex--updated-region-end limit))
                    (when (< font-latex--updated-region-end p)
                      (font-lock-unfontify-region
                       font-latex--updated-region-end p)
                      (setq font-latex--updated-region-end p))
                    (set-match-data (list beg p)))
                  (throw 'match t))
              ;; Not found.
              ;; That $ or $$ is probably unclosed in the buffer.
              (throw 'match nil)))))))

(defun font-latex-find-dollar-math (limit &optional num)
  "Find dollar sign(s) before LIMIT.
Set point just before the found $.  Ignore escaped $ (\"\\$\").
Optional argument NUM, if non-nil, specifies the number of dollar
signs to follow the point and must be 1 or 2.
LIMIT must not exceed the end of buffer."
  (catch 'found
    (while (progn
             (skip-chars-forward "^$" limit)
             (< (point) limit))
      ;; Found "$".
      ;; If that "$" is not our target, skip over it and search
      ;; again.
      (cond
       ;; check 1: Are we in a verbatim construct or comment?
       ((let ((ppss (syntax-ppss)))
          (or (nth 3 ppss)
              ;; Ignore $ in comments...
              (and (nth 4 ppss)
                   ;; ... except if we're looking for the end of the
                   ;; inline math.  We need to consider this %$
                   ;; comments because they are the workaround for
                   ;; falsely triggered math mode due to valid,
                   ;; non-math occurrences of $.  (bug#48365)
                   (not num))))
        (skip-chars-forward "$" limit))
       ;; check 2: Else, is "$" escaped?
       ((TeX-escaped-p)
        (forward-char 1))
       ;; check 3: Else, is the number of the following "$" wrong?
       ;; This check cannot precede check 2 because "$1+2\$$" is
       ;; legal.
       ((and (eq num 2) (not (eq (char-after (1+ (point))) ?$)))
        ;; If double dollars ($$) are followed by $, skip over that $.
        ;; We need not care the case that single dollar ($) is
        ;; followed by $$ because expressions like "$1+1$$2+2$" and
        ;; "$1+2$$$3+3$$" are legal.
        (forward-char 1))
       (t
        ;; That "$" is live one.
        (throw 'found t))))))

(defun font-latex-extend-region-backwards-math ()
  "Extend region backwards for math environmets.
Take into account $...$, $$...$$, \\(...\\) and \\=\\[...\\], too."
  ;; Use `texmathp' to identify whether the point is inside math mode.
  ;; Only heuristic, but it's very difficult to identify rigorously
  ;; without syntactic support.

  ;; Check if `font-lock-beg' is inside math mode.
  (goto-char font-lock-beg)

  ;; Workaround bug#41522.  Ensure `syntax-table' property is given to
  ;; all verbatim-like constructs up to the position before running
  ;; `texmathp' in order to prevent wrong fontification of verbatim
  ;; face.  This is necessary because `texmathp' calls `up-list'
  ;; inside narrowing.
  (syntax-propertize (point))

  ;; XXX: Should we make the `texmathp' search honor
  ;; `font-latex-multiline-boundary'?
  (when (and (texmathp) (< (cdr texmathp-why) font-lock-beg))
    ;; Make its beginning a new start of font lock region.
    (setq font-lock-beg (cdr texmathp-why))
    t))

(defun font-latex-sp-extend-region-backwards-verb-env (beg end)
  "Extend region backwards for verbatim environments."
  (let ((envs (and (fboundp 'LaTeX-verbatim-environments)
                   (LaTeX-verbatim-environments))))
    (when envs
      (save-excursion
        (goto-char end)
        (catch 'extend
          (while (re-search-backward
                  (concat "\\\\end[ \t]*{" (regexp-opt envs t) "\\*?}") beg t)
            (when (and (re-search-backward
                        (concat  "\\\\begin[ \t]*{"
                                 (buffer-substring-no-properties
                                  (match-beginning 1)
                                  (match-end 0))
                                 ;; Match an optional and possible
                                 ;; mandatory argument(s)
                                 "\\(?:\\[[^][]*\\(?:\\[[^][]*\\][^][]*\\)*\\]\\)?"
                                 "\\(?:{[^}]*}\\)*")
                        (- beg font-latex-multiline-boundary) t)
                       (< (point) beg))
              (throw 'extend (cons (point) end)))))))))

(defun font-latex-update-quote-list ()
  "Update quote list and regexp if value of `font-latex-quotes' changed."
  (unless (eq font-latex-quotes-control font-latex-quotes)
    (setq font-latex-quotes-control font-latex-quotes)
    (font-latex-quotes-set-internal)
    ;; Set order of each entry in `font-latex-quote-list' according to
    ;; setting of `font-latex-quotes-internal'.
    (let ((tail font-latex-quote-list)
          elt)
      (while tail
        (setq elt (car tail))
        (when (and (> (safe-length elt) 2)
                   (not (eq (nth 2 elt) font-latex-quotes-internal)))
          (setcar tail (list (nth 1 elt) (nth 0 elt)
                             font-latex-quotes-internal)))
        (setq tail (cdr tail))))
    (setq font-latex-quote-regexp-beg
          (regexp-opt (mapcar #'car font-latex-quote-list) t))))

(defun font-latex-match-quotation (limit)
  "Match quote patterns up to LIMIT.
Used for patterns like:
``this is a normal quote'' and these are multilingual quoted strings:
\"< french \"> and \"`german\"' quotes.
The quotes << french >> and 8-bit french are used if `font-latex-quotes' is
set to `french', and >>german<< (and 8-bit) are used if set to `german'."
  (when font-latex-quotes
    (font-latex-update-quote-list)
    ;; Search for matches.
    (catch 'match
      (while (TeX-re-search-forward-unescaped
              font-latex-quote-regexp-beg limit t)
        (unless (font-latex-faces-present-p '(font-lock-comment-face
                                              font-latex-verbatim-face
                                              font-latex-math-face)
                                            (match-beginning 0))
          (let* ((beg (match-beginning 0))
                 (after-beg (match-end 0))
                 (opening-quote (match-string-no-properties 0))
                 (closing-quote
                  (nth 1 (assoc opening-quote font-latex-quote-list)))
                 (nest-count 0)
                 (point-of-surrender (+ beg font-latex-multiline-boundary)))
            ;; Find closing quote taking nested quotes into account.
            (while (progn
                     (re-search-forward
                      (concat opening-quote "\\|" closing-quote)
                      point-of-surrender 'move)
                     (when (and (< (point) point-of-surrender) (not (eobp)))
                       (if (string= (match-string-no-properties 0)
                                    opening-quote)
                           (setq nest-count (1+ nest-count))
                         (when (/= nest-count 0)
                           (setq nest-count (1- nest-count)))))))
            ;; If no closing quote was found, set the second match which
            ;; will be marked with warning color, if one was found, set
            ;; the first match which will be marked with string color.
            (if (or (= (point) point-of-surrender) (eobp))
                (progn
                  (goto-char after-beg)
                  (store-match-data (list after-beg after-beg beg after-beg)))
              (let ((p (point)))
                ;; FIXME: Duplicate of code in `font-latex-match-math-env'.
                (if (< font-latex--updated-region-end limit)
                    (setq font-latex--updated-region-end limit))
                (when (< font-latex--updated-region-end p)
                  (font-lock-unfontify-region
                   font-latex--updated-region-end p)
                  (setq font-latex--updated-region-end p))
                (store-match-data (list beg p p p))))
            (throw 'match t)))))))

(defun font-latex-extend-region-backwards-quotation ()
  "Extend region backwards for quotations."
  (when font-latex-quotes
    (font-latex-update-quote-list)
    (let ((regexp-end (regexp-opt (mapcar #'cadr font-latex-quote-list) t)))
      (save-excursion
        (goto-char font-lock-end)
        (catch 'extend
          (while (re-search-backward regexp-end font-lock-beg t)
            (let ((found-end (match-beginning 0))
                  (closing-quote (match-string-no-properties 0))
                  (nest-count 0)
                  (point-of-surrender (- font-lock-beg
                                         font-latex-multiline-boundary))
                  opening-quote)
              (catch 'found
                (dolist (elt font-latex-quote-list)
                  (when (string= (cadr elt) closing-quote)
                    (setq opening-quote (car elt))
                    (throw 'found nil))))
              ;; Find opening quote taking nested quotes into account.
              (while (if (re-search-backward (concat opening-quote "\\|"
                                                     closing-quote)
                                             point-of-surrender 'move)
                         ;; Found quotes before point-of-surrender.
                         (cond ((string= (match-string-no-properties 0)
                                         closing-quote)
                                ;; Encountered another closing quote.
                                ;; Increase nest-count and continue
                                ;; the inner loop.
                                (setq nest-count (1+ nest-count)))
                               ;; Found an opening quote.
                               ((/= nest-count 0)
                                ;; If in nest, decrease nest-count
                                ;; and continue the inner loop.
                                (setq nest-count (1- nest-count)))
                               ;; Else we arrived at the opening quote
                               ;; matching with the closing quote found
                               ;; in the outer loop.
                               ((< (point) font-lock-beg)
                                ;; If that opening quote locates
                                ;; before `font-lock-beg', break the
                                ;; outer loop and extend the region.
                                (setq font-lock-beg (point))
                                (throw 'extend t))
                               (t
                                ;; Else terminate the inner loop and
                                ;; continue the outer loop.
                                nil))
                       ;; Didn't find quotes before
                       ;; point-of-surrender.
                       ;; Go back just before the closing quote,
                       ;; terminate the inner loop and
                       ;; continue the outer loop.
                       (goto-char found-end)
                       nil)))))))))

(defun font-latex-match-script (limit)
  "Match subscript and superscript patterns up to LIMIT."
  (when (and font-latex-fontify-script
             (re-search-forward "[_^] *\\([^\n\\{}]\\|\
\\\\\\([a-zA-Z@]+\\|[^ \t\n]\\)\\|\\({\\)\\)" limit t))
    (if (and (not (memq font-latex-fontify-script '(multi-level invisible)))
             (font-latex-faces-present-p '(font-latex-subscript-face
                                           font-latex-superscript-face)))
        ;; Apply subscript and superscript highlighting only once (in case
        ;; font-latex-fontify-script is not 'multi-level) in order to prevent
        ;; the font size becoming too small.  We set an empty match to do that.
        (let ((point (point)))
          (store-match-data (list point point point point)))
      (when (match-end 3)
        (let ((beg (match-beginning 3))
              (end (TeX-find-closing-brace
                    ;; Don't match groups spanning more than one line
                    ;; in order to avoid visually wrong indentation in
                    ;; subsequent lines.
                    nil (line-end-position))))
          (store-match-data (if end
                                (list (match-beginning 0) end beg end)
                              (list beg beg beg beg))))))
    t))

(defun font-latex-match-script-chars (limit)
  "Match subscript and superscript chars up to LIMIT."
  (catch 'found
    (while (re-search-forward "[^_^]\\([_^]\\)" limit t)
      (let ((pos (match-beginning 1)))
        (when (and (font-latex-faces-present-p 'font-latex-math-face pos)
                   (not (font-latex-faces-present-p '(font-lock-constant-face
                                                      font-lock-builtin-face
                                                      font-lock-comment-face
                                                      font-latex-verbatim-face) pos))
                   ;; Check for backslash quoting
                   (not (let ((odd nil)
                              (pos pos))
                          (while (eq (char-before pos) ?\\)
                            (setq pos (1- pos) odd (not odd)))
                          odd)))
          (throw 'found t))))))

(defun font-latex--get-script-props (pos script-type)
  (let* ((old-raise (or (plist-get (get-text-property pos 'display) 'raise) 0.0))
         (new-level (1+ (or (get-text-property pos 'script-level) 0)))
         (disp-props (copy-sequence (cl-case script-type
                                      (:super (cdr font-latex-script-display))
                                      (:sub   (car font-latex-script-display)))))
         (new-disp-props (let ((raise (plist-get disp-props 'raise))
                               (nl new-level))
                           (if raise
                               ;; This polynom approximates that the factor
                               ;; which is multiplied with raise is 1 for nl=1,
                               ;; 0.8 for nl=2, 0.64 for nl=3, etc. (so always
                               ;; about 80% of the previous value).
                               (plist-put disp-props 'raise
                                          (+ old-raise
                                             (* raise
                                                (+ 1.1965254857142873
                                                   (* nl -0.21841226666666758)
                                                   (* nl nl 0.012018514285714385)))))
                             disp-props))))
    `(face ,(if (<= new-level font-latex-fontify-script-max-level)
                (cl-case script-type
                  (:super 'font-latex-superscript-face)
                  (:sub   'font-latex-subscript-face))
              nil)
           script-level ,new-level
           display ,new-disp-props)))

;; Copy and adaption of `tex-font-lock-suscript' from tex-mode.el in
;; GNU Emacs on 2004-07-07.
(defun font-latex-script (pos)
  "Return face and display spec for subscript and superscript content."
  (when (and (font-latex-faces-present-p 'font-latex-math-face pos)
             (not (font-latex-faces-present-p '(font-lock-constant-face
                                                font-lock-builtin-face
                                                font-lock-comment-face
                                                font-latex-verbatim-face) pos))
             ;; Check for backslash quoting
             (not (let ((odd nil)
                        (pos pos))
                    (while (eq (char-before pos) ?\\)
                      (setq pos (1- pos) odd (not odd)))
                    odd)))
    (if (eq (char-after pos) ?_)
        (font-latex--get-script-props pos :sub)
      (font-latex--get-script-props pos :super))))

(defun font-latex-script-char (_pos)
  "Return face and display spec for subscript and superscript character at POS."
  `(face font-latex-script-char-face
         ,@(when (eq font-latex-fontify-script 'invisible)
             '(invisible t))))

;;; docTeX

(defvar font-latex-doctex-preprocessor-face
  'font-latex-doctex-preprocessor-face
  "Face used to highlight preprocessor directives in docTeX mode.")

(defface font-latex-doctex-preprocessor-face
  '((t (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face ; Emacs 21 does not provide
                                        ; the preprocessor face.
                  font-lock-preprocessor-face))))
  "Face used to highlight preprocessor directives in docTeX mode."
  :group 'font-latex-highlighting-faces)

(defvar font-latex-doctex-documentation-face
  'font-latex-doctex-documentation-face
  "Face used to highlight the documentation in docTeX mode.")

(defface font-latex-doctex-documentation-face
  '((((class mono)) (:inverse-video t))
    (((class grayscale) (background dark)) (:background "#333"))
    (((class color) (background dark)) (:background "#333"))
    (t (:background "#eeeeee")))
  "Face used to highlight the documentation parts in docTeX mode."
  :group 'font-latex-highlighting-faces)

(defvar font-latex-doctex-keywords
  (append '((font-latex-doctex-match-^^A 0 font-lock-comment-face t))
          font-latex-keywords-2
          '(("^%<[^>]*>" (0 font-latex-doctex-preprocessor-face t)))))

(defun font-latex-doctex-match-^^A (limit)
  "In docTeX mode, match comment started by ^^A and ^^X before LIMIT."
  (catch 'found
    (while (TeX-re-search-forward-unescaped "\\^\\^[AX]" limit t)
      (when (eq (char-after (line-beginning-position)) ?\%)
        (forward-line 1)
        ;; Adjust `font-latex--updated-region-end' if necessary.
        (let ((p (point)))
          (if (< font-latex--updated-region-end limit)
              (setq font-latex--updated-region-end limit))
          (when (< font-latex--updated-region-end p)
            (font-lock-unfontify-region
             font-latex--updated-region-end p)
            (setq font-latex--updated-region-end p))
          ;; We assume that the above adjustment preserves match data.
          (set-match-data (list (match-beginning 0) p)))
        (throw 'found t)))))

;; Copy and adaptation of `doctex-font-lock-syntactic-face-function'
;; in `tex-mode.el' of CVS Emacs (March 2004)
(defun font-latex-doctex-syntactic-face-function (state)
  ;; Mark docTeX documentation, which is parsed as a style A comment
  ;; starting in column 0.
  (if (or (nth 3 state) (nth 7 state)
          (not (memq (char-before (nth 8 state))
                     '(?\n nil))))
      ;; Anything else is just as for LaTeX.
      (font-latex-syntactic-face-function state)
    font-latex-doctex-documentation-face))


;;; Installation in non-AUCTeX LaTeX mode

;; Here used to be some code which tried to magically make things work and
;; thereby broke other stuff.  If you want to use font-latex with stock
;; latex-mode, then please just add `font-latex-setup' to `latex-mode-hook'
;; yourself.

;;; Byte-compilation of generated functions
;; Not needed now that we generate the code via a macro.
;; (when (byte-code-function-p
;;        (symbol-function 'font-latex-make-built-in-keywords))
;;   (dolist (elt font-latex-built-in-keyword-classes)
;;     (let ((name (nth 0 elt)))
;;       (byte-compile (intern (concat "font-latex-match-" name)))
;;       (byte-compile (intern (concat "font-latex-match-" name "-make"))))))


;; Provide ourselves:
(provide 'font-latex)

;; Local Variables:
;; coding: utf-8
;; End:

;;; font-latex.el ends here
