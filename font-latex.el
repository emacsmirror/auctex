;;; font-latex.el --- LaTeX fontification for Font Lock mode.

;; Copyright (C) 1996-2003, 2004 Free Software Foundation.

;; Authors:    Peter S. Galbraith <psg@debian.org>
;;             Simon Marshall <Simon.Marshall@esrin.esa.it>
;; Maintainer: Peter S. Galbraith <psg@debian.org>
;; Created:    06 July 1996
;; Version:    0.928 (19 Sep 2004)
;; Keywords:   LaTeX faces

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;  This package enhances font-lock fontification patterns for LaTeX.

;; New versions of this package (if they exist) may be found at:
;;  http://people.debian.org/~psg/elisp/font-latex.el
;; or in AUCTeX's CVS archive.

;; ** Infinite loops !? **
;;  If you get an infinite loop, send me a bug report!
;;  Then set the following in your ~/.emacs file to keep on working:
;;   (setq font-latex-do-multi-line nil)

;; Description:
;;  This package enhances font-lock fontification patterns for LaTeX.
;;  font-lock mode is a minor mode that causes your comments to be
;;  displayed in one face, strings in another, reserved words in another,
;;  and so on.
;;
;;  Please see the accompanying file font-latex.tex for a demo of what
;;  font-latex is supposed to do at different fontification levels.

;; Installation instructions:
;;
;;  AUCTeX users:  <URL:http://www.gnu.org/auctex/>
;;   You don't have to do anything special as it gets installed
;;   along with the rest of AUCTeX and gets enabled by default via the
;;   customizable variable TeX-install-font-lock.
;;
;;  Other users:
;;   You should byte-compile font-latex.el (It runs faster when you
;;   byte-compile it!) :
;;     M-x byte-compile-file
;;   and put the resulting font-latex.elc file in a directory listed in your
;;   Emacs load-path.  You may then enable it by adding this form to your
;;   ~/.emacs file:
;;     (if window-system
;;         (require 'font-latex))
;;
;; Turning on font-latex:
;;
;;  After font-latex is loaded (or `required'), it will be automatically
;;  used whenever you enter `font-lock-mode' on a LaTeX buffer.  This
;;  fontification is done automatically in recent versions of Emacs and
;;  XEmacs, e.g. via a toggle switch in the menu-bar's Option menu, or by
;;  customizing the variable global-font-lock-mode in Emacs:
;;    M-x customize-variable RET global-font-lock-mode RET
;;
;; Fontification Levels:
;;
;;  There are two levels of fontification, selected by the value of the
;;  font-lock variable font-lock-maximum-decoration.  There are ways
;;  documented in font-latex.el to set this differently for each mode that
;;  uses font-lock, but if you are unsure and are running on a fast enough
;;  machine, try putting this in your ~/.emacs file:
;;    (setq font-lock-maximum-decoration t)
;;  It probably best to put it before the `(require 'font-latex)' statement
;;  if you use that.
;;
;; Changing colours
;;
;;  Okay, so you hate the colours I picked.  How do you change them you ask?
;;  First, find the font name to change using the command:
;;    M-x list-text-properties-at
;;  Then, suppose you got `font-latex-math-face', edit ~/.Xdefaults and add:
;;    Emacs.font-latex-math-face.attributeForeground: blue
;;  without the semi-colon I'm using here ascomment delimiters, of course.
;;
;; ----------------------------------------------------------------------------
;;; Change log:
;; V0.928 19Sep2004 Ralf Angeli
;;  - `font-latex-set-syntactic-keywords': "*" is not allowed as a \verb
;;    delimiter.
;; V0.927 17Sep2004 Ralf Angeli
;;  - `font-latex-verbatim-environments': Change from defvar to defcustom.
;;  - `font-latex-verbatim-environments-local', `font-latex-verbatim-macros'
;;    `font-latex-verbatim-macros-local': New variables.
;;  - `font-latex-set-syntactic-keywords': Use them.
;; V0.926 16Sep2004 Ralf Angeli
;;  - `font-latex-commented-outp': Reimplement for better performance.
;; V0.925 12Sep2004 Ralf Angeli
;;  - `font-latex-keywords-1': Add highlighter for math macros.
;;  - `font-latex-keywords-2': Use regexp for matching instead of
;;    `font-latex-match-script'.
;;  - `font-latex-match-font-outside-braces': Add support for math, esp.
;;    "\ensuremath".
;;  - `font-latex-match-script': Remove.
;;  - `font-latex-script': Fix check for present faces.  Add `texmathp'
;;    check in case there is no math face present.
;; V0.924 27Aug2004 Ralf Angeli
;;  - `font-latex': Add to AUCTeX's customization group.
;;  - `font-latex-find-matching-close': Correctly recognize multiple
;;    escape characters.  Add missing paren.
;; V0.923 18Aug2004 Ralf Angeli
;;  - `font-latex-script': Disable raising of characters for older
;;    Emacsen.  Original patch by Reiner Steib.
;; V0.922 06Aug2004 Reiner Steib
;;  - Changed URL of AUCTeX. Use "AUCTeX", not "auc-tex" (skipped Change log).
;; V0.921 05Aug2004 Reiner Steib
;;  - (font-latex-script-display): New variable.  Make raise of
;;    sub-/superscripts customizable.
;;    (font-latex-unfontify-region, font-latex-script): Use it.
;;  - (font-latex-fontify-script): Default to nil in XEmacs.
;; V0.920 04Aug2004 Ralf Angeli
;;  - `font-latex-unfontify-region': New function.
;;  - `font-latex-setup': Use it.
;; V0.919 31Jul2004 Ralf Angeli
;;  - Autoload `texmathp'.
;;  - `font-latex-keywords-2': Add `font-latex-match-script'.
;;  - `font-latex-script-keywords': Remove.
;;  - `font-latex-fontify-script': Remove :set function.
;;  - `font-latex-match-script': New function.
;; V0.918 29Jul2004 Ralf Angeli
;;  Doc fix.
;; V0.917 20Jul2004 Reiner Steib
;;  - (font-latex-set-title-face): New function.
;;  - (font-latex-title-fontity): Use it to make customization work
;;  - during a session.
;;  - (font-latex-title-*-face): Use it to simplify the initialization.
;; V0.916 08Jul2004 Ralf Angeli
;;  - `font-latex-superscript-face', `font-latex-subscript-face': New faces.
;;  - `font-latex-script-keywords': New constant.
;;  - `font-latex-fontify-script': New customize option.
;;  - `font-latex-script': New function.
;; V0.915 05Jun2004 Ralf Angeli
;;  - `font-latex-make-title-faces': New function.
;;  - `font-latex-title-1-face', `font-latex-title-2-face',
;;    `font-latex-title-3-face': Now generated by
;;    `font-latex-make-title-faces' and compatible with XEmacs.
;;  - `font-latex-title-4-face': Add face specification for XEmacs.
;; V0.914 10May2004 Ralf Angeli
;;  - `font-latex-doctex-^^A': Add compatibility code for Emacs 20 and
;;     XEmacs to fix compile error.
;;  - `font-latex-verbatim-face', `font-latex-doctex-preprocessor-face',
;;    `font-latex-doctex-documentation-face': Add parentheses to fix error
;;    with Emacs 20.
;; V0.913 08May2004 Ralf Angeli
;;  - New variables and faces.: `font-latex-verbatim-face',
;;    `font-latex-doctex-preprocessor-face',
;;    `font-latex-doctex-documentation-face'
;;  - New variables: `font-latex-verbatim-environments',
;;    `font-latex-syntactic-keywords', `font-latex-doctex-syntactic-keywords',
;;    `font-latex-doctex-keywords'
;;  - New functions: `font-latex-set-syntactic-keywords'
;;    `font-latex-syntactic-face-function', `font-latex-doctex-^^A'
;;    `font-latex-doctex-syntactic-face-function'
;;  - `font-latex-setup': Set special `font-lock-defaults' for docTeX mode.
;;  - `font-latex-commented-outp': Don't classify line comments in docTeX
;;     mode as "real" comments.
;; V0.912 08Apr2004 Peter S Galbraith
;;  - font-latex-setup: was overriding font-latex-string-face.
;;    Thanks to Reuben Thomas for finding the bug.
;; V0.911 17Feb2004 Reiner Steib
;;  - font-latex-title-4-face: Added missing :weight and :inherit.
;; V0.910 15Feb2004 Reiner Steib
;;  - font-latex-title-4-face: Use different colors depending on background.
;; V0.909 25Nov2003 Reiner Steib
;;  - font-latex-match-function-keywords: Added spacing commands, "nonumber",
;;    "centering", "TeX", and "LaTeX".
;;  - font-latex-match-textual-keywords: Added textsuperscript.
;; V0.908 17Nov2003 PSG
;;  - font-latex-keywords-2: Had forgotten to set LAXMATCH on all title
;;    matches.  Thanks to Ralf Angeli for reporting the bug.
;; V0.907 23Oct2003 PSG
;;  - Make font-latex-warning-face a defface and not a copy.  Thanks to
;;    Ralf Angeli for reporting the bug that it wasn't customizable.
;;  - Idem for font-latex-string-face.
;; V0.906 19Oct2003 PSG
;;  - Enable multi-line cache on font-latex-match-command-outside-arguments
;;    such that multi-line section commands will be fontified correctly.
;;    This is a hack that dates from when font-lock _really_ only fontified
;;    the current line.  With `jit', this is no longer strictly necessary
;;    and may at some point be *removed*.
;;  - Makes new title faces blue4, which is more consistent with the rest of
;;    font-latex colors.
;; V0.905 18Oct2003 PSG
;;  - New defcustom `font-latex-title-fontity' defaults to use varying font
;;    height in sectioning commands.
;;  - New variables and faces `font-latex-title-1-face' to
;;    `font-latex-title-4-face'
;;  - New defcustoms `font-latex-match-title-1-keywords' to
;;    `font-latex-match-title-4-keywords'
;;  - New elisp developer local variables
;;    `font-latex-match-title-1-keywords-local' to
;;    `font-latex-match-title-4-keywords-local'
;; V0.904 18Oct2003 PSG
;;  - checkdoc cleaning (almost clean now).
;; V0.903 18Sep2003 PSG
;;  - Added `font-latex-quotes' to fontify either french or german quotes.
;;  - Added internal vars `font-latex-quote-regexp-beg' and
;;    `font-latex-quote-end-list'.
;;  - Fixed font-latex-match-quotation to use above.
;; V0.902 07Sep2003 PSG
;;  - Bug fix when font-lock-multiline is set to t.
;;    When a searched pattern was commented-out, we used to return a (nil
;;    nil) pattern match to font-lock, along with the status of `t' for it
;;    to keep looking past this match.  font-lock was happy with that.  But
;;    now when font-lock-multiline is `t', the match really needs to exists
;;    otherwise there is a elisp error at line 1625 of font-lock.el in
;;    function font-lock-fontify-keywords-region.  So we provide a match
;;    that begins and ends at the same character (the end of the match).
;;    Thanks to Benoit Plessis <benoit.plessis@tuxfamily.org> for reporting
;;    the problem to Debian (Bug#208503) and for being persistent enough to
;;    find the tickling conditions.
;;  - Change some doc strings following patch from Reiner Steib
;;    <reiner.steib@gmx.de>, with my thanks.
;; V0.901 25Jul2003 PSG
;;  - Make & highlighted in font-latex-warning-face.
;;  - Better document font-latex-match-*-keywords-local variables.
;; V0.900 14Apr2003 PSG
;;    font-latex-match-*-keywords are new user customizable variable
;;    to add fontification keywords.
;;    See `M-x customize-group [RET] font-latex'.
;;    Elisp Style file writers should use the buffer-local
;;    font-latex-match-*-keywords-local variables, e.g.:
;;     (add-to-list 'font-latex-match-textual-keywords-local "captcont")
;;     (font-latex-match-textual-make)
;; V0.803 17Feb03 David Kastrup
;;   (font-latex-find-matching-close): Remove a very complicated way of
;;    doing nothing since the byte compiler warns about it.
;; V0.802 15Feb03 David Kastrup
;;   (font-latex-setup): Tweak verbatim handling.
;; V0.801 07Dec02 David Kastrup
;;   (font-latex-setup): Better stab at verbatim handling.
;; V0.800 01Nov01 PSG
;;  - Added font-lock-syntactic-keywords to font-lock-defaults to handle
;;    verbatim environment, as suggested by Stefan Monnier 5 years ago (!)
;; V0.702 15Oct01 PSG
;;  - remove LaTeX-mode-hook self-installation, since AUCTeX can now install
;;    font-latex by itself.
;;  - cleanup the docs a bit, deleting stuff relevant only for emacs19
;;    since it's now more likely to confuse users.
;; V0.701 30Mar00 Stefan Monnier <monnier@rum.cs.yale.edu> (RCS V1.63)
;;    Removed tests against specific versions of Emacs, testing for
;;    functions instead.
;; V0.700 20Dec99 PSG (RCS V1.62)
;;    Added customize support.
;; V0.603 02July98 PSG (RCS V1.61)
;;    Squashed another infinite loop.
;; V0.602 02July98 PSG (RCS V1.60)
;;    Added 'font and 'infont keywords to narrow cache triggers.
;; V0.601 02July98 PSG (RCS V1.59)
;;    Added new font-latex-find-matching-close function to replace scan-sexp.
;;    It now searches for matching {} or [] when scan-sexp fails.
;; V0.600 16June98 PSG (RCS V1.58)
;;    Rewrote the cache method again.
;; V0.512 07Apr98 Stephen R. Anderson <sra@bloch.ling.yale.edu> (RCS V1.57)
;;    xemacs beta 20.5 sets the major version to 21.
;; V0.511 07Apr98 PSG (RCS V1.55)
;;    {\bf ...} multi-line cache related infinite loop fixed.
;; V0.510 19Mar98 PSG (RCS V1.54)
;;    More multi-line cache related infinite loops fixed.
;; V0.509 20Feb98 PSG (RCS V1.53)
;;    XEmacs infinite loop in font-latex-match-font-inside-braces cache.
;; V0.508 06Feb98 PSG (RCS V1.51)
;;    Created font-latex-match-textual; changed font-latex-math-face colour.
;; V0.507 30Jan98 PSG (RCS V1.50)
;;    Removed font-latex-find-matching-close because it broke the cache.
;;    Rewrote the cache method.  Ouch!
;; V0.506 08Jan98 PSG (RCS V1.48)
;;    Added variables font-latex-match-variable, font-latex-match-function
;;    font-latex-match-reference (built using reexp-opt).
;; V0.505 07Jan98 PSG (RCS V1.47)
;;    XEmacs20 has defface.
;; V0.504 20Oct97 Kevin Ruland <kruland@seistl.com> (RCS V1.46)
;;    Fixed the real bug in font-latex-match-command-outside-arguments
;; V0.503 16Oct97 PSG (RCS V1.45)
;;    Patched font-latex-match-command-outside-arguments for allow for
;;    strange interaction with AUCTeX's LaTeX-environment command.
;; V0.502 07Oct97 (RCS V1.44)
;;    Kevin Ruland <kevin@rodin.wustl.edu> edits font-latex-find-matching-close
;;    PSG: Changed OliveGreen for OliveDrab, found in rgb.txt
;; V0.501 24Sep97 (RCS V1.42)
;;    Kevin Ruland <kevin@rodin.wustl.edu> added font-latex-find-matching-close
;;    used instead of scan-sexp to find arguments containing extra brackets.
;; V0.500 23Sep97 PSG (RCS V1.41)
;;  - Support for Emacs-20 (No customize support yet)
;; V0.403 19Nov96 (RCS V1.37)
;;  - Christoph Wedler <wedler@fmi.uni-passau.de>
;;    XEmacs patch for local math-font
;;  - Changed scheme for fontification of \section*{...}
;; V0.402 13Nov96 PSG (RCS V1.35)
;;  - Embeded comments handled.
;;  - Better XEmacs initilisation.
;; V0.401 12Nov96 PSG (RCS V1.34) - Nothing fontified when commented-out.
;; V0.400 11Nov96 PSG (RCS V1.33)
;;  - Stab at on-the-fly multiline.
;;  - mono support: <Johannes.Weinert@Informatik.Uni-Oldenburg.DE>
;; V0.314 16Oct96 PSG - Support for dark background removed for XEmacs.
;; V0.313 07Oct96 PSG (RCS V1.31) - Support for dark background.
;; V0.312 26Aug96 PSG (RCS V1.30) - Added font-latex-commented-outp.
;; V0.311 22Aug96 PSG (RCS V1.29) - fixed for XEmacs.
;; V0.310 22Aug96 simon (RCS V1.27)
;;  - make font-latex-setup run font-lock-make-faces before variable trickery.
;;  - set font-latex-string-face to the global value of font-lock-string-face.
;; V0.309 21Aug96 PSG (RCS V1.26)
;;  - new font-latex-math-face done by string syntax.  User may modify it.
;;  - new font-latex-string-face.
;; V0.308 15Aug96 PSG (RCS V1.25)
;;  - $$...$$ gets font-latex-math-face
;;  - font-latex-match-math-envII fixed.
;; V0.307 14Aug96 PSG (RCS V1.23) - setup okay if loaded in a latex-mode-hook
;; V0.306 14Aug96 PSG (RCS V1.22) - added "item" to font-latex-match-function
;; V0.305 14Aug96 PSG (RCS V1.20) - use keep in font-latex-match-math-envII
;; V0.304 14Aug96 PSG (RCS V1.18) - minor comment edits.
;; V0.303 14Aug96 simon (RCS V1.17)
;;  - rewrote font-latex-match-math-envII like font-latex-match-quotation
;; V0.302 12Aug96 PSG (RCS V1.16)
;;  - (goto-char end) in condition-case error to avoid infinite loops.
;; V0.301 08Aug96 PSG (RCS V1.14)
;;  - Better faces in XEmacs.
;; V0.300 07Aug96 PSG (RCS V1.12)
;;  - Changed font-latex-match-font-inside-braces again for stranded \bf
;;  - "[a-z]+box" changed
;;  - font-latex-match-math-env checks preceding-char for \\[
;;  - use eval-after-compile in font-latex-match-math-envII
;; V0.201 05Aug96 PSG added \\(display\\)?math to Simon's changes
;; V0.200 05Aug96 simon: (RCS V1.10)
;;  - fixed font-latex-match-command-outside-arguments
;;  - rewrote font-latex-match-font-outside-braces like above
;;  - rewrote font-latex-match-font-inside-braces like above
;; V0.101 01Aug96 PSG added \\(display\\)?math
;; V0.100 01Aug96 PSG - massive new test version
;; V0.061 23Jul96 PSG
;;  - Removed trailing "\\>" in warning-face regexp (fails with \\ \- \\*)
;; V0.06  23Jul96 PSG
;;  - fixed dobib in font-latex-labels.
;;  - shorter font regexp in levels 3+4.
;;  - removed \item and & from type
;;  - fixed font-latex-math-envII regexp
;; V0.05  22Jul96 PSG
;;  - changed \ref etc to reference-face.
;;  - \\b added in buggy \item[option] regexp (not really fixed).
;;  - font-latex-labels regexp bug
;; V0.041  simon:
;;  - added font-latex-match-command-outside-arguments
;;  - rewrote font-latex-match-quotation and font-latex-bib-highlight-mouse
;;  - rewrote then removed bib-cite functionality.
;;  - general top-level cleanup
;; V0.04 11Jul96 PSG
;;  - added font-lock-comment-start-regexp defined in 19.32
;;  - encoded 8-bit characters to 7-bit.
;; V0.03 10Jul96 PSG
;;  - font-latex-bib-cite-mouse-highlight-p can change after font-lock-defaults
;;    is constructed.
;; V0.02 09Jul96 PSG
;;  - added font-latex-bib-cite-mouse-highlight-p
;;  - Fixed `overwrite' flags
;; V0.01 06Jul96 Peter S Galbraith - Created
;; ----------------------------------------------------------------------------
;;; Code:
(require 'font-lock)
(autoload 'texmathp "texmathp")

(defgroup font-latex nil
  "Font-latex text highlighting package."
  :prefix "font-latex-"
  :group 'faces
  :group 'tex
  :group 'AUCTeX)
(defgroup font-latex-highlighting-faces nil
  "Faces for highlighting text in font-latex."
  :prefix "font-latex-"
  :group 'font-latex)

(defcustom font-latex-do-multi-line t
  "Nil means disable the multi-line fontification prone to infinite loops."
  :group 'font-latex
  :type 'boolean)

(defvar font-latex-quote-regexp-beg nil
  "Regexp used to find quotes.")

(defvar font-latex-quote-end-list nil
  "List matching end quotes for `font-latex-quote-regexp-beg'.")

(defcustom font-latex-quotes 'french
  "Whether to fontify << french quotes >> or >> german quotes <<.
Also selects \"<quote\"> versus \">quote\"<."
  :type '(choice (const french) (const german))
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (equal value 'french)
             (setq font-latex-quote-regexp-beg
                   (concat "\\(``\\)\\|\\(\"<\\)\\|\\(\"`\\)\\|\\(<<\\)\\|"
                           "\\(" (char-to-string 171) "\\)") ; An 8-bit "<<"
                   font-latex-quote-end-list
                   `("''" "\">" "\"'" ">>" ,(char-to-string 187)))
           (setq font-latex-quote-regexp-beg
                 (concat "\\(``\\)\\|\\(\">\\)\\|\\(\"`\\)\\|\\(>>\\)\\|"
                         "\\(" (char-to-string 187) "\\)") ; An 8-bit ">>"
                 font-latex-quote-end-list
                 `("''" "\"<" "\"'" "<<" ,(char-to-string 171)))))
  :group 'font-latex)

(defvar font-latex-warning-face			'font-latex-warning-face
  "Face to use for LaTeX major keywords.")
(defvar font-latex-sedate-face			'font-latex-sedate-face
  "Face to use for LaTeX minor keywords.")
(defvar font-latex-italic-face			'font-latex-italic-face
  "Face to use for LaTeX italics.")
(defvar font-latex-bold-face			'font-latex-bold-face
  "Face to use for LaTeX bolds.")
(defvar font-latex-math-face			'font-latex-math-face
  "Face to use for LaTeX math environments.")
(defvar font-latex-string-face                  'font-latex-string-face
  "Face to use for strings.  This is set by Font LaTeX.")
(defvar font-latex-verbatim-face                'font-latex-verbatim-face
  "Face to use for text in verbatim macros or environments.")
(defvar font-latex-superscript-face             'font-latex-superscript-face
  "Face to use for superscripts.")
(defvar font-latex-subscript-face               'font-latex-subscript-face
  "Face to use for subscripts.")

;; The definitions of the title faces were originally taken from
;; info.el (Copyright (C) 1985, 86, 92, 93, 94, 95, 96, 97, 98, 99,
;; 2000, 2001 Free Software Foundation, Inc.) and adapted to the needs
;; of font-latex.el.
(defun font-latex-make-title-faces ()
  "Build the faces used to fontify sectioning commands."
  (dotimes (i 3)
    (let* ((num (1+ i))
	   (face-name (intern (concat "font-latex-title-" (number-to-string num)
				      "-face"))))
      (if (featurep 'xemacs)
	  (let ((size (concat (number-to-string
			       (round (* (face-height 'default)
					 (expt 1.2 (- 3 i)))))
			      "pt")))
	    (eval `(defface ,face-name
		     '((((type tty pc) (class color))
			(:foreground "lightblue" :bold t))
		       (((class color) (background light))
			(:bold t :foreground "blue4" :family "helvetica"
			       :size ,size))
		       (((class color) (background dark))
			(:bold t :foreground "yellow" :family "helvetica"
			       :size ,size))
		       (t (:size ,size :family "helvetica")))
		     ,(concat "Face for LaTeX titles at level "
			      (number-to-string num) ".")
		     :group 'font-latex-highlighting-faces)))
	(eval `(defface ,face-name
	   '((((type tty pc) (class color)) (:foreground "yellow" :weight bold))
	     (t (:height 1.2 :inherit ,(intern (concat
						"font-latex-title-"
						(number-to-string (1+ num))
						"-face")))))
	   ,(concat "Face for LaTeX titles at level "
		    (number-to-string num) ".")
	   :group 'font-latex-highlighting-faces))))))
(font-latex-make-title-faces)

(defface font-latex-title-4-face
  (if (featurep 'xemacs)
      '((((type tty pc) (class color)) (:bold t))
	(((class color) (background light))
	 (:bold t :foreground "blue4" :family "helvetica"))
	(((class color) (background dark))
	 (:bold t :foreground "yellow" :family "helvetica"))
	(t (:bold t :family "helvetica")))
    '((((type tty pc) (class color)) (:weight bold))
      (((class color) (background light))
       (:weight bold :inherit variable-pitch :foreground "blue4"))
      (((class color) (background dark))
       (:weight bold :inherit variable-pitch :foreground "yellow"))
      (t (:weight bold :inherit variable-pitch))))
  "Face for LaTeX titles at level 4."
  :group 'font-latex-highlighting-faces)

(defun font-latex-set-title-face (level)
  "Set fontification of LaTeX titles at LEVEL."
  (cond
   ((equal font-latex-title-fontity 'height)
    (set (intern (format "font-latex-title-%d-face" level))
	 (intern (format "font-latex-title-%d-face" level))))
   ((equal font-latex-title-fontity 'color)
    (set (intern (format "font-latex-title-%d-face" level))
	 'font-lock-type-face))))

(defcustom font-latex-title-fontity 'height
  "Whether to fontity LaTeX titles with varying height faces or a color face."
  :type '(choice (const height)
                 (const color))
  :set (lambda (symbol value)
	 (set symbol value)
	 (dotimes (i 4) (font-latex-set-title-face (1+ i))))
  :group 'font-latex)

(defvar font-latex-title-1-face (font-latex-set-title-face 1)
  "Face for LaTeX titles at level 1.")
(defvar font-latex-title-2-face (font-latex-set-title-face 2)
  "Face for LaTeX titles at level 2.")
(defvar font-latex-title-3-face (font-latex-set-title-face 3)
  "Face for LaTeX titles at level 3.")
(defvar font-latex-title-4-face (font-latex-set-title-face 4)
  "Face for LaTeX titles at level 4.")

(defvar font-latex-match-variable)
(defvar font-latex-match-variable-keywords)
(defvar font-latex-match-variable-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-variable-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-variable-make'.  This variable is not for end users; they
should customize `font-latex-match-variable-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-variable-keywords-local \"setstuff\")
 (font-latex-match-variable-make)")
(make-variable-buffer-local 'font-latex-match-variable-keywords-local)

(defun font-latex-match-variable-make ()
  "Make or remake the variable `font-latex-match-variable'.
Done using `font-latex-match-variable-keywords' as input."
  (setq font-latex-match-variable
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-variable-keywords-local
                        font-latex-match-variable-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-variable-keywords-set (symbol value)
  "Update `font-latex-match-variable'.
The function is called with SYMBOL bound to
`font-latex-match-variable-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-variable' is set."
  (set-default symbol value)
  (font-latex-match-variable-make))

(defcustom font-latex-match-variable-keywords
  '("setlength" "settowidth" "setcounter" "addtolength" "addtocounter")
  "Font-latex keywords for variable face.
e.g. \\setlength[option]{key}
  -> \\setlength appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in font-lock-variable-face"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-variable-keywords-set
  :group 'font-latex)

(defvar font-latex-match-reference)
(defvar font-latex-match-reference-keywords)
(defvar font-latex-match-reference-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-reference-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-reference-make'.  This variable is not for end users; they
should customize `font-latex-match-reference-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-reference-keywords-local \"someref\")
 (font-latex-match-reference-make)")

(make-variable-buffer-local 'font-latex-match-reference-keywords-local)

(defun font-latex-match-reference-make ()
  "Make or remake the variable `font-latex-match-reference'.
Done using `font-latex-match-reference-keywords' as input."
  (setq font-latex-match-reference
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-reference-keywords-local
                        font-latex-match-reference-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-reference-keywords-set (symbol value)
  "Update `font-latex-match-reference'.
The function is called with SYMBOL bound to
`font-latex-match-reference-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-reference' is set."
  (set-default symbol value)
  (font-latex-match-reference-make))

(defcustom font-latex-match-reference-keywords
  '("nocite" "cite" "label" "pageref" "vref" "eqref" "ref"
    "include" "input" "bibliography"
    "index" "glossary" "footnote" "footnotemark" "footnotetext")
  "Font-latex keyword for reference face.
e.g. \\cite[option]{key}
  -> \\cite appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in `font-lock-reference-face'"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-reference-keywords-set
  :group 'font-latex)

(defvar font-latex-match-function)
(defvar font-latex-match-function-keywords)
(defvar font-latex-match-function-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-function-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-function-make'.  This variable is not for end users; they
should customize `font-latex-match-function-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-function-keywords-local \"somecommand\")
 (font-latex-match-function-make)")
(make-variable-buffer-local 'font-latex-match-function-keywords-local)

(defun font-latex-match-function-make ()
  "Make or remake the variable `font-latex-match-function'.
Done using `font-latex-match-function-keywords' as input."
  (setq font-latex-match-function
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-function-keywords-local
                        font-latex-match-function-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-function-keywords-set (symbol value)
  "Update `font-latex-match-function'.
The function is called with SYMBOL bound to
`font-latex-match-function-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-function' is set."
  (set-default symbol value)
  (font-latex-match-function-make))

(defcustom font-latex-match-function-keywords
  '("begin" "end"
    "pagenumbering"
    "thispagestyle" "pagestyle"
    "nofiles" "includeonly"
    "bibliographystyle" "documentstyle" "documentclass"
    "newenvironment" "newcommand" "newlength" "newtheorem" "newcounter"
    "renewenvironment" "renewcommand" "renewlength" "renewtheorem"
    "renewcounter"
    "usepackage" "fbox" "mbox" "sbox" "vspace" "hspace"
    "thinspace" "negthinspace" "enspace" "enskip" "quad" "qquad"
    "nonumber" "centering" "TeX" "LaTeX")
  "Font-latex keyword for function face.
e.g. \\newcommand[option]{key}
  -> \\newcommand appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in font-lock-function-face"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-function-keywords-set
  :group 'font-latex)

;;;--------------
;;; Title level 1
(defvar font-latex-match-title-1)
(defvar font-latex-match-title-1-keywords)
(defvar font-latex-match-title-1-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-title-1-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-title-1-make'.  This variable is not for end users; they
should customize `font-latex-match-title-1-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-title-1-keywords-local \"somesection\")
 (font-latex-match-title-1-make)")
(make-variable-buffer-local 'font-latex-match-title-1-keywords-local)

(defun font-latex-match-title-1-make ()
  "Make or remake the variable `font-latex-match-title-1'.
Done using `font-latex-match-title-1-keywords' as input."
  (setq font-latex-match-title-1
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-title-1-keywords-local
                        font-latex-match-title-1-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-title-1-keywords-set (symbol value)
  "Update `font-latex-match-title-1'.
The function is called with SYMBOL bound to
`font-latex-match-title-1-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-title-1' is set."
  (set-default symbol value)
  (font-latex-match-title-1-make))

(defcustom font-latex-match-title-1-keywords
  '("part" "chapter")
  "Font-latex keywords for title level 1 face.
e.g. \\section[option]{key}
  -> \\section appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in `font-lock-type-face'"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-title-1-keywords-set
  :group 'font-latex)

;;;--------------
;;; Title level 2
(defvar font-latex-match-title-2)
(defvar font-latex-match-title-2-keywords)
(defvar font-latex-match-title-2-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-title-2-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-title-2-make'.  This variable is not for end users; they
should customize `font-latex-match-title-2-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-title-2-keywords-local \"somesection\")
 (font-latex-match-title-2-make)")
(make-variable-buffer-local 'font-latex-match-title-2-keywords-local)

(defun font-latex-match-title-2-make ()
  "Make or remake the variable `font-latex-match-title-2'.
Done using `font-latex-match-title-2-keywords' as input."
  (setq font-latex-match-title-2
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-title-2-keywords-local
                        font-latex-match-title-2-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-title-2-keywords-set (symbol value)
  "Update `font-latex-match-title-2'.
The function is called with SYMBOL bound to
`font-latex-match-title-2-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-title-2' is set."
  (set-default symbol value)
  (font-latex-match-title-2-make))

(defcustom font-latex-match-title-2-keywords
  '("section")
  "Font-latex keywords for title level 2 face.
e.g. \\section[option]{key}
  -> \\section appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in `font-lock-type-face'"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-title-2-keywords-set
  :group 'font-latex)


;;;--------------
;;; Title level 3
(defvar font-latex-match-title-3)
(defvar font-latex-match-title-3-keywords)
(defvar font-latex-match-title-3-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-title-3-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-title-3-make'.  This variable is not for end users; they
should customize `font-latex-match-title-3-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-title-3-keywords-local \"somesection\")
 (font-latex-match-title-3-make)")
(make-variable-buffer-local 'font-latex-match-title-3-keywords-local)

(defun font-latex-match-title-3-make ()
  "Make or remake the variable `font-latex-match-title-3'.
Done using `font-latex-match-title-3-keywords' as input."
  (setq font-latex-match-title-3
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-title-3-keywords-local
                        font-latex-match-title-3-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-title-3-keywords-set (symbol value)
  "Update `font-latex-match-title-3'.
The function is called with SYMBOL bound to
`font-latex-match-title-3-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-title-3' is set."
  (set-default symbol value)
  (font-latex-match-title-3-make))

(defcustom font-latex-match-title-3-keywords
  '("subsection")
;; "subsubsection"
;;; "paragraph" "subparagraph" "subsubparagraph"
  "Font-latex keywords for title level 3 face.
e.g. \\section[option]{key}
  -> \\section appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in `font-lock-type-face'"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-title-3-keywords-set
  :group 'font-latex)


;;;--------------
;;; Title level 4
(defvar font-latex-match-title-4)
(defvar font-latex-match-title-4-keywords)
(defvar font-latex-match-title-4-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-title-4-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-title-4-make'.  This variable is not for end users; they
should customize `font-latex-match-title-4-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-title-4-keywords-local \"somesection\")
 (font-latex-match-title-4-make)")
(make-variable-buffer-local 'font-latex-match-title-4-keywords-local)

(defun font-latex-match-title-4-make ()
  "Make or remake the variable `font-latex-match-title-4'.
Done using `font-latex-match-title-4-keywords' as input."
  (setq font-latex-match-title-4
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-title-4-keywords-local
                        font-latex-match-title-4-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-title-4-keywords-set (symbol value)
  "Update `font-latex-match-title-4'.
The function is called with SYMBOL bound to
`font-latex-match-title-4-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-title-4' is set."
  (set-default symbol value)
  (font-latex-match-title-4-make))

(defcustom font-latex-match-title-4-keywords
  '("subsubsection" "paragraph" "subparagraph" "subsubparagraph")
  "Font-latex keywords for title level 3 face.
e.g. \\section[option]{key}
  -> \\section appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in `font-lock-type-face'"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-title-4-keywords-set
  :group 'font-latex)
(defvar font-latex-match-textual)
(defvar font-latex-match-textual-keywords)
(defvar font-latex-match-textual-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-textual-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-textual-make'.  This variable is not for end users; they
should customize `font-latex-match-textual-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-textual-keywords-local \"somesection\")
 (font-latex-match-textual-make)")
(make-variable-buffer-local 'font-latex-match-textual-keywords-local)

(defun font-latex-match-textual-make ()
  "Make or remake the variable `font-latex-match-textual'.
Done using `font-latex-match-textual-keywords' as input."
  (setq font-latex-match-textual
        (concat
         "\\\\" "\\("
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-textual-keywords-local
                        font-latex-match-textual-keywords)))
           (regexp-opt fields t))
         "\\)\\>")
        ))

(defun font-latex-match-textual-keywords-set (symbol value)
  "Update `font-latex-match-textual'.
The function is called with SYMBOL bound to
`font-latex-match-textual-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-textual' is set."
  (set-default symbol value)
  (font-latex-match-textual-make))

(defcustom font-latex-match-textual-keywords
  '("item" ;;;FIXME: does not have an {arg} so should treated elsewhere.
;;; "part" "chapter" "section" "subsection" "subsubsection"
;;; "paragraph" "subparagraph" "subsubparagraph"
    "title" "author" "date" "thanks" "address"
    "caption" "textsuperscript")
  "Font-latex keywords for textual face.
e.g. \\section[option]{key}
  -> \\section appears in `font-lock-keyword-face'
  -> [opt] appears in `font-lock-variable-name-face'
  -> {key} appears in `font-lock-type-face'"
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-textual-keywords-set
  :group 'font-latex)

(defvar font-latex-match-warning)
(defvar font-latex-match-warning-keywords)
(defvar font-latex-match-warning-keywords-local nil
  "Buffer-local keywords to add to `font-latex-match-warning-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`font-latex-match-warning-make'.  This variable is not for end users; they
should customize `font-latex-match-warning-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list 'font-latex-match-warning-keywords-local \"somebreak\")
 (font-latex-match-warning-make)")

(make-variable-buffer-local 'font-latex-match-warning-keywords-local)

(defun font-latex-match-warning-make ()
  "Make or remake the variable `font-latex-match-warning'.
Done using `font-latex-match-warning-keywords' as input."
  (setq font-latex-match-warning
        (concat
         "\\\\"
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields (append
                        font-latex-match-warning-keywords-local
                        font-latex-match-warning-keywords)))
           (regexp-opt fields t))
);;         "\\)")
        ))

(defun font-latex-match-warning-keywords-set (symbol value)
  "Update `font-latex-match-warning'.
The function is called with SYMBOL bound to
`font-latex-match-warning-keywords' and VALUE is the the list of
keywords.  As a side effect, the variable `font-latex-match-warning' is set."
  (set-default symbol value)
  (font-latex-match-warning-make))

(defcustom font-latex-match-warning-keywords
  '("nopagebreak" "pagebreak" "newpage" "clearpage" "cleardoublepage"
    "enlargethispage" "nolinebreak" "linebreak" "newline"
    "-" "\\" "\\*" "appendix"
    "displaybreak" "allowdisplaybreaks")
  "Font-latex keywords for warning face."
  :type '(repeat (string :tag "keyword"))
  :set 'font-latex-match-warning-keywords-set
  :group 'font-latex)

(defun font-latex-match-warning-function (limit)
  "Find `font-latex-match-warning' keywords up to LIMIT for font-lock."
  (when (re-search-forward  font-latex-match-warning limit t)
    (goto-char (match-end 0))
    (store-match-data (list (match-beginning 0) (point)))
    t))


;;; Keywords

(defvar font-latex-keywords-1
  '((font-latex-match-warning-function . font-latex-warning-face)
    ("\\(^\\|[^\\\\]\\)\\(&+\\)" 2 font-latex-warning-face)   ;;; & but not \&
    ("\\$\\$\\([^$]+\\)\\$\\$" 1 font-latex-math-face)        ;;; $$...$$
    (font-latex-match-quotation . font-latex-string-face)     ;;; ``...''
    (font-latex-match-font-outside-braces		      ;;;\textit{text}
     (0 font-lock-keyword-face
        append                         ;Override? [t 'keep 'prepend 'append]
        ;; Can't use prepend because that overwrites syntax fontification
        ;; e.g. comments.
        t)                              ;Laxmatch? if t, do not signal error
     (1 font-latex-italic-face append t)
     (2 font-latex-bold-face append t)
     (3 font-lock-type-face append t)
     (4 font-latex-math-face append t))
    (font-latex-match-font-inside-braces		      ;;;{\it text}
     (0 font-lock-keyword-face append t)
     (1 font-latex-italic-face append t)
     (2 font-latex-bold-face append t)
     (3 font-lock-type-face append t)))
  "Subdued level highlighting for LaTeX modes.")

(defvar font-latex-keywords-2
  (append font-latex-keywords-1
   '((font-latex-match-reference                              ;;;\cite
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;    [opt]
      (2 font-lock-reference-face append t))                 ;;;         {key}
     (font-latex-match-function                              ;;;\documentclass
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-lock-function-name-face append t))             ;;;        {text}
     (font-latex-match-title-1                               ;;;\chapter
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-latex-title-1-face append t))                  ;;;        {text}
     (font-latex-match-title-2                               ;;;\section
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-latex-title-2-face append t))                  ;;;        {text}
     (font-latex-match-title-3                               ;;;\subsection
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-latex-title-3-face append t))                  ;;;        {text}
     (font-latex-match-title-4                               ;;;\subsubsection
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-latex-title-4-face append t))                  ;;;        {text}
     (font-latex-match-textual                               ;;;\title
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-lock-type-face append t))                      ;;;        {text}
     (font-latex-match-variable
      (0 font-lock-keyword-face nil t)
      (1 font-lock-variable-name-face append t)
      (2 font-lock-variable-name-face append t))
     (font-latex-match-math-env
      (0 font-latex-math-face append t))         	      ;;;\(...\)
     (font-latex-match-math-envII                             ;;;Math environ.
      (0 font-latex-math-face append t))
     ("\\\\[@A-Za-z]+"                                        ;;;Other commands
      (0 font-latex-sedate-face append))
     ;; Regexp taken from `tex-font-lock-keywords-3' from tex-mode.el
     ;; in GNU Emacs on 2004-07-07.
     ("[_^] *\\([^\n\\{}]\\|\\\\\\([a-zA-Z@]+\\|[^ \t\n]\\)\\|{\\(?:[^{}\\]\\|\\\\.\\|{[^}]*}\\)*}\\)"
      (1 (font-latex-script (match-beginning 0)) append))))
  "High level highlighting for LaTeX modes.")

(defvar font-latex-keywords font-latex-keywords-1
  "Default expressions to highlight in TeX mode.")

;; End-User can stop reading here.

(defvar font-lock-comment-start-regexp nil
  "Regexp to match the start of a comment.")

(eval-when-compile
  (require 'cl))


;;; Subscript and superscript

(defcustom font-latex-fontify-script (not (featurep 'xemacs))
  "Non-nil means do not fontify subscript or superscript strings.
Fontification will only work if texmathp.el is available.
This feature does not work in XEmacs."
  :type 'boolean
  :group 'font-latex)


;;; Syntactic keywords

(defcustom font-latex-verbatim-environments
  '("verbatim" "verbatim*")
  "Environments which should be fontified as verbatim."
  :type '(repeat (string))
  :group 'font-latex)

(defvar font-latex-verbatim-environments-local nil
  "Buffer-local keywords to add to `font-latex-verbatim-environments'.
This must be a list of strings.  The variable is not for end
users; they should customize `font-latex-verbatim-environments'
instead.  It is for authors of Lisp files that get loaded when
LaTeX style files are used in the current buffer.  They should
add keywords to this list and rebuild the variable
`font-latex-syntactic-keywords' by calling the function
`font-latex-set-syntactic-keywords'.")
(make-variable-buffer-local 'font-latex-verbatim-environments-local)

(defcustom font-latex-verbatim-macros
  '("verb" "verb*")
  "Macros which should be fontified as verbatim."
  :type '(repeat (string))
  :group 'font-latex)

(defvar font-latex-verbatim-macros-local nil
  "Buffer-local keywords to add to `font-latex-verbatim-macros'.
This must be a list of strings.  The variable is not for end
users; they should customize `font-latex-verbatim-macros'
instead.  It is for authors of Lisp files that get loaded when
LaTeX style files are used in the current buffer.  They should
add keywords to this list and rebuild the variable
`font-latex-syntactic-keywords' by calling the function
`font-latex-set-syntactic-keywords'.")
(make-variable-buffer-local 'font-latex-verbatim-macros-local)

(defun font-latex-set-syntactic-keywords ()
  "Set the variable `font-latex-syntactic-keywords'.
This function can be used to refresh the variable in case other
variables influencing its value, like `font-latex-verbatim-environments',
have changed."
  (let ((verb-envs (regexp-opt
		    (append font-latex-verbatim-environments
			    font-latex-verbatim-environments-local)))
	(verb-macros (regexp-opt
		      (append font-latex-verbatim-macros
			      font-latex-verbatim-macros-local))))
    (setq font-latex-syntactic-keywords
	  `((,(concat "^\\\\begin *{\\(?:" verb-envs "\\)}\\(.?\\).*\\(\n\\)")
	     (1 "<") (2 "|" t))
	    (,(concat "\\(\n\\)\\\\end *{\\(?:" verb-envs "\\)}\\(.?\\)")
	     (1 "|" t) (2 "<"))
	    (,(concat "\\\\\\(?:" verb-macros "\\)\\([^a-z@*]\\).*?\\(\\1\\)")
	     (1 "\"") (2 "\""))))))

(defvar font-latex-syntactic-keywords
  (font-latex-set-syntactic-keywords)
  "Syntactic keywords used by `font-latex'.")


;;; Syntactic fontification

;; Copy and adaptation of `tex-font-lock-syntactic-face-function' in
;; `tex-mode.el' of CVS Emacs (March 2004)
(defun font-latex-syntactic-face-function (state)
  (let ((char (nth 3 state)))
    (cond
     ((not char) font-lock-comment-face)
     ((eq char ?$) font-latex-math-face)
     (t
      (when (char-valid-p char)
	;; This is a \verb?...? construct.  Let's find the end and mark it.
	(save-excursion
	  (skip-chars-forward (string ?^ char)) ;; Use `end' ?
	  (when (eq (char-syntax (preceding-char)) ?/)
	    (put-text-property (1- (point)) (point) 'syntax-table '(1)))
	  (unless (eobp)
	    (put-text-property (point) (1+ (point)) 'syntax-table '(7)))))
      font-latex-verbatim-face))))


;;; Faces

(defface font-latex-bold-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :bold t))
    (((class color) (background light))
     (:foreground "DarkOliveGreen" :bold t ))
    (((class color) (background dark)) (:foreground "OliveDrab" :bold t ))
    (t (:bold t)))
  "Font Lock mode face used to bold LaTeX."
  :group 'font-latex-highlighting-faces)

(defface font-latex-italic-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :italic t))
    (((class color) (background light))
     (:foreground "DarkOliveGreen" :italic t ))
    (((class color) (background dark))
     (:foreground "OliveDrab" :italic t ))
    (t (:italic t)))
  "Font Lock mode face used to highlight italic LaTeX."
  :group 'font-latex-highlighting-faces)

(defface font-latex-math-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :underline t))
    (((class color) (background light)) (:foreground "SaddleBrown"))
    (((class color) (background dark))  (:foreground "burlywood"))
    (t (:underline t)))
  "Font Lock mode face used to highlight math in LaTeX."
  :group 'font-latex-highlighting-faces)

(defface font-latex-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Font Lock mode face used to highlight sedate stuff in LaTeX."
  :group 'font-latex-highlighting-faces)

(defface font-latex-string-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock face used to highlight strings in LaTeX."
  :group 'font-latex-highlighting-faces)

(defface font-latex-warning-face
  '((((class grayscale)(background light))(:foreground "DimGray" :bold t))
    (((class grayscale)(background dark))(:foreground "LightGray" :bold t))
    (((class color)(background light))(:foreground "red" :bold t ))
    (((class color)(background dark))(:foreground "red" :bold t ))
    (t (:bold t)))
  "Font Lock face for LaTeX major keywords."
  :group 'font-latex-highlighting-faces)

(defface font-latex-verbatim-face
  '((t (:inherit font-latex-math-face :family "courier")))
  "Face used to highlight TeX verbatim environments."
  :group 'font-latex-highlighting-faces)

(defface font-latex-superscript-face
  '((t (:height 0.8)))
  "Face used for superscripts.")

(defface font-latex-subscript-face
  '((t (:height 0.8)))
  "Face used for subscripts.")


;;; Setup

;;;###autoload
(defun font-latex-setup ()
  "Setup this buffer for LaTeX font-lock.  Usually called from a hook."
  ;; Trickery to make $$ fontification be in `font-latex-math-face' while
  ;; strings get whatever `font-lock-string-face' has been set to.
  (cond
   ((fboundp 'built-in-face-specifiers)
    ;; Cool patch from Christoph Wedler...
    (let (instance)
      (mapcar (lambda (property)
		(setq instance
		      (face-property-instance 'font-latex-math-face property
					      nil 0 t))
		(if (numberp instance)
		    (setq instance
			  (face-property-instance 'default property nil 0)))
		(or (numberp instance)
		    (set-face-property 'font-lock-string-face property
				       instance (current-buffer))))
	      (built-in-face-specifiers))))
   (t
  ;;(if (fboundp 'font-lock-make-faces) (font-lock-make-faces))
    (make-local-variable 'font-lock-string-face)
    (setq font-lock-string-face font-latex-math-face)))

  ;; Tell Font Lock about the support.
  (make-local-variable 'font-lock-defaults)
  ;; The test for `major-mode' currently only works with docTeX mode
  ;; because `TeX-install-font-lock' is called explicitely in
  ;; `doctex-mode'.  In case other modes have to be distinguished as
  ;; well, remove the call to `TeX-install-font-lock' from
  ;; `VirTeX-common-initialization' and place it in the different
  ;; `xxx-mode' calls instead, but _after_ `major-mode' is set.
  (cond
   ((eq major-mode 'doctex-mode)
    (setq font-lock-defaults
          '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2
				 font-latex-doctex-keywords)
            nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
            (font-lock-comment-start-regexp . "%")
            (font-lock-mark-block-function . mark-paragraph)
	    (font-lock-unfontify-region-function
	     . font-latex-unfontify-region)
            (font-lock-syntactic-face-function
             . font-latex-doctex-syntactic-face-function)
            (font-lock-syntactic-keywords
             . font-latex-doctex-syntactic-keywords))))
   (t
    (setq font-lock-defaults
          '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
            nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
            (font-lock-comment-start-regexp . "%")
            (font-lock-mark-block-function . mark-paragraph)
	    (font-lock-unfontify-region-function
	     . font-latex-unfontify-region)
            (font-lock-syntactic-keywords
             . font-latex-syntactic-keywords))))))

;; Copy and adaption of `tex-font-lock-unfontify-region' from
;; tex-mode.el in GNU Emacs on 2004-08-04.
(defun font-latex-unfontify-region (beg end)
  "Unfontify region from BEG to END."
  (font-lock-default-unfontify-region beg end)
  (while (< beg end)
    (let ((next (next-single-property-change beg 'display nil end))
	  (prop (get-text-property beg 'display)))
      (if (and (eq (car-safe prop) 'raise)
	       (member (car-safe (cdr prop))
		       (list (nth 1 (car font-latex-script-display))
			     (nth 1 (cdr font-latex-script-display))))
	       (null (cddr prop)))
	  (put-text-property beg next 'display nil))
      (setq beg next))))

;; Should not be necessary since XEmacs' font-lock also supports
;; Emacs' use of the `font-lock-defaults' local variable.   -Stefan
;; (when (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))
;;     (put 'latex-mode 'font-lock-defaults
;;          '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
;;            nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
;;            (font-lock-comment-start-regexp . "%")
;;            (font-lock-mark-block-function . mark-paragraph)))
;;     (put 'latex-tex-mode	'font-lock-defaults 'latex-mode)
;;     (put 'LaTex-tex-mode	'font-lock-defaults 'latex-mode)
;;     (put 'LaTeX-mode        'font-lock-defaults 'latex-mode)
;;     (put 'japanese-LaTeX-mode 'font-lock-defaults 'latex-mode)
;;     (put 'LATeX-MoDe	'font-lock-defaults 'latex-mode)
;;     (put 'lATEx-mODe	'font-lock-defaults 'latex-mode))


(defun font-latex-match-reference (limit)
  (if font-latex-match-reference
      (font-latex-match-command-outside-arguments font-latex-match-reference
;;;   (eval-when-compile
;;;     (concat "\\\\" "\\("
;;;             (mapconcat 'identity
;;;              '("[A-Za-z]*cite[A-Za-z]*" "label" "\\(page\\|v\\|eq\\)?ref"
;;;                "index" "glossary" "\\(footnote\\(mark\\|text\\)?\\)")
;;;              "\\|")
;;;      "\\)\\>"))
                                                  limit nil nil)))

(defun font-latex-match-function (limit)
  "Fontify things like \\documentclass{article} up to LIMIT."
  (if font-latex-match-function
      (font-latex-match-command-outside-arguments font-latex-match-function
                                                  limit nil t)))
(defun font-latex-match-textual (limit)
  "Fontify things like \\title{text} up to LIMIT."
  (if font-latex-match-textual
      (font-latex-match-command-outside-arguments font-latex-match-textual
                                                  limit nil t)))
(defun font-latex-match-title-1 (limit)
  "Fontify things like \\chapter{text} up to LIMIT."
  (if font-latex-match-title-1
      (font-latex-match-command-outside-arguments font-latex-match-title-1
                                                  limit nil t)))
(defun font-latex-match-title-2 (limit)
  "Fontify things like \\section{text} up to LIMIT."
  (if font-latex-match-title-2
      (font-latex-match-command-outside-arguments font-latex-match-title-2
                                                  limit nil t)))
(defun font-latex-match-title-3 (limit)
  "Fontify things like \\subsection{text} up to LIMIT."
  (if font-latex-match-title-3
      (font-latex-match-command-outside-arguments font-latex-match-title-3
                                                  limit nil t)))
(defun font-latex-match-title-4 (limit)
  "Fontify things like \\subsubsection{text} up to LIMIT."
  (if font-latex-match-title-4
      (font-latex-match-command-outside-arguments font-latex-match-title-4
                                                  limit nil t)))
(defun font-latex-match-variable (limit)
  "Fontify things like \\newcommand{stuff} up to LIMIT."
  (if font-latex-match-variable
      (font-latex-match-command-outside-arguments font-latex-match-variable
                                                  limit t nil)))

;; font-latex-find-matching-close is a little helper function which
;; is used like scan-sexp.  It skips over matching
;; pairs of '{' and '}'.  As an added benefit, it ignores any characters
;; which occur after the tex comment character %.
(defun font-latex-find-matching-close (openchar closechar)
  "Skip over matching pairs of { } or [ ], ignoring comments.
OPENCHAR is the opening character and CLOSECHAR is the closing character."
  (let ((parse-sexp-ignore-comments t) ; scan-sexps ignores comments
        (init-point (point))
        (status)
	(esc-char (if (and (boundp 'TeX-esc) TeX-esc) TeX-esc "\\")))
    (if (condition-case nil
            (goto-char (scan-sexps (point) 1))
          (error))
        ;; No error code.  See if closechar is quoted
        (if (save-excursion
	      (backward-char 1)
	      (not (zerop (mod (skip-chars-backward (regexp-quote esc-char))
			       2))))
            (setq status nil)
          (setq status t))
      ;; Terminated in error -- Try ourselves
      (setq status nil))
    (if status
        t
      (goto-char init-point)
      (let ((target)
            (mycount 1))
        (save-excursion
          (save-match-data
            (forward-char 1)
            (while (and (> mycount 0)
                        (progn
                          (re-search-forward
                           (concat "["
                                   ;; closechar might be ]
                                   ;; and therefor must be first in regexp
                                   (char-to-string closechar)
                                   (char-to-string openchar)
                                   "]")
                           nil t)))
              (cond
               ((font-latex-commented-outp)
                (forward-line 1))
               ((save-excursion
		  (backward-char 1)
		  (not (zerop (mod (skip-chars-backward (regexp-quote esc-char))
				   2))))
                nil)
               (t
                (setq mycount (if (= (preceding-char) openchar)
                                  (+ mycount 1)
                                (- mycount 1))))))
            (setq target (point))))
        (if (= mycount 0)
            (goto-char target))))))

;; FIXME: --About font-latex-commented-outp--
;; Fontification is *slower* for affected functions (in particular
;; font-latex-match-function), so it will be worth it to increase
;; performance in the algorithm.
;;  - don't return (store-match-data (list nil nil)) in
;;    font-latex-match-command-outside-arguments, instead skip over
;;    commented-out parts internally.
;;  - Perhaps handling outlined code is excessive and slows down the
;;    search too much?
;;  - Is save-match-data expensive? The calling function could store
;;    the match-data before it calls (font-latex-commented-outp) knowing
;;    that is would trash the list.
(defun font-latex-commented-outp ()
  "Return t if comment character is found between bol and point."
  (save-excursion
    (let ((limit (point))
	  (esc-char (if (and (boundp 'TeX-esc) TeX-esc) TeX-esc "\\")))
      (forward-line 0)
      (if (eq (char-after) ?\%)
	  (not (eq major-mode 'doctex-mode))
	(catch 'found
	  (while (progn (skip-chars-forward "^%" limit)
			(< (point) limit))
	    (when (save-excursion
		    (zerop
		     (mod (skip-chars-backward (regexp-quote esc-char)) 2)))
	      (throw 'found t))
	    (forward-char)))))))


;;;;------------------
;;;; Cache Method:
;;;
;;; This works:
;;;
;;; (defun font-latex-set-cache (cache-id)
;;;   (let ((cache (intern cache-id)))
;;;     (set cache (list (point) (point-max)))))
;;; (defun font-latex-get-cache (cache-id item)
;;;   (let ((cache (intern cache-id)))
;;;     (nth item (symbol-value cache))))
;;; (font-latex-set-cache "font-latex-match-command-cache")
;;; (font-latex-get-cache "font-latex-match-command-cache" 1)
;;;
;;; but let's use symbols instead:

;;; Hacker's note: I haven't tested extensively using lazy-lock, which
;;; apparently fontifies the entire visble page instead of just the current
;;; line.  This could actually be slower than not using lazy-lock using the
;;; current code.  Perhaps there's an opportunity to take advantage of
;;; lazy-lock with alternate coding.

;;; Hacker's note: If this method leads to infinite loops again, I could
;;; change the cache method to something like:
;;;  - When the pattern is un-finished, simply store the limit in the cache.
;;;    and the regexp to match the termination.
;;;  - When checking the cache, check to see if we're at the limit, and if
;;;    so fontify the text directly like at point limit-1 (instead of
;;;    letting font-lock itself set the font!) until either the regexp match
;;;    is found or set another cache at the new limit
;;;  - the scheme must allow a newline to be correctly fontified, and well
;;;    as new characters on the same line as the first cache.  (How?)

;;; Hacker's note (2001-11-02) : It's possible that the caching system is
;;; no longer needed using font-lock-multiline in Emacs21.  I should
;;; disable it and try.  Also, now that I look at this, I wonder why I
;;; didn't use text-properties to be able to set many unterminated
;;; fontification matches in a given buffer.  Perhaps it was portability to
;;; XEmacs?

(defun font-latex-set-cache (cache-id kbeg kend limit keywords match-list)
  "Set cache for font-latex.
Caches the following info into CACHE-ID:
KBEG and KEND: beginning and end points of the LaTeX keyword (e.g. \"section\")
LIMIT:         up to where fontification is done.
KEYWORDS:      the font-lock regexp that initiated the cache.
MATCH-LIST:    the match list that was returned to font-lock

The INITIAL POINT from which we last moved is stored in the same cache, but
it's done elsewhere.  We will never fontify the same MATCH LIST twice in a
row from same INITIAL POINT."
;debug  (message "Setting cache!")
  (let ((ini-point (nth 5 (symbol-value cache-id)))
        (oldlimit (nth 6 (symbol-value cache-id))))
    (set cache-id
         (list kbeg kend limit keywords match-list ini-point oldlimit))))

(defun font-latex-get-cache (cache-id item)
  "Retrieve info from cache in symbol CACHE-ID.
Then ITEMs are:
 0: kbegin
 1: kend
 2: limit
 3: keywords
 4: match-list from last succesful cache
 5: initial point from which we last moved
 6: limit when we last moved"
  (let ((cache (symbol-value cache-id)))
    (nth item cache)))

(defun font-latex-check-cache (cache-id keywords limit)
  "Check that current parameters are consistent with cache to move point.
If we move point, alter the last entry in the cache to indicate from where
we moved and the current limit.
Return t if we move, false if we don't."
  (let ((the-point (point))
        (kbeg (font-latex-get-cache cache-id 0))
        (inip (or (font-latex-get-cache cache-id 5) 0))
        (oldlimit (or (font-latex-get-cache cache-id 6) 0)))
    (when
        (and
         font-latex-do-multi-line
         kbeg                           ;; Check that cache is actually set
         (equal keywords (font-latex-get-cache cache-id 3))
;debug   (message "1- cache: %s" (symbol-name cache-id))
;debug   (message "1- keywords are the same; next compare point %s to %s"
;debug            the-point (font-latex-get-cache cache-id 1))
         (not (= the-point (font-latex-get-cache cache-id 1)))
;debug   (message "2- Not on end of keyword %s != %s; next after kbeg %s"
;debug            the-point (font-latex-get-cache cache-id 1) kbeg)
         (< kbeg the-point)
;debug   (message "3- After beginning of keyword at %s; next within limit %s"
;debug            kbeg (font-latex-get-cache cache-id 2))
         (<= the-point (font-latex-get-cache cache-id 2))
;debug   (message "4- Within limit at %s" (font-latex-get-cache cache-id 2))
;debug   (message "5- Same limit as last time?: %s vs %s  Point greater? %s > %s"
;debug            limit oldlimit the-point inip)
         (or (< the-point inip) (not (= limit oldlimit)))
;debug   (message "6- Is %s on same line as %s?" the-point kbeg)
         (font-latex-not-on-same-line-as kbeg))
;debug   (message "7- moving from %s to %s!" the-point kbeg)
      (goto-char kbeg)
      (let* ((cache (symbol-value cache-id))
             (e0 kbeg)
             (e1 (nth 1 cache))
             (e2 (nth 2 cache))
             (e3 (nth 3 cache))
             (e4 (nth 4 cache)))
        (set cache-id (list e0 e1 e2 e3 e4 the-point limit)))
      t)))

;;;;-----

(defvar font-latex-match-command-cache nil
  "Cache for font-latex-match-command.")
(make-variable-buffer-local 'font-latex-match-command-cache)

;; FIXME - Note to myself
;; In call to font-latex-match-command-outside-arguments, I could arrange
;; such that keywords which cannot use [options] have this set to nil.
;; LaTeX code wouldn't fontify if options are used illegally in commands,
;; cuing users in that they are doing something wrong.  (See RCS V1.11 for
;; useopt option)
;;
;; NOTE - Without an override flag, font-lock does not re-fontify the
;;  option `opt' when the `t' is typed-in in "\cite[opt".  The first `o'
;;  was fontified and now has a face, which font-lock-apply-highlight
;;  won't override.  The `p' and `t' get a face as they are typed by
;;  inheriting from left-stickyness on the `o'.
;;  THEREFORE, I cannot rely on font-lock-apply-highlight to continue
;;  multi-line incomplete patterns, because the first character of the
;;  pattern on the first line has a face.  I must use `prepend'.
(defun font-latex-match-command-outside-arguments (keywords limit twoargs
                                                   asterix)
  "Search for regexp command KEYWORDS[opt]{arg} before LIMIT.
If TWOARGS is t, allow two arguments {arg1}{arg2}
If ASTERIX is t, fontify trailing asterix in command.
Sets `match-data' so that:
 subexpression 0 is the keyword,
 subexpression 1 is the contents of any following [...] forms
 subexpression 2 is the contents of any following {...} forms.
Returns nil if none of KEYWORDS is found."
;;(let ((we-moved (font-latex-check-cache
;;                 'font-latex-match-command-cache keywords limit)))
  (font-latex-check-cache 'font-latex-match-command-cache keywords limit)
    (when (re-search-forward keywords limit t)
      (cond
       ((font-latex-commented-outp)
        ;; Return a dummy match such that we skip over this pattern.
        ;; (Would be better to skip over internally to this function)
        ;; We used to return a (nil nil) pattern match along with the
        ;; status of `t' to keep looking.  font-lock was happy with that.
        ;; But when font-lock-multiline is `t', the match really needs to
        ;; exists otherwise there is a elisp error at line 1625 of
        ;; font-lock.el in function font-lock-fontify-keywords-region.
        (store-match-data (list (match-end 0)(match-end 0)))
        t)
       (t
        (let ((kbeg (match-beginning 0))
              kend sbeg send cbeg cend
              cache-reset
              (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
          (goto-char (match-end 0))
          (if (and asterix (eq (following-char) ?\*))
              (forward-char 1))
          (skip-chars-forward " \n\t" limit)
          (setq kend (point))
          (while (eq (following-char) ?\[)
            (setq sbeg kend)
            (save-restriction
              ;; Restrict to LIMIT.
              (narrow-to-region (point-min) limit)
              (if (font-latex-find-matching-close ?\[ ?\])
                  (setq send (point))
                (setq cache-reset t)
                (setq send (point-max))
                (goto-char send))))
          (skip-chars-forward " \n\t" limit)
          (when (eq (following-char) ?\{)
            (setq cbeg (point))
            (save-restriction
              ;; Restrict to LIMIT.
              (narrow-to-region (point-min) limit)
              (if (font-latex-find-matching-close ?\{ ?\})
                  (setq cend (point))
                (setq cache-reset t)
                (setq cend (point-max))
                (goto-char cend))))
          (when twoargs
            (skip-chars-forward " \n\t" limit)
            (when (eq (following-char) ?\{)
              (save-restriction
                ;; Restrict to LIMIT.
                (narrow-to-region (point-min) limit)
                (if (font-latex-find-matching-close ?\{ ?\})
                    (setq cend (point))
                  (setq cache-reset t)
                  (setq cend (point-max))
                  (goto-char cend)))))
          (store-match-data (list kbeg kend sbeg send cbeg cend))

          ;; Handle cache
;          (if (and we-moved
;                   (equal (list kbeg kend sbeg send cbeg cend)
;                          (font-latex-get-cache
;                           'font-latex-match-command-cache 4)))
;              (progn
;                (message "pattern cancelled... twice in a row")
;                nil) ;; Return a nul search (cancel this fontification)

          (when (and font-latex-do-multi-line cache-reset)
            (font-latex-set-cache
             'font-latex-match-command-cache
             kbeg kend limit keywords (list kbeg kend sbeg send cbeg cend)))
          t)))))

(defvar font-latex-match-font-cache nil
  "Cache start of unterminated LaTeX font-changing commands to fontify.")
(make-variable-buffer-local 'font-latex-match-font-cache)

(defun font-latex-match-font-outside-braces (limit)
  "Search for font-changing command like \textbf{fubar} before LIMIT.
Sets `match-data' so that:
 subexpression 0 is the keyword,
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (font-latex-check-cache 'font-latex-match-font-cache 'font limit)
  (when (re-search-forward
         (eval-when-compile
           (concat "\\\\" "\\("
                   "\\(emph\\)\\|"			      ;;; 2 - italic
                   "\\(text\\("
                               "\\(it\\|sl\\)\\|"	      ;;; 5 - italic
                               "\\(md\\|rm\\|sf\\|tt\\)\\|"   ;;; 6 - type
                               "\\(bf\\|sc\\|up\\)"	      ;;; 7 - bold
                          "\\)\\)\\|"
                   "\\(boldsymbol\\|pmb\\)\\|"		      ;;; 8 - bold
		   "\\(ensuremath\\)"                         ;;; 9 - math
                   "\\)" "{"))
         limit t)
    (cond
     ((font-latex-commented-outp)
      ;; Return a nul match such that we skip over this pattern.
      ;; (Would be better to skip over internally to this function)
      ;; Using `prepend' won't help here, because the problem is that
      ;; scan-sexp *fails* to find a commented-out matching bracket!
      (store-match-data (list (match-end 0)(match-end 0)))
      t)
     (t
      (let ((kbeg (match-beginning 0)) (kend (match-end 1))
            (beg  (1- (match-end 0)))   ;Include openning bracket
            end itbeg itend bfbeg bfend ttbeg ttend mathbeg mathend
            (parse-sexp-ignore-comments t) ; scan-sexps ignores comments
            cache-reset)
        (goto-char kend)
        (save-restriction
          ;; Restrict to LIMIT.
          (narrow-to-region (point-min) limit)
          (if (font-latex-find-matching-close ?\{ ?\})
              (setq end (point))
            (setq cache-reset t)
            (setq end (point-max))
            (goto-char end)))
        (cond ((or (match-beginning 2) (match-beginning 5))
               (setq itbeg beg    itend end))
              ((match-beginning 6)
               (setq ttbeg beg    ttend end))
	      ((match-beginning 9)
	       (setq mathbeg beg  mathend end))
              (t
               (setq bfbeg beg    bfend end)))
        (store-match-data
         (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend mathbeg mathend))
        ;; Start the subsequent search immediately after this keyword.
          (goto-char kend)

        (when (and font-latex-do-multi-line cache-reset)
          (goto-char limit)             ;Avoid infinite loops?
          (font-latex-set-cache
           'font-latex-match-font-cache
           kbeg kend limit 'font
           (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend)))

        t)))))

(defvar font-latex-match-infont-cache nil
  "Cache start of unterminated LaTeX font-changing commands to fontify.")
(make-variable-buffer-local 'font-latex-match-infont-cache)

(defun font-latex-match-font-inside-braces (limit)
  "Search for font-changing command like {\bf fubar} before LIMIT.
Sets `match-data' so that:
 subexpression 0 is the keyword.
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (font-latex-check-cache 'font-latex-match-infont-cache 'infont limit)
  (when (re-search-forward
         (eval-when-compile
           (concat "\\\\" "\\("
                                                              ;;; 2 - italic
                   "\\(em\\|it\\(shape\\)?\\|sl\\(shape\\)?\\)\\|"
	                                                      ;;; 5 - bold
                   "\\(bf\\(series\\)?\\|upshape\\|sc\\(shape\\)?\\)\\|"
                   "mdseries\\|tt\\(family\\)?\\|"
                   "sf\\(family\\)?\\|rm\\(family\\)?\\|"
                   "tiny\\|scriptsize\\|footnotesize\\|"
                   "small\\|normalsize\\|large\\|Large\\|LARGE\\|huge\\|Huge"
                   "\\)\\>[ \t]*"))
         limit t)
    (cond
     ((font-latex-commented-outp)
      ;; Return a nul match such that we skip over this pattern.
      ;; (Would be better to skip over internally to this function)
      ;; Using `prepend' won't help here, because the problem is that
      ;; scan-sexp *fails* to find a commented-out matching bracket!
      (store-match-data (list (match-end 0)(match-end 0)))
      t)
     (t
      (let ((kbeg (match-beginning 0)) (kend (match-end 1))
            (beg  (match-end 0))
            end itbeg itend bfbeg bfend ttbeg ttend
            cache-reset
            (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
        (goto-char kbeg)
        (cond
         ((not (eq (preceding-char) ?\{))
          ;; Fontify only the keyword as bf/it/type (no argument found).
          (cond ((match-beginning 2) (setq itbeg kbeg itend kend))
                ((match-beginning 5) (setq bfbeg kbeg bfend kend))
                (t                   (setq ttbeg kbeg ttend kend)))
          (goto-char (match-end 0))
          (store-match-data
           (list nil nil itbeg itend bfbeg bfend ttbeg ttend))
          t)
         (t
          ;; There's an opening bracket
          (save-restriction
            ;; Restrict to LIMIT.
            (narrow-to-region (point-min) limit)
            (forward-char -1)           ;Move on the opening bracket
            (if (font-latex-find-matching-close ?\{ ?\})
                (setq end (point))
              (setq cache-reset t)
              (setq end (point-max))
              (goto-char end))
            (cond ((match-beginning 2) (setq itbeg beg  itend end))
                  ((match-beginning 5) (setq bfbeg beg  bfend end))
                  (t                   (setq ttbeg beg  ttend end)))
            (store-match-data
             (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend))

            ;; Start the subsequent search immediately after this keyword.
            (goto-char kend)

            (when (and font-latex-do-multi-line cache-reset)
              (goto-char limit)             ;Avoid infinite loops?
              (font-latex-set-cache
               'font-latex-match-infont-cache
               kbeg kend limit 'infont
               (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend)))

            t))))))))

(defun font-latex-not-on-same-line-as (cache-start)
  "Return t if point is not on same line as CACHE-START."
  (save-excursion
    (not (= (progn (beginning-of-line)(point))
            (progn (goto-char cache-start) (beginning-of-line)(point))))))

;;; FIXME: Add caches for math-env, math-envII and quotations.
(defun font-latex-match-math-env (limit)
  "Match math pattern up to LIMIT.
Used for patterns like:
\\( F = ma \\)
\\ [ F = ma \\] but not \\\\ [len]"
  (when (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
    (goto-char (match-beginning 0))
    (if (eq (preceding-char) ?\\)       ; \\[ is not a math environment
        (progn
          (goto-char (match-end 0))
          (store-match-data (list (match-end 0)(match-end 0)))
          t)
      (let ((b1start (point)))
        (search-forward (cond ((match-beginning 1) "\\)")
                              (t                   "\\]"))
                        limit 'move)
        (let ((b2end (or (match-end 0) (point))))
          (store-match-data (list b1start b2end))
          t)))))

(defun font-latex-match-math-envII (limit)
  "Match math patterns up to LIMIT.
Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation}
The \\begin{equation} and \\end{equation are not fontified here."
  (when (re-search-forward
         (eval-when-compile
           (concat "\\\\begin{\\(\\(display\\)?math\\|equation\\|eqnarray"
                   "\\|gather\\|multline\\|align\\|x*alignat"
                   "\\)\\*?}"))
         limit t)
    (let ((beg (match-end 0)) end)
      (if (search-forward (concat "\\end{" (buffer-substring
                                            (match-beginning 1)(match-end 0)))
                          limit 'move)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun font-latex-match-quotation (limit)
  "Match quote patterns up to LIMIT.
Used for patterns like:
``this is a normal quote'' and these are multilingual quoted strings:
\"< french \"> and \"`german\"' quotes.
The quotes << french >> and 8-bit french are used if `font-latex-quotes' is
set to french, and >> german << (and 8-bit) are used if set to german."
  (when (re-search-forward font-latex-quote-regexp-beg limit t)
    (let ((beg (match-beginning 0)))
      (search-forward
       (cond ((match-beginning 1) (nth 0 font-latex-quote-end-list))
	     ((match-beginning 2) (nth 1 font-latex-quote-end-list))
	     ((match-beginning 3) (nth 2 font-latex-quote-end-list))
	     ((match-beginning 4) (nth 3 font-latex-quote-end-list))
	     ((match-beginning 5) (nth 4 font-latex-quote-end-list)))
       limit 'move)
      (store-match-data (list beg (point)))
      t)))

(defcustom font-latex-script-display '((raise -0.3) . (raise 0.3))
  "Display specification for subscript and superscript content.
The car is used for subscript, the cdr is used for superscripts."
  :group 'font-latex
  :type '(cons (choice (sexp :tag "Subscript form")
		       (const :tag "No lowering" nil))
	       (choice (sexp :tag "Superscript form")
		       (const :tag "No raising" nil))))

;; Copy and adaption of `tex-font-lock-suscript' from tex-mode.el in
;; GNU Emacs on 2004-07-07.
(defun font-latex-script (pos)
  "Return face and display spec for subscript and superscript content."
  (let* ((prop (get-text-property pos 'face))
	 (prop-list (if (listp prop) prop (list prop))))
    (unless (or (catch 'member
		  (dolist (item prop-list)
		    (when (memq item '(font-lock-constant-face
				       font-lock-builtin-face
				       font-lock-comment-face
				       font-latex-verbatim-face))
		      (throw 'member t))))
		;; Check for math (but only if we don't already know
		;; because of an existing fontification).
		(and (not (memq 'font-latex-math-face prop-list))
		     (not (save-match-data
			    (condition-case nil (texmathp) (error nil)))))
		;; Check for backslash quoting
		(let ((odd nil)
		      (pos pos))
		  (while (eq (char-before pos) ?\\)
		    (setq pos (1- pos) odd (not odd)))
		  odd))
      ;; Adding other text properties than `face' is supported by
      ;; `font-lock-apply-highlight' in CVS Emacsen since 2001-10-28 or
      ;; Emacs 21.4 respectively.  With the introduction of this feature
      ;; the variable `font-lock-extra-managed-props' was introduced and
      ;; serves here for feature checking.  XEmacs (CVS and 21.4.15)
      ;; currently (2004-08-18) does not support this feature.
      (let ((extra-props-flag (boundp 'font-lock-extra-managed-props)))
	(if (eq (char-after pos) ?_)
	    (if extra-props-flag
		`(face font-latex-subscript-face display
		       ,(car font-latex-script-display))
	      font-latex-subscript-face)
	  (if extra-props-flag
	      `(face font-latex-superscript-face display
		     ,(cdr font-latex-script-display))
	    font-latex-superscript-face))))))


;;; docTeX

(defvar font-latex-doctex-preprocessor-face
  'font-latex-doctex-preprocessor-face
  "Face used to highlight preprocessor directives in docTeX mode.")

(defface font-latex-doctex-preprocessor-face
  '((t (:inherit (list font-latex-doctex-documentation-face
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
  (append font-latex-keywords-2
	  '(("^%<[^>]*>" (0 font-latex-doctex-preprocessor-face t)))))

(defvar font-latex-doctex-syntactic-keywords
  (append
   font-latex-syntactic-keywords
   ;; For docTeX comment-in-doc.
   `(("\\(\\^\\)\\^A" (1 (font-latex-doctex-^^A))))))

;; Copy and adaptation of `doctex-font-lock-^^A' in `tex-mode.el' of
;; CVS Emacs (March 2004)
(defun font-latex-doctex-^^A ()
  (if (eq (char-after (line-beginning-position)) ?\%)
      (progn
	(put-text-property
	 (1- (match-beginning 1)) (match-beginning 1) 'syntax-table
	 (if (= (1+ (line-beginning-position)) (match-beginning 1))
	     ;; The `%' is a single-char comment, which Emacs
	     ;; syntax-table can't deal with.  We could turn it
	     ;; into a non-comment, or use `\n%' or `%^' as the comment.
	     ;; Instead, we include it in the ^^A comment.
	     ;; COMPATIBILITY for Emacs 20 and XEmacs
	     (eval-when-compile (if (fboundp 'string-to-syntax)
				    (string-to-syntax "< b")
				  '(2097163)))
	   ;; COMPATIBILITY for Emacs 20 and XEmacs
	   (eval-when-compile (if (fboundp 'string-to-syntax)
				  (string-to-syntax ">")
				'(12)))))
	(let ((end (line-end-position)))
	  (if (< end (point-max))
	      (put-text-property end (1+ end) 'syntax-table
				    ;; COMPATIBILITY for Emacs 20 and XEmacs
				    (eval-when-compile
				      (if (fboundp 'string-to-syntax)
					  (string-to-syntax "> b")
					'(2097164))))))
	;; COMPATIBILITY for Emacs 20 and XEmacs
	(eval-when-compile (if (fboundp 'string-to-syntax)
			       (string-to-syntax "< b")
			     '(2097163))))))

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

(add-hook 'latex-mode-hook 'font-latex-setup)
;; If font-latex is loaded using a latex-mode-hook, then the add-hook above
;; won't be called this time around.  Check for this now:
(if (eq major-mode 'latex-mode)
    (font-latex-setup))

;; Provide ourselves:
(provide 'font-latex)

;;; font-latex.el ends here
