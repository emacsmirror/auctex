;;; font-latex.el --- LaTeX fontification for Font Lock mode.

;; Copyright (C) 1996 Peter S. Galbraith
 
;; Authors:    Peter S. Galbraith <galbraith@mixing.qc.dfo.ca>
;;             Simon Marshall <Simon.Marshall@esrin.esa.it>
;; Maintainer: Peter S. Galbraith <galbraith@mixing.qc.dfo.ca>
;; Created:    06 July 1996
;; Version:    0.306 *Beta* (14 Aug 96)
;; Keywords:   LaTeX faces

;; RCS $Id: font-latex.el,v 5.1 1996-08-14 17:45:09 abraham Exp $
;; Note: RCS version number does not correspond to release number.

;; LCD Archive Entry: (Not yet submitted!)
;; font-latex|Peter Galbraith|galbraith@mixing.qc.dfo.ca|
;; LaTeX fontification for font-lock|
;; 06-Jul-1996|0.01|~/modes/font-latex.el|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

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

;; New versions of this package (if they exist) may be found at:
;;  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/font-latex.el

;; Description:
;;  This package enhances font-lock fontification patterns for LaTeX.
;;
;;  Please see the accompanying file font-latex.tex for a demo of what
;;  it's supposed to do at different fontification levels.

;; Installation instructions:
;;
;;  Put this file in your emacs load-path, and byte-compile it:
;;     M-x byte-compile-file
;;  ** It runs faster when you byte-compile it! **
;;
;;  Then all you need to do is add this form to your .emacs file:
;;
;;    (if window-system
;;        (require 'font-latex))
;;
;;  There are two levels of fontification, selected by the value of
;;  the font-lock variable font-lock-maximum-decoration.
;; ----------------------------------------------------------------------------
;;; Change log:
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

(defvar font-latex-warning-face			'font-latex-warning-face
  "Face to use for LaTeX major keywords.")
(defvar font-latex-sedate-face			'font-latex-sedate-face
  "Face to use for LaTeX minor keywords.")
(defvar font-latex-math-face			'font-lock-string-face
  "Face to use for LaTeX math environments.")
(defvar font-latex-italic-face			'font-latex-italic-face
  "Face to use for LaTeX italics.")
(defvar font-latex-bold-face			'font-latex-bold-face
  "Face to use for LaTeX bolds.")

;; End-User can stop reading here.
(require 'font-lock)

(defvar font-latex-is-XEmacs
  (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))))

;; Make sure font-latex.el is supported.  I don't claim to have tested this...
(if (if (save-match-data (string-match "Lucid\\|XEmacs" (emacs-version)))
	(and (= emacs-major-version 19) (< emacs-minor-version 14))
      (and (= emacs-major-version 19) (< emacs-minor-version 29)))
    (error "`font-latex' was written for Emacs 19.29/XEmacs 19.14 or later"))

;;In 19.31: Symbol's value as variable is void: font-lock-comment-start-regexp
(defvar font-lock-comment-start-regexp nil
  "*Regexp to match the start of a comment.
This need not discriminate between genuine comments and quoted comment
characters or comment characters within strings.
If nil, `comment-start-skip' is used instead; see that variable for more info.
This is normally set via `font-lock-defaults'.")

(eval-when-compile
  (require 'cl))

(cond
 ((not font-latex-is-XEmacs)
  ;;; emacs:
  ;; Otherwise I overwrite fock-lock-face-attributes.
  ;; font-lock.el needs a better way to add these faces!        
  (if (not font-lock-face-attributes)
      (font-lock-make-faces))
  (unless (assq 'font-latex-sedate-face font-lock-face-attributes)
    (setq font-lock-face-attributes
          (append font-lock-face-attributes
                  ;;FIXME: These won't follow font-lock-type-face's changes.
                  ;;       Should I change to a (copy-face) scheme?
                  '((font-latex-bold-face "DarkOliveGreen" nil t nil nil)
                    (font-latex-italic-face "DarkOliveGreen" nil nil t nil)
                    (font-latex-sedate-face "grey50")
                    (font-latex-warning-face "red" nil t nil nil))))))
 (t
  ;;; XEmacs:
  (make-face 'font-latex-math-face "Face to use for LaTeX maths.")
  (copy-face 'font-lock-string-face 'font-latex-math-face)
  (make-face 'font-latex-bold-face "Face to use for LaTeX bolds.")
  (copy-face 'font-lock-type-face 'font-latex-bold-face)
  (make-face-bold 'font-latex-bold-face)
  (make-face 'font-latex-italic-face "Face to use for LaTeX italics.")
  (copy-face 'font-lock-type-face 'font-latex-italic-face)
  (make-face-italic 'font-latex-italic-face)
  (make-face 'font-latex-sedate-face "Face to use for LaTeX minor keywords.")
  (set-face-foreground 'font-latex-sedate-face "grey50" 'global nil 'append)
  (make-face 'font-latex-warning-face "Face to use for LaTeX major keywords.")
  (make-face-bold 'font-latex-warning-face)
  (set-face-foreground 'font-latex-warning-face "red" 'global nil 'append)))


(defun font-latex-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
          nil nil ((?$ . "\"")) nil
          (font-lock-comment-start-regexp . "%")
          (font-lock-mark-block-function . mark-paragraph))))

(add-hook 'LaTeX-mode-hook 'font-latex-setup)
(add-hook 'latex-mode-hook 'font-latex-setup)

(defconst font-latex-keywords-1
  (list
   (list (concat "\\\\\\(\\(no\\)?pagebreak\\|\\(new\\|clear\\(double\\)?\\)"
		 "page\\|enlargethispage\\|\\(no\\)?linebreak\\|newline\\|"
		 "-\\|\\\\\\(\*\\)?\\|displaybreak\\|allowdisplaybreaks\\)")
	 0 'font-latex-warning-face t)
   '("\\$\\$\\([^$]+\\)\\$\\$" 1 font-lock-string-face)        ;; $$...$$
   '(font-latex-match-quotation . font-lock-string-face)       ;; ``...''
   '(font-latex-match-font-outside-braces		       ;;\textit{text}
     (0 font-lock-keyword-face t)
     (1 font-latex-italic-face prepend t)
     (2 font-latex-bold-face prepend t)
     (3 font-lock-type-face prepend t))
   '(font-latex-match-font-inside-braces		       ;;{\it text}
     (0 font-lock-keyword-face t t)
     (1 font-latex-italic-face prepend t)
     (2 font-latex-bold-face prepend t)
     (3 font-lock-type-face prepend t)))  
  "Subdued level highlighting for LaTeX modes.")

(defconst font-latex-keywords-2
  (append font-latex-keywords-1
   '((font-latex-match-reference                               ;;\cite
      (0 font-lock-keyword-face)
      (1 font-lock-variable-name-face                          ;;    [opt]
         nil                             ;Override? [t 'keep 'prepend 'append]
         t)                              ;Laxmatch? if t, do not signal error
      (2 font-lock-reference-face nil t))                      ;;         {key}
     (font-latex-match-function                                ;;\section
      (0 font-lock-keyword-face)
      (1 font-lock-variable-name-face nil t)                   ;;   [opt]
      (2 font-lock-function-name-face prepend t))              ;;        {text}
     (font-latex-match-variable
      (0 font-lock-keyword-face)
      (1 font-lock-variable-name-face nil t)
      (2 font-lock-variable-name-face nil t))
     (font-latex-match-math-env . font-latex-math-face)	       ;;\(...\)
     (font-latex-match-math-envII                              ;;Math environ.
      (0 font-latex-math-face keep t))      
          ;;FIXME: In XEmacs, this is overriding stranded \bf commands
     ;;       Should OVERRIDE flag be different on either?
     ;;       Or is XEmacs broken yet again?
     ("\\\\\\sw+" 0 font-latex-sedate-face)))                  ;;Other commands
  "High level highlighting for LaTeX modes.")

(defvar font-latex-keywords font-latex-keywords-1
  "Default expressions to highlight in TeX mode.")


(defun font-latex-match-reference (limit)
  (font-latex-match-command-outside-arguments
   (eval-when-compile
     (concat "\\\\" "\\("
	     (mapconcat 'identity 
	      '("[A-Za-z]*cite[A-Za-z]*" "label" "\\(page\\|v\\|eq\\)?ref"
		"index" "glossary" "\\(footnote\\(mark\\|text\\)?\\)")
	      "\\|")
      "\\)\\>"))
   limit nil))

(defun font-latex-match-function (limit)
  "Fontify things like \\section{text}"
  (font-latex-match-command-outside-arguments
   (eval-when-compile
     (concat "\\\\" "\\("
      (mapconcat 'identity 
       '("item" ;;;FIXME: does not have an {arg} so should treated elsewhere.
         "include" "input" "bibliography" 
	 "part" "chapter" "\\(sub\\)*section" "\\(sub\\)*paragraph"
	 "begin" "end"
	 "title" "author" "date" "thanks" "address"
	 "pagenumbering"
	 "\\(this\\)?pagestyle"
	 "nofiles" "includeonly"
	 "bibliographystyle" "\\(document\\(style\\|class\\)\\)"
         "\\(re\\)?new\\(environment\\|command\\|length\\|theorem\\|counter\\)"
	 "usepackage" "caption" "\\(f\\|m\\|s\\)box" "\\(v\\|h\\)space\\*?")
       "\\|")
      "\\)\\>"))
   limit nil))

(defun font-latex-match-variable (limit)
  "Fontify things like \\newcommand{stuff}"
  (font-latex-match-command-outside-arguments
   (eval-when-compile
     (concat "\\\\" "\\("
	     "set\\(length\\|towidth\\|counter\\)\\|"
	     "addto\\(length\\|counter\\)"
             "\\)\\>"))
   limit t))

;; FIXME - Note to myself 
;; In call to font-latex-match-command-outside-arguments, I could arrange
;; such that keywords which cannot use [options] have this set to nil.
;; LaTeX code woulldn't fontify if options are used illegally in commands,
;; cuing users in that they are doing something wrong.  (See RCS V1.11 for
;; useopt option)
(defun font-latex-match-command-outside-arguments (keywords limit twoargs)
  "Search for regexp command KEYWORDS[opt]{arg} before LIMIT.
If TWOARG is t, allow two arguments {arg1}{arg2}
Sets `match-data' so that:
 subexpression 0 is the keyword, 
 subexpression 1 is the contents of any following [...] forms 
 subexpression 2 is the contents of any following {...} forms.  
Returns nil if none of KEYWORDS is found."
  (when (re-search-forward keywords limit t)
    (let ((kbeg (match-beginning 0)) (kend (match-end 0)) sbeg send cbeg cend)
      (while (eq (following-char) ?\[)
	(save-restriction
	  ;; Restrict to LIMIT.
	  (narrow-to-region (point-min) limit)
	  (setq sbeg (1+ kend))
	  (if (condition-case nil
		  (goto-char (or (scan-sexps (point) 1) (point-max)))
		(error))
	      (setq send (1- (point)))
	    (setq send (point-max))
            (goto-char send))))
      (when (eq (following-char) ?\{)
	(save-restriction
	  ;; Restrict to LIMIT.
	  (narrow-to-region (point-min) limit)
	  (setq cbeg (1+ (point)))
	  (if (condition-case nil
		  (goto-char (or (scan-sexps (point) 1) (point-max)))
		(error))
	      (setq cend (1- (point)))
	    (setq cend (point-max))
            (goto-char cend))))
      (when (and twoargs (eq (following-char) ?\{))
	(save-restriction
	  ;; Restrict to LIMIT.
	  (narrow-to-region (point-min) limit)
	  (if (condition-case nil
		  (goto-char (or (scan-sexps (point) 1) (point-max)))
		(error))
	      (setq cend (1- (point)))
	    (setq cend (point-max))
            (goto-char cend))))
      (store-match-data (list kbeg kend sbeg send cbeg cend))
      t)))

(defun font-latex-match-font-outside-braces (limit)
  "Search for font-changing command like \textbf{fubar} before LIMIT.  
Sets `match-data' so that:
 subexpression 0 is the keyword, 
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (when (re-search-forward
	 (eval-when-compile
	   (concat "\\\\" "\\("
		   "\\(emph\\)\\|"				;;; 2 - italic
		   "\\(text\\("
			      "\\(it\\|sl\\)\\|"		;;; 5 - italic
			      "\\(md\\|rm\\|sf\\|tt\\)\\|"	;;; 6 - type
			      "\\(bf\\|sc\\|up\\)"		;;; 7 - bold
		   "\\)\\)\\|"
		   "\\(boldsymbol\\|pmb\\)"			;;; 8 - bold
		   "\\)" "{"))
	 limit t)
    (let ((kbeg (match-beginning 0)) (kend (match-end 1)) 
	  (beg  (match-end 0)) end itbeg itend bfbeg bfend ttbeg ttend)
      (goto-char kend)
      (save-restriction
	;; Restrict to LIMIT.
	(narrow-to-region (point-min) limit)
	(if (condition-case nil
		(goto-char (or (scan-sexps (point) 1) (point-max)))
	      (error))
	    (setq end (1- (point)))
	  (setq end (point-max))
          (goto-char end)))
      (cond ((or (match-beginning 2) (match-beginning 5))
	     (setq itbeg beg
		   itend end))
	    ((match-beginning 6)
	     (setq ttbeg beg
		   ttend end))
	    (t
	     (setq bfbeg beg
		   bfend end)))
      (store-match-data (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend))
      ;; Start the subsequent search immediately after this keyword.
      (goto-char kend))))

(defun font-latex-match-font-inside-braces (limit)
  "Search for font-changing command like {\bf fubar} before LIMIT.  
Sets `match-data' so that:
 subexpression 0 is the keyword. 
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (when (re-search-forward
	 (eval-when-compile
	   (concat "\\\\" "\\("
		   "\\(em\\|it\\(shape\\)?\\|"
			"sl\\(shape\\)?\\)\\|"			;;; 2 - italic
		   "\\(bf\\(series\\)?\\|upshape\\|"
			"sc\\(shape\\)?\\)\\|"			;;; 5 - bold
		   "mdseries\\|tt\\(family\\)?\\|"
			"sf\\(family\\)?\\|rm\\(family\\)?\\|"
			"tiny\\|scriptsize\\|footnotesize\\|"
			"small\\|normalsize\\|large\\|Large\\|LARGE\\|"
                        "huge\\|Huge"
		   "\\)\\>[ \t]*"))
	 limit t)
    (let ((kbeg (match-beginning 0)) (kend (match-end 1)) 
	  (beg  (match-end 0)) end itbeg itend bfbeg bfend ttbeg ttend)
      (goto-char (match-beginning 0))
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
        (condition-case nil
            (forward-char -1)
          (error))
        (save-restriction
          ;; Restrict to LIMIT.
          (narrow-to-region (point-min) limit)
          (if (condition-case nil
                  (goto-char (or (scan-sexps (point) 1) (point-max)))
                (error))
              (setq end (1- (point)))
            (setq end (point-max))
            (goto-char end)))
        (cond ((match-beginning 2) (setq itbeg beg itend end))
              ((match-beginning 5) (setq bfbeg beg bfend end))
              (t             	   (setq ttbeg beg ttend end)))
        (store-match-data (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend))
        ;; Start the subsequent search immediately after this keyword.
        (goto-char kend))))))

(defun font-latex-match-math-env (limit)
  "Used for patterns like:
\\( F = ma \\)
\\ [ F = ma \\] but not \\\\ [len]"
  (if (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
      (if (eq (preceding-char) ?\\)       ; \\[ is not a math environment
          (progn
            (store-match-data (list nil nil))
            t)
        (let ((b1start (match-beginning 0)))
          (search-forward (cond ((match-beginning 1) "\\)")
                                (t                   "\\]"))
                          limit 'move)
          (let ((b2end (or (match-end 0) (point))))
            (store-match-data (list b1start b2end))
            t)))
    nil))

(defun font-latex-match-math-envII (limit)
  "Used for patterns like:
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
    (let ((beg (match-end 0)))
      (search-forward 
       (concat "\\end{" (buffer-substring (match-beginning 1) (match-end 0)))
       limit 'move)
      (store-match-data (list beg (match-beginning 0)))
      t)))

(defun font-latex-match-quotation (limit)
  "Used for patterns like:
``this is a normal quote'' and these are multilingual quoted strings:
\"< french \"> and \"`german\"' quotes, << french >> and 8-bit french."
  (when (re-search-forward
	 (eval-when-compile
	   (concat "\\(``\\)\\|\\(\"<\\)\\|\\(\"`\\)\\|\\(<<\\)\\|"
		   "\\(" (char-to-string 171) "\\)")) ; An 8-bit "<<"
	 limit t)
    (let ((beg (match-beginning 0)))
      (search-forward
       (cond ((match-beginning 1) "''")
	     ((match-beginning 2) "\">")
	     ((match-beginning 3) "\"'")
	     ((match-beginning 4) ">>")
	     ((match-beginning 5) (eval-when-compile (char-to-string 187))))
       limit 'move)
      (store-match-data (list beg (point)))
      t)))

(provide 'font-latex)
;;; font-latex.el ends here
