;;; latex.el - Special code for LaTeX.

;; $Id: latex.el,v 1.9 1993-11-18 20:13:33 amanda Exp $

;;; Code:

(defvar TeX-arg-cite-note-p nil
  "*If non-nil, ask for optional note in citations.")

(defvar TeX-arg-footnote-number-p nil
  "*If non-nil, ask for optional number in footnotes.")

(defvar TeX-arg-item-label-p nil
  "*If non-nil, always ask for optional label in items.
Otherwise, only ask in description environments.")

(TeX-add-style-hook "latex"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("document" LaTeX-env-document)
     '("enumerate" LaTeX-env-item)
     '("itemize" LaTeX-env-item)
     '("list" LaTeX-env-list)
     '("trivlist" LaTeX-env-item)
     '("picture" LaTeX-env-picture)
     '("tabular" LaTeX-env-array)
     '("tabular*" LaTeX-env-array)
     '("array" LaTeX-env-array)
     '("eqnarray" LaTeX-env-label)
     '("eqnarray*" LaTeX-env-label)
     '("equation" LaTeX-env-label)
     '("minipage" LaTeX-env-minipage)

     ;; The following have no special support, but are included in
     ;; case the auto files are missing. 

     "sloppypar" "picture" "tabbing" "verbatim" "verbatim*"
     "flushright" "flushleft" "displaymath" "math" "quote" "quotation"
     "abstract" "center" "titlepage" "verse"
     
     ;; The following are not defined in latex.el, but in a number of
     ;; other style files.  I'm to lazy to copy them to all the
     ;; corresponding .el files right now.
     
     ;; This means that AUC TeX will complete e.g.
     ;; ``thebibliography'' in a letter, but I guess we can live with
     ;; that.  
     
     '("description" LaTeX-env-item)
     '("figure" LaTeX-env-figure)
     '("figure*" LaTeX-env-figure)
     '("table" LaTeX-env-figure)
     '("table*" LaTeX-env-figure)
     '("thebibliography" LaTeX-env-bib)
     '("theindex" LaTeX-env-item))

    (TeX-add-symbols
     '("addtocounter" TeX-arg-counter "Value")
     '("alph" TeX-arg-counter)
     '("arabic" TeX-arg-counter)
     '("fnsymbol" TeX-arg-define-counter)
     '("newcounter" TeX-arg-define-counter
       [ TeX-arg-counter "Within counter" ])
     '("roman" TeX-arg-counter)
     '("setcounter" TeX-arg-counter "Value")
     '("usecounter" TeX-arg-counter)
     '("value" TeX-arg-counter)
     '("stepcounter" TeX-arg-counter)
     '("refstepcounter" TeX-arg-counter)
     '("label" TeX-arg-define-label)
     '("pageref" TeX-arg-label)
     '("ref" TeX-arg-label)
     '("newcommand" TeX-arg-define-macro [ "Number of arguments" ] t)
     '("renewcommand" TeX-arg-macro [ "Number of arguments" ] t)
     '("newenvironment" TeX-arg-define-environment
       [ "Number of arguments"] t t)
     '("renewenvironment" TeX-arg-environment
       [ "Number of arguments"] t t)
     '("newtheorem" TeX-arg-define-environment
       [ TeX-arg-environment "Numbered like" ]
       t [ TeX-arg-counter "Within counter" ])
     '("newfont" TeX-arg-define-macro t)
     '("circle" "Diameter")
     '("circle*" "Diameter")
     '("dashbox" "Dash Length" TeX-arg-size
       [ TeX-arg-corner ] t)
     '("frame" t)
     '("framebox" (TeX-arg-conditional 
		   (string-equal (LaTeX-current-environment) "picture")
		   (TeX-arg-size [ TeX-arg-corner ] t)
		   ([ "Length" ] [ TeX-arg-lr ] t)))
     '("line" (TeX-arg-pair "X slope" "Y slope") "Length")
     '("linethickness" "Dimension")
     '("makebox" (TeX-arg-conditional 
		  (string-equal (LaTeX-current-environment) "picture")
		  (TeX-arg-size [ TeX-arg-corner ] t)
		  ([ "Length" ] [ TeX-arg-lr ] t)))
     '("multiput"
       TeX-argument-coordinate-pair
       (TeX-arg-pair "X delta" "Y delta")
       "Number of copies"
       t)
     '("oval" TeX-arg-size [ TeX-arg-corner "Portion" ])
     '("put" TeX-argument-coordinate-pair t)
     '("savebox" TeX-arg-define-savebox
       (TeX-arg-conditional
	(string-equal (LaTeX-current-environment) "picture")
	(TeX-arg-size [ TeX-arg-corner ] t)
	([ "Length" ] [ TeX-arg-lr ] t)))
     '("shortstack" [ TeX-arg-lr ] t)
     '("vector" (TeX-arg-pair "X slope" "Y slope") "Length")
     '("cline" "Span `i-j'")
     '("multicolumn" "Columns" "Position" t)
     '("item"
       (TeX-arg-conditional (or TeX-arg-item-label-p
				(string-equal (LaTeX-current-environment)
					      "description"))
			    ([ "Item label" ])
			    ())
       (TeX-arg-literal " "))
     '("bibitem" [ "Bibitem label" ] TeX-arg-define-cite)
     '("cite"
       (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) ())
       TeX-arg-cite)
     '("nocite" TeX-arg-cite)
     '("bibliographystyle" TeX-arg-bibstyle)
     '("bibliography" TeX-arg-bibligraphy)
     '("footnote"
       (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
       t)
     '("footnotetext" 
       (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
       t)
     '("footnotemark" 
       (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil))
     '("newlength" TeX-arg-define-macro)
     '("setlength" TeX-arg-macro "Length")
     '("addtolength" TeX-arg-macro "Length")
     '("settowidth" TeX-arg-macro t)
     '("\\" [ "Space" ])
     '("\\*" [ "Space" ])
     '("hyphenation" t)
     '("linebreak" [ "How much [0 - 4]" ])
     '("nolinebreak" [ "How much [0 - 4]" ])
     '("nopagebreak" [ "How much [0 - 4]" ])
     '("pagebreak" [ "How much [0 - 4]" ])
     '("stackrel" t nil)
     '("frac" t nil)
     '("lefteqn" t)
     '("overbrace" t)
     '("overline" t)
     '("sqrt" [ "Root" ] t)
     '("underbrace" t)
     '("underline" t)
     '("author" t)
     '("date" t)
     '("thanks" t)
     '("title" t)
     '("pagenumbering" (TeX-arg-eval
			completing-read "Numbering style: "
			'(("arabic") ("roman") ("Roman") ("alph") ("Alph"))))
     '("pagestyle" TeX-arg-pagestyle)
     '("markboth" t nil)
     '("markright" t)
     '("thispagestyle" TeX-arg-pagestyle)
     '("addvspace" "Length")
     '("fbox" t)
     '("hspace*" "Length")
     '("hspace" "Length")
     '("mbox" t)
     '("newsavebox" TeX-arg-define-savebox)
     '("parbox" [ TeX-arg-tb] "Width" t)
     '("raisebox" "Raise" [ "Height above" ] [ "Depth below" ] t)
     '("rule" [ "Raise" ] "Width" "Thickness")
     '("sbox" TeX-arg-define-savebox t)
     '("usebox" TeX-arg-savebox)
     '("vspace*" "Length")
     '("vspace" "Length")
     '("usepackage" [ "Options" ] (TeX-arg-input-file "Package"))
     '("documentstyle" TeX-arg-document)
     '("documentclass" TeX-arg-document)
     '("include" (TeX-arg-input-file "File" t))
     '("includeonly" t)
     '("input" TeX-arg-input-file)
     '("addcontentsline" TeX-arg-file
       (TeX-arg-eval
	completing-read "Numbering style: " LaTeX-section-list)
       t)
     '("addtocontents" TeX-arg-file t)
     '("typeout" t)
     '("typein" [ TeX-arg-define-macro ] t)
     '("verb" TeX-arg-verb)
     '("verb*" TeX-arg-verb)
     '("extracolsep" t)
     '("index" t)
     '("glossary" t)
     '("numberline" "Section number" "Heading")
     '("caption" t)
     '("marginpar" [ "Left margin text" ] "Text")
     
     ;; These have no special support, but are included in case the
     ;; auto files are missing. 

     "LaTeX" "SLiTeX" "samepage" "newline" "smallskip" "medskip"
     "bigskip" "stretch" "nonumber" "centering" "raggedright"
     "raggedleft" "kill" "pushtabs" "poptabs" "protect" "arraystretch"
     "hline" "vline" "cline" "thinlines" "thicklines" "and" "makeindex"
     "makeglossary" "reversemarginpar" "normalmarginpar"
     "raggedbottom" "flushbottom" "sloppy" "fussy" "newpage"
     "clearpage" "cleardoublepage" "twocolumn" "onecolumn"))))

;;; latex.el ends here
