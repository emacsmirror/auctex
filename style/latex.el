;;; @ latex.el - Special code for LaTeX.
;;;
;;; $Id: latex.el,v 1.2 1993-03-18 06:02:16 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "latex"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("document" LaTeX-document-hook)
     '("enumerate" LaTeX-item-hook)
     '("itemize" LaTeX-item-hook)
     '("list" LaTeX-list-hook)
     '("trivlist" LaTeX-item-hook)
     '("picture" LaTeX-picture-hook)
     '("tabular" LaTeX-array-hook)
     '("tabular*" LaTeX-array-hook)
     '("array" LaTeX-array-hook)
     '("eqnarray" LaTeX-label-hook)
     '("eqnarray*" LaTeX-label-hook)
     '("equation" LaTeX-label-hook)
     '("minipage" LaTeX-minipage-hook)

     ;; The following have no special support, but are included in
     ;; case the auto files are missing. 

     "sloppypar" "picture" "tabbing" "verbatim" "verbatim*"
     "flushright" "flushleft" "displaymath" "math"

     ;; The following are not defined in latex.el, but in a number of
     ;; other style files.  I'm to lazy to copy them to all the
     ;; corresponding .el files right now.
     
     ;; This means that AUC TeX will complete e.g.
     ;; ``thebibliography'' in a letter, but I guess we can live with
     ;; that.  
     
     '("description" LaTeX-item-hook)
     '("figure" LaTeX-figure-hook)
     '("figure*" LaTeX-figure-hook)
     '("table" LaTeX-figure-hook)
     '("table*" LaTeX-figure-hook)
     '("thebibliography" LaTeX-bib-hook))

    (TeX-add-symbols
     '("addtocounter" TeX-argument-counter-hook "Value")
     '("alph" TeX-argument-counter-hook)
     '("arabic" TeX-argument-counter-hook)
     '("fnsymbol" TeX-argument-define-counter-hook)
     '("newcounter" TeX-argument-define-counter-hook
       [ TeX-argument-counter-hook "Within counter" ])
     '("roman" TeX-argument-counter-hook)
     '("setcounter" TeX-argument-counter-hook "Value")
     '("usecounter" TeX-argument-counter-hook)
     '("value" TeX-argument-counter-hook)
     '("stepcounter" TeX-argument-counter-hook)
     '("refstepcounter" TeX-argument-counter-hook)
     '("label" TeX-argument-define-label-hook)
     '("pageref" TeX-argument-label-hook)
     '("ref" TeX-argument-label-hook)
     '("newcommand" TeX-argument-define-macro-hook [ "Number of arguments" ] t)
     '("renewcommand" TeX-argument-macro-hook [ "Number of arguments" ] t)
     '("newenvironment" TeX-argument-define-environment-hook
       [ "Number of arguments"] t t)
     '("renewenvironment" TeX-argument-environment-hook
       [ "Number of arguments"] t t)
     '("newtheorem" TeX-argument-define-environment-hook
       [ TeX-argument-environment-hook "Numbered like" ]
       t [ TeX-argument-counter-hook "Within counter" ])
     '("newfont" TeX-argument-define-macro-hook t)
     '("circle" "Diameter")
     '("circle*" "Diameter")
     '("dashbox" "Dash Length" TeX-argument-size-hook
       [ TeX-argument-corner-hook ] t)
     '("frame" t)
     '("framebox" (TeX-argument-conditional-hook 
		   (string-equal (LaTeX-current-environment) "picture")
		   (TeX-argument-size-hook [ TeX-argument-corner-hook ] t)
		   ([ "Length" ] [ TeX-argument-lr-hook ] t)))
     '("line" (TeX-argument-pair-hook "X slope" "Y slope") "Length")
     '("linethickness" "Dimension")
     '("makebox" (TeX-argument-conditional-hook 
		  (string-equal (LaTeX-current-environment) "picture")
		  (TeX-argument-size-hook [ TeX-argument-corner-hook ] t)
		  ([ "Length" ] [ TeX-argument-lr-hook ] t)))
     '("multiput"
       TeX-argument-coordinate-pair
       (TeX-argument-pair-hook "X delta" "Y delta")
       "Number of copies"
       t)
     '("oval" TeX-argument-size-hook [ TeX-argument-corner-hook "Portion" ])
     '("put" TeX-argument-coordinate-pair t)
     '("savebox" TeX-argument-define-savebox-hook
       (TeX-argument-conditional-hook
	(string-equal (LaTeX-current-environment) "picture")
	(TeX-argument-size-hook [ TeX-argument-corner-hook ] t)
	([ "Length" ] [ TeX-argument-lr-hook ] t)))
     '("shortstack" [ TeX-argument-lr-hook ] t)
     '("vector" (TeX-argument-pair-hook "X slope" "Y slope") "Length")
     '("cline" "Span `i-j'")
     '("multicolumn" "Columns" "Position" t)
     '("item" [ "Item label" ])
     '("bibitem" [ "Bibitem label" ] TeX-argument-define-cite-hook)
     '("cite" [ "Note" ] TeX-argument-cite-hook)
     '("nocite" TeX-argument-cite-hook)
     '("bibliographystyle" TeX-argument-bibstyle-hook)
     '("bibliography" TeX-argument-bibligraphy-hook)
     '("footnote" [ "Number" ] t)
     '("footnotetext" [ "Number" ] t)
     '("footnotemark" [ "Number" ])
     '("newlength" TeX-argument-define-macro-hook)
     '("setlength" TeX-argument-macro-hook "Length")
     '("addtolength" TeX-argument-macro-hook "Length")
     '("settowidth" TeX-argument-macro-hook t)
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
     '("pagenumbering" (TeX-argument-eval-hook
			completing-read "Numbering style: "
			'(("arabic") ("roman") ("Roman") ("alph") ("Alph"))))
     '("pagestyle" TeX-argument-pagestyle-hook)
     '("markboth" t nil)
     '("markright" t)
     '("thispagestyle" TeX-argument-pagestyle-hook)
     '("addvspace" "Length")
     '("fbox" t)
     '("hspace*" "Length")
     '("hspace" "Length")
     '("mbox" t)
     '("newsavebox" TeX-argument-define-savebox-hook)
     '("parbox" [ TeX-argument-bt-hook] "Width" t)
     '("raisebox" "Raise" [ "Height above" ] [ "Depth below" ] t)
     '("rule" [ "Raise" ] "Width" "Thickness")
     '("sbox" TeX-argument-define-savebox-hook t)
     '("usebox" TeX-argument-savebox-hook)
     '("vspace*" "Length")
     '("vspace" "Length")
     '("include" TeX-argument-input-file-hook)
     '("includeonly" t)
     '("input" TeX-argument-input-file-hook)
     '("addcontentsline" TeX-argument-file-hook
       (TeX-argument-eval-hook
	completing-read "Numbering style: " LaTeX-section-list)
       t)
     '("addtocontents" TeX-argument-file-hook t)
     '("typeout" t)
     '("typein" [ TeX-argument-define-macro-hook ] t)
     '("verb" TeX-argument-verb-hook)
     '("verb*" TeX-argument-verb-hook)
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

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
