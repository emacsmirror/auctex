;;; context-en.el --- Support for the ConTeXt english interface.
;;
;; Maintainer: Berend de Boer <berend@pobox.com>
;; Version: 11.14
;; Keywords: wp
;; X-URL: http://www.gnu.org/software/auctex/
;; Copyright 2003 Free Software Foundation

;; Last Change: $Date: 2004-04-15 22:46:26 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Notes:

;; This file is loaded by context.el when required.


;; Build upon ConTeXt
(require 'context)


;;; ConText macro names

(defvar ConTeXt-environment-list-en
  '("alignment" "appendices"
	 "background" "backmatter" "bodymatter" "bodypart" "buffer"
	 "color" "columns" "combination"
	 "encoding" "extroductions"
	 "fact" "formula" "framedtext" "frontmatter"
	 "helptext" "hiding"
	 "itemize"
	 "legend" "line" "linecorrection" "linenumbering" "lines" "localenvironment" "localfootnotes"
		"makeup" "mapping" "marginblock" "marginedge" "marginrule" "mode"
		"narrower" "notmode"
		"opposite"
		"packed" "positioning" "postponing"
		"quotation"
		"raster" "register"
		"standardmakeup"
		"table" "tabulate" "text" "textbackground" "typing"
		"unpacked"
		;; project structure
		"component" "environment" "product" "project"
		;; flowcharts, if you have loaded this module
		"FLOWcell" "FLOWchart"
		;; typesetting computer languages
		"EIFFEL" "JAVA" "JAVASCRIPT" "MP" "PASCAL" "PERL" "SQL" "TEX" "XML"
		;; some metapost environments
		"MPpositiongraphic" "useMPgraphic" "MPcode" "reusableMPgraphic"
		"uniqueMPgraphic")
    "List of the ConTeXt en interface start/stop pairs.")

(defvar ConTeXt-setup-list-en
	'("align" "arranging" "background" "backgrounds" "blackrules"
		"blank" "block" "bodyfont" "bodyfontenvironment" "bottom"
		"bottomtexts" "buffer" "capitals" "caption" "captions" "color"
		"colors" "columns" "combinations" "combinedlist" "descriptions"
		"enumerations" "externalfigure" "fillinlines" "fillinrules" "float"
		"floats" "footer" "footertexts" "footnodedefinition" "footnotes"
		"framed" "framedtexts" "head" "header" "headertexts" "headnumber"
		"heads" "headtext" "hyphenmark" "indentations" "indenting" "inmargin"
		"inerlinespace" "itemize" "items" "labeltext" "language" "layout"
		"linenumbering" "lines" "list" "makeup" "marginblocks"
		"marginrules" "marking" "narrower" "oppositeplacing" "pagenumber"
		"pagenumbering" "palet" "papersize" "paragraphs" "quote" "referencing"
		"register" "screens" "section" "sectionblock" "sorting" "spacing"
		"subpagenumber" "synonyms" "text" "textrules" "texttexts" "thinrules"
		"tolerance" "top" "toptexts" "type" "typing" "underbar" "whitespace")
	"List of the names of ConTeXt en interface  macro's that setup things.")

(defun ConTeXt-setup-command-en (what)
	"The ConTeXt en interface way of creating a setup command."
	(concat "setup" what)
	)

(defvar ConTeXt-project-structure-list-en
	'("project" "environment" "product" "component")
	"List of the names of ConTeXt project structure elements for its en interface. List should be in logical order.")

(defvar ConTeXt-section-block-list-en
	'("frontmatter" "bodymatter" "appendices" "backmatter")
	"List of the names of ConTeXt section blocks for its en interface. List should be in logical order.")


;; TODO:
;; ConTeXt has alternative sections like title and subject. Currently
;; the level is used to find the section name, so the alternative
;; names are never found. Have to start using the section name instead
;; of the number.
(defvar ConTeXt-section-list-en
	'(("part" 0)
		("chapter" 1)
		("section" 2)
		("subsection" 3)
		("subsubsection" 4)
;; 		("title" 1)
;; 		("subject" 2)
;; 		("subsubject" 3)
;; 		("subsubsubject" 4)
		)
	"List of the names of ConTeXt sections for its en interface."
)

(defvar ConTeXt-text-en "text"
	"The ConTeXt en interface body text group.")

(defvar ConTeXt-item-list-en
	'("item" "its" "mar" "ran" "sub" "sym")
	"The ConTeXt macro's that are variants of item")

;; Emacs en menu names and labels should go here
;; to be done


;;; Mode

(defun ConTeXt-en-mode-initialization ()
  "ConTeXt english interface specific initialization."
  (mapcar 'ConTeXt-add-environments (reverse ConTeXt-environment-list-en))

	(TeX-add-symbols
	 '("but" ConTeXt-arg-define-ref (TeX-arg-literal " "))
	 '("item" ConTeXt-arg-define-ref (TeX-arg-literal " "))
	 '("items" (ConTeXt-arg-setup t) (TeX-arg-string "Comma separated list"))
	 '("its" ConTeXt-arg-define-ref (TeX-arg-literal " "))
	 '("nop" (TeX-arg-literal " "))
	 '("ran" TeX-arg-string (TeX-arg-literal " "))
	 '("sub" ConTeXt-arg-define-ref (TeX-arg-literal " "))
	 '("sym" (TeX-arg-string "Symbol") (TeX-arg-literal " "))
	 )
)

;;;###autoload
(defun context-en-mode ()
	"Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering context-mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of context-mode-hook."
  (interactive)
  ;; set the ConTeXt interface
	(set (make-local-variable 'ConTeXt-current-interface) "nl")

  ;; initialization
  (ConTeXt-mode-common-initialization)
  (ConTeXt-en-mode-initialization)

  ;; set mode line
	(setq mode-name "ConTeXt-en")
)

(provide 'context-en)

;;; context-en.el ends here