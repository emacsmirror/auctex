;;; tex-info.el - Support for editing TeXinfo source.
;;
;; $Id: tex-info.el,v 5.13 1994-05-28 02:47:44 amanda Exp $

;; Copyright (C) 1993, 1994 Per Abrahamsen 
;; 
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

;;; Code:

(require 'tex)
(condition-case nil			;Lucid is not providing.
    (require 'texinfo)
  (error))
(require 'easymenu)

;;; Environments:

(defvar TeXinfo-environment-list
  '(("cartouche")
    ("defcv")
    ("deffn") ("defivar") ("defmac")
    ("defmethod") ("defop") ("defopt") ("defspec") ("deftp")
    ("deftypefn") ("deftypefun") ("deftypevar") ("deftypevr")
    ("defun") ("defvar") ("defvr") ("description") ("display")
    ("enumerate") ("example") ("ifset") ("ifclear") ("flushleft")
    ("flushright") ("format") ("ftable") ("iftex") ("itemize")
    ("lisp") ("quotation") ("smallexample") ("smalllisp") ("table")
    ("tex") ("titlepage") ("vtable")) 
  "Alist of TeXinfo environments.")

(defconst texinfo-environment-regexp
  ;; Overwrite version from `texinfo.el'.
  (concat "^@\\("
	  (mapconcat 'car TeXinfo-environment-list "\\|")
	  "\\|end\\)")
  "Regexp for environment-like TeXinfo list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

(defun TeXinfo-insert-environment (env)
  "Insert TeXinfo environment ENV.
When called interactively, prompt for an environment."
  (interactive (list (completing-read "Environment: "
				      TeXinfo-environment-list)))
  (insert "@" env "\n\n@end " env "\n")
  (if (null (cdr-safe (assoc "defcv" TeXinfo-environment-list)))
      (forward-line -2)
    ;; apply arguments
    ))

;;; TeXinfo

(defvar TeXinfo-section-list
  '(("top" 0)
    ("majorheading" 0)
    ("chapter" 1)
    ("unnumbered" 1)
    ("appendix" 1)
    ("chapheading" 1)
    ("section" 2)
    ("unnumberedsec" 2)
    ("appendixsec" 2)
    ("heading" 2)
    ("subsection" 3)
    ("unnumberedsubsec" 3)
    ("appendixsubsec" 3)
    ("subheading" 3)
    ("subsubsection" 4)
    ("unnumberedsubsubsec" 4)
    ("appendixsubsubsec" 4)
    ("subsubheading" 4))
  "Alist of sectioning commands and their relative level.")

(defvar TeXinfo-outline-regexp
  (concat "@\\("
	  (mapconcat 'car TeXinfo-section-list "\\|")
	  "\\)")
  "Regular expression matching TeXinfo outline headers.")

(defun TeXinfo-outline-level ()
  ;; Calculate level of current TeXinfo outline heading.
  (save-excursion
    (if (bobp)
	0
      (forward-char 1)
    (let* ((word (buffer-substring (point) (progn (forward-word 1) (point))))
	   (entry (assoc word TeXinfo-section-list)))
      (if entry
	  (nth 1 entry)
	5)))))

;;; Keymap:

(defvar TeXinfo-mode-map nil
  "Keymap for TeXinfo mode.")

(if TeXinfo-mode-map
    ()
  (setq TeXinfo-mode-map (make-sparse-keymap))

  ;; From texinfo.el

  ;; bindings for updating nodes and menus
  (define-key TeXinfo-mode-map "\C-c\C-um"      'texinfo-master-menu)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-m"   'texinfo-make-menu)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-n"   'texinfo-update-node)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-e"   'texinfo-every-node-update)
  (define-key TeXinfo-mode-map "\C-c\C-u\C-a"   'texinfo-all-menus-update)

  ;; From TeX-mode

  ;; Standard
  (define-key TeXinfo-mode-map "\177"     'backward-delete-char-untabify)
  (define-key TeXinfo-mode-map "\C-c}"    'up-list)
  (define-key TeXinfo-mode-map "\C-c#"    'TeX-normal-mode)
  (define-key TeXinfo-mode-map "\C-c\C-n" 'TeX-normal-mode)
  (define-key TeXinfo-mode-map "\C-c?"    'describe-mode)
  
  ;; From tex.el
  (define-key TeXinfo-mode-map "\C-c{"    'TeX-insert-braces)
  (define-key TeXinfo-mode-map "\C-c\C-f" 'TeX-font)
  (define-key TeXinfo-mode-map "\C-c\C-m" 'TeX-insert-macro)
  (define-key TeXinfo-mode-map "\e\t"     'TeX-complete-symbol) 

  (define-key TeXinfo-mode-map "\C-c;"    'TeX-comment-region)
  (define-key TeXinfo-mode-map "\C-c%"    'TeX-comment-paragraph)
  (define-key TeXinfo-mode-map "\C-c'"    'TeX-comment-paragraph) ;*** Old way
  (define-key TeXinfo-mode-map "\C-c:"    'TeX-un-comment-region) ;*** Old way
  (define-key TeXinfo-mode-map "\C-c\""   'TeX-un-comment) ;*** Old way

  ;; From tex-buf.el
  (define-key TeXinfo-mode-map "\C-c\C-r" 'TeX-command-region)
  (define-key TeXinfo-mode-map "\C-c\C-c" 'TeX-command-master)
  (define-key TeXinfo-mode-map "\C-c\C-k" 'TeX-kill-job)
  (define-key TeXinfo-mode-map "\C-c\C-l" 'TeX-recenter-output-buffer)
  (define-key TeXinfo-mode-map "\C-c^" 'TeX-home-buffer)
  (define-key TeXinfo-mode-map "\C-c`"    'TeX-next-error)
  (define-key TeXinfo-mode-map "\C-c\C-w" 'TeX-toggle-debug-boxes)

  ;; From tex.cpl.el

  ;; Simulating LaTeX-mode

  (define-key TeXinfo-mode-map "\C-c\C-e" 'TeXinfo-insert-environment)
  (define-key TeXinfo-mode-map "\C-c\n"   'texinfo-insert-@item)
  (define-key TeXinfo-mode-map "\C-c\C-s" 'texinfo-insert-@node)
  (define-key TeXinfo-mode-map "\C-c]" 'texinfo-insert-@end))

(easy-menu-define TeXinfo-mode-menu
    TeXinfo-mode-map
    "Menu used in TeXinfo mode."
  (list "TeXinfo"
	["Environment..." TeXinfo-insert-environment t]
	["Node..." texinfo-insert-@node t]
	["Macro..." TeX-insert-macro t]
	["Complete" TeX-complete-symbol t]
	["Item" texinfo-insert-@item t]
	(list "Insert Font"
	      ["Emphasize"  (TeX-font nil ?\C-e) "C-c C-f C-e"]
	      ["Bold"       (TeX-font nil ?\C-b) "C-c C-f C-b"]
	      ["Typewriter" (TeX-font nil ?\C-t) "C-c C-f C-t"]
	      ["Small Caps" (TeX-font nil ?\C-c) "C-c C-f C-c"]
	      ["Italic"     (TeX-font nil ?\C-i) "C-c C-f C-i"]
	      ["Sample"    (TeX-font nil ?\C-s) "C-c C-f C-s"]
	      ["Roman"      (TeX-font nil ?\C-r) "C-c C-f C-r"])
	(list "Change Font"
	      ["Emphasize"  (TeX-font t ?\C-e) "C-u C-c C-f C-e"]
	      ["Bold"       (TeX-font t ?\C-b) "C-u C-c C-f C-b"]
	      ["Typewriter" (TeX-font t ?\C-t) "C-u C-c C-f C-t"]
	      ["Small Caps" (TeX-font t ?\C-c) "C-u C-c C-f C-c"]
	      ["Italic"     (TeX-font t ?\C-i) "C-u C-c C-f C-i"]
	      ["Sample"    (TeX-font t ?\C-s) "C-u C-c C-f C-s"]
	      ["Roman"      (TeX-font t ?\C-r) "C-u C-c C-f C-r"])
	"-"
	["Save Document" TeX-save-document t]
	(TeX-command-create-menu "Command on Master File  (C-c C-c)"
				 'TeX-command-master)
	(TeX-command-create-menu "Command on Buffer  (C-c C-b)"
				 'TeX-command-buffer)
	(TeX-command-create-menu "Command on Region  (C-c C-r)"
				 'TeX-command-region)
	["Next Error" TeX-next-error t]
	(list "TeX Output"
	      ["Kill Job" TeX-kill-job t]
	      ["Toggle debug of boxes" TeX-toggle-debug-boxes t]
	      ["Switch to original file" TeX-home-buffer t]
	      ["Recenter Output Buffer" TeX-recenter-output-buffer t])
	"--"
	["Create Master Menu" texinfo-master-menu t]
	["Create Menu" texinfo-make-menu t]
	["Update Node" texinfo-update-node t]
	["Update Every Node" texinfo-every-node-update t]
	["Update All Menus" texinfo-all-menus-update t]
	["Uncomment Region" TeX-un-comment-region t]
	["Comment Region" TeX-comment-region t]
	["Switch to Master file" TeX-home-buffer t]
	["Submit bug report" TeX-submit-bug-report t]
	["Reset Buffer" TeX-normal-mode t]
	["Reset AUC TeX" (TeX-normal-mode t) "C-u C-c C-n"]))

;;; Mode:

;;;###autoload
(defun texinfo-mode ()
  "Major mode for editing files of input for TeXinfo.

Special commands:
\\{TeXinfo-mode-map}

Entering TeXinfo mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value of
TeXinfo-mode-hook."
  (interactive)
  ;; Mostly stolen from texinfo.el
  (setq mode-name "TeXinfo")
  (setq major-mode 'texinfo-mode)
  (use-local-map TeXinfo-mode-map)
  (easy-menu-add TeXinfo-mode-menu TeXinfo-mode-map)
  (set-syntax-table texinfo-mode-syntax-table)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter 
	(concat 
	 "^@node [ \t]*[Tt]op\\|^@\\(" 
	 texinfo-chapter-level-regexp 
	 "\\)"))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "^\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start
	(concat "^\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (make-local-variable 'fill-column)
  (setq fill-column 72)
  (make-local-variable 'comment-start)
  (setq comment-start "@c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "@c +\\|@comment +")
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)
  
  ;; Mostly AUC TeX stuff
  (easy-menu-add TeXinfo-mode-menu TeXinfo-mode-map)
  
  (setq TeX-default-extension "texi")
  (make-local-variable 'TeX-esc)
  (setq TeX-esc "@")

  (make-local-variable 'TeX-auto-regexp-list)
  (setq TeX-auto-regexp-list 'TeX-auto-empty-regexp-list)
  (make-local-variable 'TeX-auto-update)
  (setq TeX-auto-update t)

  (setq TeX-command-default "TeX")
  (setq TeX-header-end "%**end")
  (setq TeX-trailer-start (regexp-quote (concat TeX-esc "bye")))
  
  (make-local-variable 'TeX-font-list)
  (setq TeX-font-list '((?\C-b "@b{" "}")
			(?\C-c "@sc{" "}")
			(?\C-e "@emph{" "}")
			(?\C-i "@i{" "}")
			(?\C-r "@r{" "}")
			(?\C-s "@samp{" "}")
			(?\C-t "@t{" "}")
			(?s    "@strong{" "}")
			(?\C-f "@file{" "}")
			(?\C-d "@dfn{" "}")
			(?\C-v "@var{" "}")
			(?k    "@key{" "}")
			(?\C-k "@kbd{" "}")
			(?c    "@code{" "}")
			(?C    "@cite{" "}")))
  
  (TeX-add-symbols
   '("appendix" "Title")
   '("appendixsec" "Title")
   '("appendixsection" "Title")
   '("appendixsubsec" "Title")
   '("appendixsubsubsec" "Title")
   '("asis")
   '("author" "Author")
   '("b" "Text")
   '("bullet")
   '("bye")
   '("c" "Comment")
   '("center" "Line-of-text")
   '("chapheading" "Title")
   '("chapter" "Title")
   '("cindex" "Entry")
   '("cite" "Reference")
   '("clear" "Flag")
   '("code" "Sample-code")
   '("comment" "Comment")
   '("contents")
   '("copyright")
   '("defcodeindex" "Index-name")
   '("defindex" "Index-name")
   '("dfn" "Term")
   '("dmn" "Dimension")
   '("dots")
   '("emph" "Text")
   '("equiv")
   '("error")
   '("evenfooting" TeXinfo-lrc-argument-hook)
   '("evenheading" TeXinfo-lrc-argument-hook)
   '("everyfooting" TeXinfo-lrc-argument-hook)
   '("everyheading" TeXinfo-lrc-argument-hook)
   '("exdent" "Line-of-text")
   '("expansion")
   '("file" "Filename")
   '("finalout")
   '("findex" "Entry")
   '("footnote" "Text-of-footnote")
   '("footnotestyle" "Style")
   '("group")
   '("heading" "Title")
   '("headings" "On-off-single-double")
   '("i" "Text")
   '("ignore")
   '("include" "Filename")
   '("inforef" "Node-name" "Info-file-name")
   '("item")
   '("itemx")
   '("kbd" "Keyboard-characters")
   '("key" "Key-name")
   '("kindex" "Entry")
   '("majorheading"  "Title")
   '("menu")
   '("minus")
   '("need" "N")
   '("node" "Name" "Next" "Previous" "Up")
   '("noindent")
   '("oddfooting" TeXinfo-lrc-argument-hook)
   '("oddheading" TeXinfo-lrc-argument-hook)
   '("page")
   '("paragraphindent" "Indent")
   '("pindex" "Entry")
   '("point")
   '("print")
   '("printindex" "Index-name")
   '("pxref" "Node-name")
   '("r" "Text")
   '("ref" "Node-name")
   '("refill")
   '("result")
   '("samp" "Text")
   '("sc" "Text")
   '("section" "Title")
   '("set" "Flag")
   '("setchapternewpage" "On-off-odd")
   '("setfilename" "Info-file-name")
   '("settitle" "Title")
   '("shortcontents")
   '("smallbook")
   '("sp" "N")
   '("strong" "Text")
   '("subheading" "Title")
   '("subsection" "Title")
   '("subsubheading" "Title")
   '("subsubsection" "Title")
   '("subtitle" "Title")
   '("summarycontents")
   '("syncodeindex" "From-index" "Into-index")
   '("synindex" "From-index" "Into-index")
   '("t" "Text")
   '("TeX")
   '("thischapter")
   '("thischaptername")
   '("thisfile")
   '("thispage")
   '("tindex" "Entry")
   '("title" "Title")
   '("titlefont" "Text")
   '("titlepage")
   '("today")
   '("top" "Title")
   '("unnumbered" "Title")
   '("unnumberedsec" "Title")
   '("unnumberedsubsec" "Title")
   '("unnumberedsubsubsec" "Title")
   '("value" "Flag")
   '("var" "Metasyntactic-variable")
   '("vindex" "Entry")
   '("vskip" "Amount")
   '("w" "Text"))
  
  (require 'outline)		;Must be loaded first.
  (make-local-variable 'outline-regexp)
  (setq outline-regexp TeXinfo-outline-regexp)
  (make-local-variable 'outline-level)
  (setq outline-level 'TeXinfo-outline-level)
  
  (run-hooks 'text-mode-hook 'TeXinfo-mode-hook))
  
(provide 'tex-info)
  
;;; tex-info.el ends here
