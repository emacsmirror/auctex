(setq debug-on-error t)
;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;
;;
;; auc-tex.el - A much enhanced LaTeX mode
;; 
;; Copyright (C) 1991 Kresten Krab Thorup (krab@iesd.auc.dk).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; LCD Archive Entry:
;; AUC TeX|Kresten Krab Thorup|krab@iesd.auc.dk
;; | A much enhanced LaTeX mode 
;; |$Date: 1992-01-27 15:48:21 $|$Revision: 5.17 $|iesd.auc.dk:/pub/emacs-lisp/auc-tex.tar.Z
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RCS status      : $Revision: 5.17 $  
;; Author          : Kresten Krab Thorup
;; Created On      : Fri May 24 09:36:21 1991
;; Last Modified By: Kresten Krab Thorup
;; Last Modified On: Mon Jan 27 15:18:45 1992
;; Update Count    : 484
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUC TeX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing. 
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; this file so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.
;;
;; This software was written as part of the author's official duty as
;; an employee of the University of Aalborg (denmark) and is in the
;; public domain.  You are free to use this software as you wish, but
;; WITHOUT ANY WARRANTY WHATSOEVER.  It would be nice, though if when
;; you use this code, you give due credit to the author.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;
;; HISTORY
;; 9-Dec-1991  (Last Mod: Mon Dec  9 10:04:18 1991 #461)  Kresten Krab Thorup
;;    Fixed a bug in LaTeX-style-options, which caused completion to
;;    crash when no optional arguments were available in
;;    \documentstyle, as supposed by smith@pell.anu.edu.au (Michael Smith)
;; 13-Jun-1991  (Last Mod: Thu Jun 13 20:27:50 1991 #334)  Kresten Krab Thorup
;;    Fixed some bugs in Mattison's patch. Thanks to Robert Estes
;;    estes@ebony.eecs.ucdavis.edu  for supplying some..
;; 10-Jun-1991  (Last Mod: Mon Jun 10 02:15:45 1991 #332)  Kresten Krab Thorup
;;    Added batches from Sven Mattison.  This includes also some
;;    handling of 7-bit modes... All "\\" are translated to TeX-esc etc.
;; 1-Jun-1991  (Last Mod: Sat Jun  1 20:32:48 1991 #129)  Kresten Krab Thorup
;;    fill-paragraph is made to indent too due to help from Per Hagen
;;    <per@iesd.auc.dk>
;; 31-May-1991  (Last Mod: Fri May 31 09:10:01 1991 #118)  Kresten Krab Thorup
;;    The distribution has been split into 8 individual modules. This will
;;    speed up the entire system.  Also, there has been added completion for
;;    LaTeX commands (placed on M-TAB), and a new minor mode "math" has been
;;    introduced. Math mode may be loaded by M-x TeX-math-mode.
;;    A lot of minor bugs has been fixed due suggestions from a lot of people.
;; 30-May-1991  (Last Mod: Thu May 30 21:00:46 1991 #54)  Kresten Krab Thorup
;;    Fixed bug in TeX-preview, au suggested by Martin Simons 
;;    <simons@ibiza.karlsruhe.gmd.de>
;; 30-May-1991  (Last Mod: Thu May 30 13:40:08 1991 #41)  Kresten Krab Thorup
;;    Added TeX-complete-symbol 
;; 30-May-1991  (Last Mod: Thu May 30 00:42:55 1991 #37)  Kresten Krab Thorup
;;    Added TeX-default-jobname-prefix
;; 29-May-1991  (Last Mod: Wed May 29 21:12:16 1991 #35)  Kresten Krab Thorup
;;    Stopped calling the shell, as this didn't work with bash. Instead
;;    the external programs latex/tex/previewer/makeindex/bibtex/etc.
;;    are called directly.
;; 29-May-1991  (Last Mod: Wed May 29 13:10:32 1991 #30)  Kresten Krab Thorup
;;    Changed paragraph-start/seperate as suggested by Per Abrahamsen.
;;    Also  fixed  bug in comment-out-region.
;; 27-May-1991  (Last Mod: Mon May 27 06:43:10 1991 #13)  Kresten Krab Thorup
;;    Added comment macros, and took out the danish support
;; 27-May-1991  (Last Mod: Mon May 27 01:17:14 1991 #9)  Kresten Krab Thorup
;;    validate-TeX-buffer changed to TeX-validate-buffer
;; 
;; AUC TeX was derived from tex-mode.el of the original 
;; Emacs distribution. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; THE GOOD GUYS
;; 
;; Lars Fischer                      <fischer@iesd.auc.dk> 
;; Per Abrahamsen                    <abraham@iesd.auc.dk> 
;; Martin Simons                     <simons@ibiza.karlsruhe.gmd.de>
;; Per Hagen                         <per@iesd.auc.dk>
;; Sven Mattisson                    <sven@tde.lth.se>
;; Michael Smith                     <smith@pell.anu.edu.au>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BUGREPORTS 
;;
;;  please send to:
;;  Internet : auc-tex_mgr@iesd.auc.dk
;;
;;  Comments and ideas to auc-tex@iesd.auc.dk
;;
;;  A mailing list `auc-tex' discusses topics concerning
;;  auctex.  You may subscribe by mailing the above adress.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Thanks a lot to Leslie Lamport for supplying the source 
;;  for the LaTeX error messages in the tex-dbg.el module
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TO DO LIST  (to add items, mail auc-tex_mgr@iesd.auc.dk)
;;
;;   Incorporate `TeX-info-mode'... This is just a mad idea, but 
;;   why not ?  --- I've started working on it!
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Futher variables for customization are found in the individual
;; modules:
;; 
;; ltx-sec       : Smart sectioning
;; ltx-env   : Smart handling of environments 
;; tex-buf          : Invoking TeX/LaTeX from an inferior shell
;;                       includes also print/preview/makeindex etc.
;; tex-dbg           : Debugging documents, when they have been run
;; tex-misc            : Miscellaneous functions
;; tex-math            : Smart bindings for math symbols
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar TeX-esc "\\" "The TeX escape character.")
(make-variable-buffer-local 'TeX-esc)

(defvar TeX-grop "{" "The TeX group opening character.")
(make-variable-buffer-local 'TeX-grop)

(defvar TeX-grcl "}" "The TeX group closing character.")
(make-variable-buffer-local 'TeX-grcl)

(defvar LaTeX-optop "[" "The LaTeX optional argument opening character.")
(make-variable-buffer-local 'LaTeX-optop)

(defvar LaTeX-optcl "]" "The LaTeX optional argument closeing character.")
(make-variable-buffer-local 'LaTeX-optcl)

(defvar TeX-mode-syntax-table nil
  "Syntax table used while in TeX mode.")
(make-variable-buffer-local 'TeX-mode-syntax-table)

(defvar TeX-mode-map nil
  "Keymap used in TeX-mode.")

(defvar LaTeX-mode-map nil
  "Keymap used in LaTeX-mode.")

(defvar TeX-compilation-map nil
  "Keymap for the TeX compile buffer.")

(defvar TeX-mode-syntax-table nil
  "Syntax table used while in TeX mode.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoload modules
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first load the site-default settings for auc-tex

(require 'tex-site)

(let ((no-doc
       "Documentation will be available when the function has been called
as the definition of this this function is placed in an external module."))

  ;; minor modes
  (autoload 'LaTeX-math-mode "tex-math" no-doc t)
  (autoload 'outline-minor-mode "outline-m" no-doc t)

  ;; sectioning commands
  (autoload 'LaTeX-section "ltx-sec" no-doc t)

  ;; environment commands
  (autoload 'LaTeX-environment "ltx-env" no-doc t)
  (autoload 'LaTeX-insert-item "ltx-env" no-doc t)
  (autoload 'LaTeX-close-environment "ltx-env" no-doc t)

  ;; invoking tex
  (autoload 'TeX-home-buffer "tex-buf" no-doc t)
  (autoload 'TeX-region "tex-buf" no-doc t)
  (autoload 'TeX-buffer "tex-buf" no-doc t)
  (autoload 'TeX-kill-job "tex-buf" no-doc t)
  (autoload 'TeX-recenter-output-buffer "tex-buf" no-doc t)
  (autoload 'TeX-preview "tex-buf" no-doc t)
  (autoload 'TeX-print "tex-buf" no-doc t)
  (autoload 'LaTeX-bibtex "tex-buf" no-doc t)
  (autoload 'LaTeX-makeindex "tex-buf" no-doc t)
  
  ;; `debugging' commands
  (autoload 'TeX-next-error "tex-dbg" no-doc t)
  (autoload 'TeX-toggle-debug-boxes "tex-dbg" no-doc t)
  (autoload 'TeX-help-error "tex-dbg" no-doc t)
  
  ;; miscellaneous
  (autoload 'TeX-comment-out-region "tex-misc" no-doc t)
  (autoload 'TeX-comment-out-paragraph "tex-misc" no-doc t)
  (autoload 'TeX-un-comment "tex-misc" no-doc t)
  (autoload 'TeX-un-comment-region "tex-misc" no-doc t)
  (autoload 'TeX-validate-buffer "tex-misc" no-doc t)
  (autoload 'TeX-terminate-paragraph "tex-misc" no-doc t)
  (autoload 'TeX-complete-symbol "tex-cpl" no-doc t)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymaps
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun TeX-define-common-keys (keymap)
  "Define the keys that we want defined both in TeX-mode
and in the TeX-compilation."
  (define-key keymap "\C-c\C-k"    'TeX-kill-job)
  (define-key keymap "\C-c\C-l"    'TeX-recenter-output-buffer)
  )

(if TeX-mode-map 
    ()
  (setq TeX-mode-map (make-sparse-keymap))
  (TeX-define-common-keys TeX-mode-map)
  (define-key TeX-mode-map "\177"     'backward-delete-char-untabify)
  (define-key TeX-mode-map "\n"       'TeX-terminate-paragraph)
  (define-key TeX-mode-map "\""       'TeX-insert-quote)
  (define-key TeX-mode-map "\e}"      'up-list)
  (define-key TeX-mode-map "\e{"      'TeX-insert-braces)
  (define-key TeX-mode-map "\C-c;"    'TeX-comment-out-region)
  (define-key TeX-mode-map "\C-c'"    'TeX-comment-out-paragraph)
  (define-key TeX-mode-map "\C-c:"    'TeX-un-comment-region)
  (define-key TeX-mode-map "\C-c\""   'TeX-un-comment)
  (define-key TeX-mode-map "\C-c\C-o" 'Tex-cmd-on-region)
  (define-key TeX-mode-map "\C-c\C-b" 'TeX-bold)
  (define-key TeX-mode-map "\C-c\C-i" 'TeX-italic)
  (define-key TeX-mode-map "\C-c\C-s" 'TeX-slanted)
  (define-key TeX-mode-map "\C-c\C-r" 'TeX-roman)
  (define-key TeX-mode-map "\C-c\C-e" 'TeX-emphasize)
  (define-key TeX-mode-map "\C-c\C-t" 'TeX-typewriter)
  (define-key TeX-mode-map "\C-c\C-y" 'TeX-small-caps)

  (define-key TeX-mode-map "\C-c\C-m" 'TeX-insert-macro)
  (define-key TeX-mode-map "\C-c\C-d" 'TeX-region)
  (define-key TeX-mode-map "\C-c\C-a" 'TeX-buffer)
  (define-key TeX-mode-map "\C-c\C-p" 'TeX-preview)
  (define-key TeX-mode-map "\C-c\C-n" 'TeX-next-error)

  (define-key TeX-mode-map "\C-c\C-h" 'TeX-home-buffer)
  (define-key TeX-mode-map "\C-c\C-w" 'TeX-toggle-debug-boxes)
  (define-key TeX-mode-map "\C-c!"    'TeX-print)
  (define-key TeX-mode-map "\e\t"     'TeX-complete-symbol))

(if LaTeX-mode-map
    ()
  (setq LaTeX-mode-map (copy-keymap TeX-mode-map))
  (define-key LaTeX-mode-map "\n"       'reindent-then-newline-and-indent)
  (define-key LaTeX-mode-map "\t"       'LaTeX-indent-line)
  (define-key LaTeX-mode-map "\M-\r"     'LaTeX-insert-item)
  (define-key LaTeX-mode-map "\C-c\n"   'TeX-terminate-paragraph)
  (define-key LaTeX-mode-map "\C-c\C-x" 'LaTeX-section)
  (define-key LaTeX-mode-map "\C-c\C-c" 'LaTeX-environment)
  (define-key LaTeX-mode-map "\C-c@"    'LaTeX-bibtex)
  (define-key LaTeX-mode-map "\C-c#"    'LaTeX-makeindex)
  (define-key LaTeX-mode-map "\M-q"     'LaTeX-format-paragraph)
  (define-key LaTeX-mode-map "\M-g"     'LaTeX-format-region)
  (define-key LaTeX-mode-map "\M-s"     'LaTeX-format-section)
  (define-key LaTeX-mode-map "\M-\C-e"  'LaTeX-mark-environment)
  (define-key LaTeX-mode-map "\C-c\C-f"  'LaTeX-close-environment)
  (define-key LaTeX-mode-map "\M-\C-x"  'LaTeX-mark-section) 
  (define-key LaTeX-mode-map "\M-\C-q"  'LaTeX-format-environment))

(if (boundp 'texinfo-mode-map)
    ()
  (setq texinfo-mode-map (copy-keymap TeX-mode-map))
  (define-key texinfo-mode-map "\M-\C-d" 'texinfo-format-region)
  (define-key texinfo-mode-map "\M-\C-a" 'texinfo-format-buffer)
  (define-key texinfo-mode-map "\C-c\C-v"    'texinfo-insert-@var)
  (define-key texinfo-mode-map "\C-c\C-s"    'texinfo-insert-@samp)
  (define-key texinfo-mode-map "\C-c\C-e"    'texinfo-insert-@emph)
  (define-key texinfo-mode-map "\C-c\C-x"    'texinfo-insert-@node)
  (define-key texinfo-mode-map "\C-c\C-="    'texinfo-insert-@dfn)
  (define-key texinfo-mode-map "\C-c\C-t"    'texinfo-insert-@code)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX / LaTeX modes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-mode ()
  "Major mode for editing files of input for TeX or LaTeX.
Tries to intuit whether this file is for plain TeX or LaTeX and
calls plain-tex-mode or latex-mode.  If it cannot be determined
\(e.g., the file is empty), the value of TeX-default-mode is used."
  (interactive)
  (let
      ((mode
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward 
	       (concat "^[^%]*"
		       (regexp-quote TeX-esc)
		       "\\(begin[^a-z]\\|section\s*{\\|part\\|chapter\\)") nil t)
	      'latex-mode
	    (if nil
		'texinfo-mode
	      TeX-default-mode)))))
    (if mode (funcall mode)
      (funcall TeX-default-mode))))

(defun plain-tex-mode ()
  "Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[TeX-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  \\[TeX-buffer] does the whole buffer.
Use \\[TeX-preview] to preview the .dvi file made by either of these.

Use \\[TeX-next-error] to trace through the errors output from TeX.
Use \\[TeX-help-error] to display a help message for the most recent
error.

Use \\[TeX-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{TeX-mode-map}
 
Mode variables:
TeX-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[TeX-region] or \\[TeX-buffer].

Entering plain-TeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of plain-TeX-mode-hook."
  (interactive)
  (TeX-common-initialization)
  (use-local-map TeX-mode-map)
  (setq mode-name "AUC-TeX")
  (setq major-mode 'plain-TeX-mode)
  (make-local-variable 'TeX-command)
  (setq TeX-command "tex")
  (setq TeX-bibtex-command "bibtex")
  (setq TeX-index-command "makeindex")
  (setq paragraph-start
	(concat
	 "\\(^[ \t]*$"
	 "\\|" (regexp-quote TeX-esc) "par\\|" 
	 "^[ \t]*"
	 (regexp-quote TeX-esc)
	 "\\("
	 "begin\\|end\\|part\\|chapter\\|"
	 "section\\|subsection\\|subsubsection\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix\\|label\\|caption\\|"
	 "\[\\|\]" ; display math delimitors
	 "\\)"
	 "\\|"
	 "^[ \t]*\\$\\$" ; display math delimitor
	 "\\)" ))
  (setq paragraph-separate
	(concat
	 "\\("
	 (regexp-quote TeX-esc)
	 "par\\|"
	 "^[ \t]*$\\|"
	 "^[ \t]*"
	 (regexp-quote TeX-esc)
	 "\\("
	 "begin\\|end\\|label\\|caption\\|part\\|chapter\\|"
	 "section\\|subsection\\|subsubsection\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix"
	 "\\)"
	 "\\)"))
  (setq comment-start-skip
	(concat
	 "\\(\\(^\\|[^\\]\\)\\("
	 (regexp-quote TeX-esc)
	 (regexp-quote TeX-esc)
	 "\\)*\\)\\(%+ *\\)"))
  (setq TeX-start-of-header "%**start of header")
  (setq TeX-end-of-header "%**end of header")
  (setq TeX-trailer (concat TeX-esc "bye"))
  (setq TeX-h1 (concat
		TeX-esc "nonstopmode" TeX-grop TeX-grcl))
  (setq TeX-h2 (concat
		TeX-esc "input"
		TeX-grop TeX-auto-header TeX-grcl
		TeX-grop TeX-grcl))
  (setq TeX-t1 (concat TeX-grop TeX-grcl))
  (setq TeX-t2 (concat
		TeX-esc "input"
		TeX-grop TeX-auto-trailer TeX-grcl
		TeX-grop TeX-grcl))
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'plain-TeX-mode-hook))

(defun latex-mode ()
  "Major mode for editing files of input for LaTeX.

Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.
LFD and TAB indent lines as with programming modes.

Use \\[TeX-region] to run LaTeX on the current region, plus the
preamble copied from the top of the file (containing \\documentstyle,
etc.).  \\[TeX-buffer] does the whole buffer.  Use \\[TeX-preview] to
preview the .dvi file made by either of these.

Use \\[TeX-next-error] to trace through the errors. 

See LaTeX-section and LaTeX-environment for a description of customization.

Use \\[TeX-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

TAB is forced to insert spaces, as TeX ignores ordinary tab's.

Special commands:
\\{LaTeX-mode-map}

Mode variables:

	Refer to `tex-site.el' for customization

Entering LaTeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of LaTeX-mode-hook."
  (interactive)
  (TeX-common-initialization)
  (setq LaTeX-optop "[")
  (setq LaTeX-optcl "]")
  (modify-syntax-entry (string-to-char LaTeX-optop) (concat "(" LaTeX-optcl))  
  (modify-syntax-entry (string-to-char LaTeX-optcl) (concat ")" LaTeX-optop))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'LaTeX-indent-line)
  (use-local-map LaTeX-mode-map)
  (setq mode-name "AUC-LaTeX")
  (setq major-mode 'LaTeX-mode)  
  (setq outline-level-function 'LaTeX-outline-level)

  (make-variable-buffer-local 'outline-regexp)
  (setq outline-regexp LaTeX-outline-regexp)

  (make-local-variable 'TeX-command)
  (setq TeX-command "latex")

  (make-local-variable 'TeX-format-package)
  (setq TeX-format-package LaTeX-format-package)

  (make-local-variable 'LaTeX-style)
  (setq LaTeX-style '(""))
  (LaTeX-document-style)

  (make-local-variable 'LaTeX-options)
  (setq LaTeX-options '(""))
  (LaTeX-style-options)

  (setq TeX-bibtex-command "bibtex")
  (setq TeX-index-command "makeindex")
  (setq LaTeX-paragraph-start-command
	(concat
	 (regexp-quote TeX-esc)
	 "\\("
	 "begin\\|end\\|item\\|part\\|chapter\\|label\\|caption\\|"
	 "section\\|subsection\\|subsubsection\\|par\\|noindent\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix\\|"
	 "\[\\|]"  ; display math delimitors
	 "\\)" ))
  (setq paragraph-start
	(concat
	 "\\("
	 "^.*[^" (regexp-quote TeX-esc) "]%.*$\\|"
	 "^%.*$\\|"
	 "^[ \t]*$"
	 "\\|"
	 "^[ \t]*"
	 LaTeX-paragraph-start-command
	 "\\|"
	 "^[ \t]*\\$\\$" ; display math delimitor
	 "\\)" ))
  (setq paragraph-separate
	(concat
	 "\\("
	 "^.*[^" (regexp-quote TeX-esc) "]%.*$\\|"
	 "^%.*$\\|"
	 (regexp-quote TeX-esc)
	 "par\\|"
	 "^[ \t]*$\\|"
	 "^[ \t]*"
	 (regexp-quote TeX-esc)
	 "\\("
	 "begin\\|end\\|part\\|chapter\\|label\\|caption\\|"
	 "section\\|subsection\\|subsubsection\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix"
	 "\\)"
	 "\\)"))
  (setq comment-start-skip
	(concat
	 "\\(\\(^\\|[^\\]\\)\\("
	 (regexp-quote TeX-esc)
	 (regexp-quote TeX-esc)
	 "\\)*\\)\\(%+ *\\)"))
  (setq TeX-start-of-header (concat TeX-esc "documentstyle"))
  (setq TeX-end-of-header (concat TeX-esc "begin"
				  TeX-grop "document" TeX-grcl))
  (setq TeX-trailer (concat TeX-esc "end"
				  TeX-grop "document" TeX-grcl))
  (setq TeX-h1 (concat
		TeX-esc "nonstopmode" TeX-grop TeX-grcl))
  (setq TeX-h2 (concat
		TeX-esc "input"
		TeX-grop TeX-auto-header TeX-grcl
		TeX-grop TeX-grcl))
  (setq TeX-t1 (concat TeX-grop TeX-grcl))
  (setq TeX-t2 (concat
		TeX-esc "input"
		TeX-grop TeX-auto-trailer TeX-grcl
		TeX-grop TeX-grcl))
  (setq indent-tabs-mode nil)
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'LaTeX-mode-hook))

(defun TeX-common-initialization ()
  "Initialization common to TeX and LaTeX modes."
  (kill-all-local-variables)
  (use-local-map TeX-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq ispell-filter-hook "idetex")
  (setq ispell-filter-hook-args '())
  (if (null TeX-mode-syntax-table)
      (progn
	(setq TeX-mode-syntax-table (make-syntax-table))
	(set-syntax-table TeX-mode-syntax-table)
	(modify-syntax-entry (string-to-char TeX-esc) "\\")
	(modify-syntax-entry ?\f ">")
	(modify-syntax-entry ?\n ">")
	(modify-syntax-entry ?$ "$$")
	(modify-syntax-entry (string-to-char TeX-grop) (concat "(" TeX-grcl))  
	(modify-syntax-entry (string-to-char TeX-grcl) (concat ")" TeX-grop))  
	(modify-syntax-entry ?% "<")
	(modify-syntax-entry ?" ".")
	(modify-syntax-entry ?& ".")
	(modify-syntax-entry ?_ ".")
	(modify-syntax-entry ?@ "_")
	(modify-syntax-entry ?~ " ")
	(modify-syntax-entry ?' "w"))
    (set-syntax-table TeX-mode-syntax-table))
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'TeX-comment-indent)
  (make-local-variable 'TeX-bibtex-command)
  (make-local-variable 'TeX-index-command)
   (make-local-variable 'TeX-command)
  (make-local-variable 'TeX-start-of-header)
  (make-local-variable 'TeX-end-of-header)
  (make-local-variable 'TeX-trailer)
  (make-local-variable 'TeX-h1)
  (make-local-variable 'TeX-h2)
  (make-local-variable 'TeX-t1)
  (make-local-variable 'TeX-t2)
  (setq TeX-esc "\\")
  (setq TeX-grop "{")
  (setq TeX-grcl "}")
  (modify-syntax-entry (string-to-char TeX-esc) "/")
  (modify-syntax-entry (string-to-char TeX-grop) (concat "(" TeX-grcl))  
  (modify-syntax-entry (string-to-char TeX-grcl) (concat ")" TeX-grop)))

(defun insert-mode-line ()
    "This little macro inserts `% -*- mode-name -*-' if not present.
You should insert this in your TeX-mode-hook!"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward "-\\*-.*-\\*-" 100 t))
	  (insert-string (concat "% -*- " mode-name " -*-\n")))))



(defvar dummy "OUTLINE-MODE")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outline-mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar LaTeX-outline-regexp
  (concat "[ \t]*" (regexp-quote TeX-esc)
	  "\\(appendix\\|documentstyle\\|part\\|chapter\\|section\\|"
	  "subsection\\|subsubsection\\|paragraph\\|subparagraph\\)")
  "Regular expression used for outlining.")

(defun LaTeX-outline-level ()
  "Find the level of current outline heading in an LaTeX document."
  (save-excursion
    (skip-chars-forward " \t")
    (forward-char 1)
    (cond ((looking-at "subparagraph") 9)
	  ((looking-at "paragraph") 8)
	  ((looking-at "subsubsection") 7)
	  ((looking-at "subsection") 6)
	  ((looking-at "section") 5)
	  ((looking-at "chapter") 4)
	  ((looking-at "part") 3)
	  ((looking-at "appendix") 2)
	  ((looking-at "documentstyle") 2))))

(defvar dummy "INDENTATION")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun LaTeX-indent-line ()
  "Indent the line containing point, as LaTeX source.
Add LaTeX-indent-level indentation in each \\begin{ - \\end{ block.
Lines starting with \\item is given an extra indentation of
LaTeX-item-indent."
  (interactive)
  (save-excursion
  (let ((indent (calculate-LaTeX-indentation)))
    (if (/= (current-indentation) indent)
	(let ((beg (progn
		     (beginning-of-line)
		     (point))))
	  (back-to-indentation)
	  (delete-region beg (point))
	  (indent-to indent)))))
  (if (< (current-column) (calculate-LaTeX-indentation))
      (back-to-indentation)))

(defun LaTeX-format-paragraph (prefix)
"Fill and indent paragraph at or after point.
Prefix arg means justify as well."
  (interactive "P")
  (let ((where (point))
	(word (progn
		(re-search-forward "[^ \t]")
		(buffer-substring (match-beginning 0)
				  (match-end 0)))))
    (goto-char where)
    (save-excursion
      (LaTeX-indent-line)
      (let ((indent-val (calculate-LaTeX-indentation)))
	(mark-paragraph)
	(setq fill-column (- fill-column indent-val))
	(fill-paragraph prefix)
	(indent-region (region-beginning) (region-end) nil)
	(setq fill-column (+ fill-column indent-val))))
    (search-forward word)
    (backward-char)))
  
(defun LaTeX-format-region (from to &optional justify what)
 "Fill and indent each of the paragraphs in the region as LaTeX text.
Prefix arg (non-nil third arg, if called from program)
means justify as well. Fourth arg WHAT is a word to be displayed when
formatting."
  (interactive "r\nP")
  (save-restriction
    (save-excursion
      (let ((length (- to from))) 
	(goto-char from)
	(beginning-of-line)
	(narrow-to-region (point-min) to)
	(while (not (eobp))
	  (message "Formatting%s ... %d%%"
		   (if (not what)
		       ""
		     what)
		   (/ (* 100 (- (point) from)) length))
	  (LaTeX-format-paragraph justify)
	  (re-search-forward (concat "\\(" LaTeX-paragraph-start-command "\\|^ +$\\|\\'\\)" ) (point-max) t)))))
  (message "Finished"))

(defun LaTeX-mark-environment ()
  "Set mark to end of current environment and point to the matching begin
will not work properly if there are unbalanced begin-end pairs in
comments and verbatim environments"
  (interactive)
  (LaTeX-find-matching-end)
  (set-mark (point))
  (search-backward TeX-esc)
  (LaTeX-find-matching-begin))

(defun LaTeX-find-matching-end ()
  "Move mark to the \\end of the current environment"
  (interactive)
  (while
      (progn
        (if (re-search-forward (concat (regexp-quote TeX-esc)
				       "\\(begin\\|end\\)") nil  t)
            (if (string-match "\\begin" (buffer-substring
					 (match-beginning 1)
					 (match-end 1)))
                (progn (LaTeX-find-matching-end) t)
              nil)
          (error "Can't locate current environment - end"))))
  (re-search-forward "}[ \t]*\n?"))


(defun LaTeX-find-matching-begin ()
  ""
  (interactive)
  (while 
      (progn
	(if (re-search-backward (concat " *"
					(regexp-quote TeX-esc)
					"\\(begin\\|end\\)") nil t)
	    (if (equal "\\end" (buffer-substring
				(match-beginning 1)
				(match-end 1)))
		(progn (LaTeX-find-matching-begin) t)
	      nil)
	(error "Can't locate current environment - begin")))))


(defun LaTeX-format-environment (justify)
  "Fill and indent current environment as LaTeX text."
  (interactive "P")
  (save-excursion
    (LaTeX-mark-environment)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-format-region
     (region-beginning)
     (region-end)
     justify
     (concat " environment " (buffer-substring (match-beginning 1)
					     (match-end 1))))))


(defun LaTeX-format-section (justify)
  "Fill and indent current logical section as LaTeX text."
  (interactive "P")
  (save-excursion
    (LaTeX-mark-section)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-format-region
     (region-beginning)
     (region-end)
     justify
     (concat " section " (buffer-substring (match-beginning 1)
					   (match-end 1))))))

(defun LaTeX-mark-section ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "\\(^" LaTeX-outline-regexp
			      "\\|\\'\\)"))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "\\(^" LaTeX-outline-regexp
			      "\\|\\`\\)")))


(defun LaTeX-format-buffer (justify)
  "Fill and indent current buffer as LaTeX text."
  (interactive "P")
  (save-excursion
    (LaTeX-format-region
     (point-min)
     (point-max)
     justify
     (concat " buffer " (buffer-name)))))

(defun calculate-LaTeX-indentation ()
  "Return the correct indentation of line of LaTeX source. (I hope...)"
  (save-excursion
    (back-to-indentation)
    (cond ((looking-at (concat (regexp-quote TeX-esc) "end{verbatim}"))
	   (save-excursion
	     (search-backward "\\begin{verbatim}")
	     (current-indentation)))
	  ((looking-at (concat "\\("
			       (regexp-quote TeX-esc)
			       "end{\\|"
			       (regexp-quote TeX-esc)
			       "right\\)"))
	   (- (calculate-normal-LaTeX-indentation) LaTeX-indent-level))
	  ((looking-at (concat (regexp-quote TeX-esc) "item\\W"))
	   (+ (calculate-normal-LaTeX-indentation) LaTeX-item-indent))
	  (t (calculate-normal-LaTeX-indentation)))))

(defun calculate-normal-LaTeX-indentation ()
  "Return the correct indentation of a normal line of text."
  (skip-chars-backward "\n\t ")
  (move-to-column (current-indentation))
  (cond ((looking-at (concat (regexp-quote TeX-esc) "begin{document}"))
	 ;; I dislike having all of the document indented...
	 (current-indentation))
	((looking-at (concat (regexp-quote TeX-esc) "begin"
			     (regexp-quote TeX-grop)
			     "verbatim"))
	 0)
	((looking-at (concat (regexp-quote TeX-esc) "begin"
			     (regexp-quote TeX-grop)))
	 (+ (current-indentation) LaTeX-indent-level))
	((looking-at (concat (regexp-quote TeX-esc) "item\\W"))
	 (- (current-indentation) LaTeX-item-indent))
	(t (current-indentation))))

(defun TeX-comment-indent ()
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
	 comment-column)))

(defvar dummy "GROUPS")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun Tex-cmd-on-region (begin end command)
  "Reads a (La)TeX-command. Makes current region a TeX-group.
Inserts command at the start of the group."
  (interactive "*r\ns(La)TeX-command on region: ")
  (save-excursion
    (goto-char end)   (insert TeX-grcl)
    (goto-char begin) (insert TeX-grop TeX-esc command " ")))

(defun TeX-insert-macro ()
  (interactive)
  (insert TeX-esc TeX-grop TeX-grcl)
  (backward-char 2))

(defun TeX-insert-braces ()
  "Make a pair of braces and be poised to type inside of them."
  (interactive)
  (insert TeX-grop)
  (save-excursion
    (insert TeX-grcl)))

(defun TeX-bold ()
  (interactive)
  (insert TeX-grop TeX-esc "bf " TeX-grcl)
  (backward-char 1))

(defun TeX-italic ()
  (interactive)
  (insert TeX-grop TeX-esc "it " TeX-esc "/" TeX-grcl)
  (backward-char 3))

(defun TeX-slanted ()
  (interactive)
  (insert TeX-grop TeX-esc "sl " TeX-esc "/" TeX-grcl)
  (backward-char 3))

(defun TeX-roman ()
  (interactive)
  (insert TeX-grop TeX-esc "rm " TeX-grcl)
  (backward-char 1))

(defun TeX-emphasize ()
  (interactive)
  (insert TeX-grop TeX-esc "em " TeX-esc "/" TeX-grcl)
  (backward-char 3))

(defun TeX-typewriter ()
  (interactive)
  (insert TeX-grop TeX-esc "tt " TeX-grcl)
  (backward-char 1))

(defun TeX-small-caps ()
  (interactive)
  (insert TeX-grop TeX-esc "sc " TeX-grcl)
  (backward-char 1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous often used functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun TeX-insert-quote (arg)
  "Insert ``, '' or \" according to preceding character.
With prefix argument, always insert \" characters."
  (interactive "P")
  (if arg
      (let ((count (prefix-numeric-value arg)))
	(if (listp arg)
	    (self-insert-command 1)	;C-u always inserts just one
	  (self-insert-command count)))
    (insert
     (cond
      ((or (bobp)
	   (save-excursion
	     (forward-char -1)
	     (looking-at "[ \t\n]\\|\\s(")))
       "``")
      ((= (preceding-char) (string-to-char TeX-esc))
       ?\")
      (t "''")))))



(defun LaTeX-document-style ()
  "Return the name of the used documentstyle in this LaTeX file."
  (interactive)
  (if (not (equal LaTeX-style '("")));Have we found it before?
      LaTeX-style
    (save-excursion
      (goto-char (point-min))
      (if (and (re-search-forward 
		(concat "^[ \t]*"
			(regexp-quote TeX-esc) "documentstyle")
				  (+ (point) 2048) t)
	       (search-forward TeX-grop (+ (point) 512) t)) ;May be wrong!
	  (let ((beg (point)))
	    (skip-chars-forward (concat "^" TeX-grcl))	
	    (setq LaTeX-style (buffer-substring beg (point))))))))


(defun LaTeX-style-options ()
  "Return the name of the used style options in this LaTeX file."
  (interactive)
  (if (not (equal LaTeX-options '("")));Have we found it before?
      LaTeX-options
    (save-excursion
      (goto-char (point-min))
      (if (and (re-search-forward 
		(concat "^[ \t]*"
			(regexp-quote TeX-esc) "documentstyle")
				  (+ (point) 2048) t)
	       (search-forward LaTeX-optop (+ (point) 512) t)) ;May be wrong!
	  (let ((beg (point)))
	    (skip-chars-forward (concat "^" LaTeX-optcl))
	    (setq LaTeX-options (buffer-substring beg (point))))
	""
	))))


(defun split-string (char string)
  "Returns a list of strings. given REGEXP the STRING is split into 
sections which in string was seperated by regexp 

This function is used to seperate the arguments of

\\documentstyle\[style1,style2,style3\]{style}, and to seperate the 
arguments for TeX-print.  This allows us to use call-process directly without writing \"-c exec ...\", which some shells won't eat

Examples:

      (split-string \"\:\" \"abc:def:ghi\")
          -> (\"abc\" \"def\" \"ghi\")

      (split-string \" *\" \"dvips -Plw -p3 -c4 testfile.dvi\")

          -> (\"dvips\" \"-Plw\" \"-p3\" \"-c4\" \"testfile.dvi\")

If CHAR is nil, or \"\", an error will occur."

  (let ((regexp char)
	(start 0)
	(result '()))
    (while (string-match regexp string start)
      (let ((match (string-match regexp string start)))
	(setq result (cons (substring string start match) result))
	(setq start (+ 1 match))))
    (setq result (cons (substring string start nil) result))
    (nreverse result)))

(defvar TeX-last-fmt TeX-format-package
  "last used format package")

(defvar LaTeX-last-opt '("")
  "last used LaTeX style options")

(defvar LaTeX-last-sty '("")
  "last used LaTeX-style")

