;;; latex.el --- Support for LaTeX documents.
;; 
;; Maintainer: Per Abrahamsen <auc-tex@iesd.auc.dk>
;; Version: $Id: latex.el,v 5.17 1994-05-28 02:47:33 amanda Exp $
;; Keywords: wp

;; Copyright 1991 Kresten Krab Thorup
;; Copyright 1993, 1994 Per Abrahamsen
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
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

;;; Syntax

(defvar LaTeX-optop "["
  "The LaTeX optional argument opening character.")
(make-variable-buffer-local 'LaTeX-optop)

(defvar LaTeX-optcl "]"
  "The LaTeX optional argument closeing character.")
(make-variable-buffer-local 'LaTeX-optcl)

;;; Style

(defvar LaTeX-default-style "article"
  "*Default when creating new documents.")

  (make-variable-buffer-local 'LaTeX-default-style)

(defvar LaTeX-default-options nil
  "*Default options to documentstyle.
A list of strings.")

 (make-variable-buffer-local 'LaTeX-default-options)

;;; Syntax Table

(defvar LaTeX-mode-syntax-table (copy-syntax-table TeX-mode-syntax-table)
  "Syntax table used in LaTeX mode.")

(progn ; set [] to match for LaTeX.
  (modify-syntax-entry (string-to-char LaTeX-optop)
		       (concat "(" LaTeX-optcl) 
		       LaTeX-mode-syntax-table)  
  (modify-syntax-entry (string-to-char LaTeX-optcl)
		       (concat ")" LaTeX-optop)
		       LaTeX-mode-syntax-table))

;;; Sections

(defun LaTeX-section (arg)
  "Insert a template for a LaTeX section.
Determinate the type of section to be inserted, by the argument ARG.

If ARG is nil or missing, use the current level.
If ARG is a list (selected by C-u), go downward one level.
If ARG is negative, go up that many levels.
If ARG is positive or zero, use absolute level:

  0 : part
  1 : chapter
  2 : section
  3 : subsection
  4 : subsubsection
  5 : paragraph
  6 : subparagraph

The following variables can be set to customize:

LaTeX-section-hook	Hooks to run when inserting a section.
LaTeX-section-label	Prefix to all section labels."
  
  (interactive "*P")
  (let* ((val (prefix-numeric-value arg))
	 (level (cond ((null arg)
		       (LaTeX-current-section))
		      ((listp arg)
		       (LaTeX-down-section))
		      ((< val 0)
		       (LaTeX-up-section (- val)))
		      (t val)))
	 (name (LaTeX-section-name level))
	 (toc nil)
	 (title "")
	 (done-mark (make-marker)))
    (newline)
    (run-hooks 'LaTeX-section-hook)
    (newline)
    (if (marker-position done-mark)
	(goto-char (marker-position done-mark)))
    (set-marker done-mark nil)))

(defun LaTeX-current-section ()
  "Return the level of the section that contain point.
See also LaTeX-section for description of levels."
  (save-excursion
    (max (LaTeX-largest-level)
	 (if (re-search-backward (LaTeX-outline-regexp) nil t)
	     (- (LaTeX-outline-level) (LaTeX-outline-offset))
	   (LaTeX-largest-level)))))

(defun LaTeX-down-section ()
  "Return the value of a section one level under the current. Tries to
find what kind of section that have been used earlier in the text, If
this fail, it will just return one less than the current section."
  (save-excursion 
    (let ((current (LaTeX-current-section))
	  (next nil)
	  (regexp (LaTeX-outline-regexp)))
      (if (not (re-search-backward regexp nil t))
	  (1+ current)
	(while (not next)
	  (cond
	   ((eq (LaTeX-current-section) current)
	    (if (re-search-forward regexp nil t)
		(if (<= (setq next (LaTeX-current-section)) current) ;Wow!
		    (setq next (1+ current)))
	      (setq next (1+ current))))
	   ((not (re-search-backward regexp nil t))
	    (setq next (1+ current)))))
	next))))

(defun LaTeX-up-section (arg)
  "Return the value of the section ARG levels above this one."
  (save-excursion
    (if (zerop arg)
	(LaTeX-current-section)
      (let ((current (LaTeX-current-section)))
	(while (and (>= (LaTeX-current-section) current)
		    (re-search-backward (LaTeX-outline-regexp)
					nil t)))
	(LaTeX-up-section (1- arg))))))

(defvar LaTeX-section-list '(("part" 0)
			     ("chapter" 1)
			     ("section" 2)
			     ("subsection" 3)
			     ("subsubsection" 4)
			     ("paragraph" 5)
			     ("subparagraph" 6))
  "List which elements is the names of the sections used by LaTeX.")

(defun LaTeX-section-name (level)
  "Return the name of the section corresponding to LEVEL."
  (let ((entry (TeX-member level LaTeX-section-list
			   (function (lambda (a b) (equal a (nth 1 b)))))))
    (if entry
	(nth 0 entry)
      nil)))

(defun LaTeX-section-level (name)
  "Return the level of the section NAME."
  (let ((entry (TeX-member name LaTeX-section-list
			   (function (lambda (a b) (equal a (nth 0 b)))))))

    (if entry
	(nth 1 entry)
      nil)))

(defvar TeX-outline-extra nil
  "*List of extra TeX outline levels.

Each element is a list with two entries.  The first entry is the
regular expression matching a header, and the second is the level of
the header.  See LaTeX-section-list for existing header levels.")

(defun LaTeX-outline-regexp (&optional anywhere)
  "Return regexp for LaTeX sections.  

If optional argument ANYWHERE is not nil, do not require that the
header is at the start of a line."
  (concat (if anywhere "" "^")
	  "[ \t]*"
	  (regexp-quote TeX-esc)
	  "\\(appendix\\|documentstyle\\|"
	  (mapconcat 'car LaTeX-section-list "\\|")
	  "\\)\\b"
	  (if TeX-outline-extra
	      "\\|"
	    "")
	  (mapconcat 'car TeX-outline-extra "\\|")
	  "\\|" TeX-header-end
	  "\\|" TeX-trailer-start))

(defvar LaTeX-largest-level nil
  "Largest sectioning level with current document style")

(make-variable-buffer-local 'LaTeX-largest-level)

(defun LaTeX-largest-level ()
  (TeX-update-style)
  LaTeX-largest-level)

(defun LaTeX-outline-offset ()
  "Offset to add to LaTeX-section-list levels to get outline level."
  (- 2 (LaTeX-largest-level)))

(defun TeX-look-at (list)
  "Check if we are looking at the first element of a member of LIST.
If so, return the second element, otherwise return nil."
  (while (and list
	      (not (looking-at (nth 0 (car list)))))
    (setq list (cdr list)))
  (if list
      (nth 1 (car list))
    nil))

(defun LaTeX-outline-level ()
  "Find the level of current outline heading in an LaTeX document."
  (cond ((looking-at LaTeX-header-end) 1)
	((looking-at LaTeX-trailer-start) 1)
	((TeX-look-at TeX-outline-extra)
	 (max 1 (+ (TeX-look-at TeX-outline-extra)
		   (LaTeX-outline-offset))))
	(t
	 (save-excursion
	  (skip-chars-forward " \t")
	  (forward-char 1)
	  (cond ((looking-at "appendix") 1)
		((looking-at "documentstyle") 1)
		((TeX-look-at LaTeX-section-list)
		 (max 1 (+ (TeX-look-at LaTeX-section-list)
			   (LaTeX-outline-offset))))
		(t
		 (error "Unrecognized header.")))))))

(add-hook 'TeX-remove-style-hook
	  (function (lambda () (setq LaTeX-largest-level nil))))

(defvar LaTeX-section-hook
  '(LaTeX-section-heading
    LaTeX-section-title
;; LaTeX-section-toc		; Most people won't want this
    LaTeX-section-section
    LaTeX-section-label)
  "*List of hooks to run when a new section is inserted.

The following variables are set before the hooks are run

level - numeric section level, see the documentation of `LaTeX-section'.
name - name of the sectioning command, derived from `level'.
title - The title of the section, default to an empty string.
toc - Entry for the table of contents list, default nil.
done-mark - Position of point afterwards, default nil (meaning end).

The following standard hook exist -

LaTeX-section-heading: Query the user about the name of the
sectioning command.  Modifies `level' and `name'.

LaTeX-section-title: Query the user about the title of the
section. Modifies `title'.

LaTeX-section-toc: Query the user for the toc entry.  Modifies
`toc'. 

LaTeX-section-section: Insert LaTeX section command according to
`name', `title', and `toc'.  If `toc' is nil, no toc entry is
enserted.  If `toc' or `title' are empty strings, `done-mark' will be
placed at the point they should be inserted.

LaTeX-section-label: Insert a label after the section command.
Controled by the variable `LaTeX-section-label'.

To get a full featured LaTeX-section command, insert 

 (setq LaTeX-section-hook
       '(LaTeX-section-heading
	 LaTeX-section-title
	 LaTeX-section-toc
	 LaTeX-section-section
	 LaTeX-section-label))

in your .emacs file.")

(defvar LaTeX-section-label
  '(("chapter" . "cha:")
    ("section" . "sec:")
    ("subsection" . "sec:"))
  "*Default prefix when asking for a label.

If it is a string, it it used unchanged for all kinds of sections. 
If it is nil, no label is inserted.
If it is a list, the list is searched for a member whose car is equal
to the name of the sectioning command being inserted.  The cdr is then
used as the prefix.  If the name is not found, or if the cdr is nil,
no label is inserted.")

;;; Section Hooks.

(defun LaTeX-section-heading ()
  "Hook to prompt for LaTeX section name.
Insert this hook into LaTeX-section-hook to allow the user to change
the name of the sectioning command inserted with M-x LaTeX-section."
  (let ((string (completing-read
		 (concat "Select level: (default " name ") ")
		 LaTeX-section-list
		 nil nil nil)))
    ; Update name
    (if (not (zerop (length string)))
	(setq name string))
    ; Update level
    (setq level (LaTeX-section-level name))))

(defun LaTeX-section-title ()
  "Hook to prompt for LaTeX section title.
Insert this hook into LaTeX-section-hook to allow the user to change
the title of the section inserted with M-x LaTeX-section."
  (setq title (read-string "What title: ")))

(defun LaTeX-section-toc ()
  "Hook to prompt for the LaTeX section entry in the table of content .
Insert this hook into LaTeX-section-hook to allow the user to insert
a different entry for the section in the table of content."
  (setq toc (read-string "Toc Entry: "))
  (if (zerop (length toc))
      (setq toc nil)))

(defun LaTeX-section-section ()
  "Hook to insert LaTeX section command into the file.
Insert this hook into LaTeX-section-hook after those hooks which sets
the `name', `title', and `toc' variables, but before those hooks which
assumes the section already is inserted."
    (insert TeX-esc name)
    (cond ((null toc))
	  ((zerop (length toc))
	   (insert LaTeX-optop)
	   (set-marker done-mark (point))
	   (insert LaTeX-optcl))
	  (t
	   (insert LaTeX-optop toc LaTeX-optcl)))
    (insert TeX-grop)
    (if (zerop (length title))
	(set-marker done-mark (point)))
    (insert title TeX-grcl)
    (newline))

(defun LaTeX-section-label ()
  "Hook to insert a label after the sectioning command.
Insert this hook into LaTeX-section-hook to prompt for a label to be
inserted after the sectioning command.

The beaviour of this hook is controled by LaTeX-section-label."
  (let ((prefix (cond ((null LaTeX-section-label)
		       nil)
		      ((stringp LaTeX-section-label)
		       LaTeX-section-label)
		      ((assoc level LaTeX-section-label)
		       (cdr (assoc level LaTeX-section-label)))
		      ((assoc name LaTeX-section-label)
		       (cdr (assoc name LaTeX-section-label))))))
    (if prefix
	(let ((label (read-string "What label: " prefix)))
	  (if (string-equal prefix label)
	      ()				; Done - no label entered
	    (insert TeX-esc "label" TeX-grop label TeX-grcl)
	    (LaTeX-add-labels label)
	    (newline))))))

;;; Environments

(defvar LaTeX-default-environment "itemize"
  "*The default environment when creating new ones with LaTeX-environment.")
 (make-variable-buffer-local 'LaTeX-default-environment)

(defun LaTeX-environment (arg)
  "Make LaTeX environment (\\begin{...}-\\end{...} pair).
With optional ARG, modify current environment.
 
It may be customized with the following variables:
 
LaTeX-default-environment       Your favorite environment.
LaTeX-default-style             Your favorite document style.
LaTeX-default-options           Your favorite document style options.
LaTeX-float                     Where you want figures and tables to float.
LaTeX-table-label               Your prefix to labels in tables.
LaTeX-figure-label              Your prefix to labels in figures.
LaTeX-default-format            Format for array and tabular.
LaTeX-default-position          Position for array and tabular."
 
  (interactive "*P")
  (let ((environment (completing-read (concat "Environment type: (default "
                                               (if (TeX-near-bobp)
                                                   "document"
                                                 LaTeX-default-environment)
                                               ") ")
                                       (LaTeX-environment-list))))
    ;; Get default
    (cond ((and (zerop (length environment))
                (TeX-near-bobp))
           (setq environment "document"))
          ((zerop (length environment))
           (setq environment LaTeX-default-environment))
          (t
           (setq LaTeX-default-environment environment)))
 
    (let ((entry (assoc environment (LaTeX-environment-list))))
      (if (null entry)
          (LaTeX-add-environments (list environment)))
 
      (if arg
	  (LaTeX-modify-environment environment)
	(LaTeX-environment-menu environment)))))

(defun LaTeX-environment-menu (environment)
  ;; Insert ENVIRONMENT around point or region. 
  (let ((entry (assoc environment (LaTeX-environment-list))))
    (cond ((not (and entry (nth 1 entry)))
	   (LaTeX-insert-environment environment))
	  ((numberp (nth 1 entry))
	   (let ((count (nth 1 entry))
		 (args ""))
	     (while (> count 0)
	       (setq args (concat args TeX-grop TeX-grcl))
	       (setq count (- count 1)))
	     (LaTeX-insert-environment environment args)))
	  ((stringp (nth 1 entry))
	   (let ((prompts (cdr entry))
		 (args ""))
	     (while prompts
	       (setq args (concat args
				  TeX-grop
				  (read-from-minibuffer (concat (car prompts)
								": "))
				  TeX-grcl))
	       (setq prompts (cdr prompts)))
	     (LaTeX-insert-environment environment args)))
	  (t
	   (apply (nth 1 entry) environment (nthcdr 2 entry))))))

(defun LaTeX-close-environment ()
  "Creates an \\end{...} to match the current environment."
  (interactive "*")
  (if (not (save-excursion
             (beginning-of-line)
             (looking-at "^[ \t]*$")))
      (insert "\n"))
  (insert "\\end{" (LaTeX-current-environment 1) "}")
  (LaTeX-indent-line)
  (insert "\n")
  (LaTeX-indent-line))

(autoload 'outline-flag-region "outline")

(defun LaTeX-hide-environment ()
  "Hide current LaTeX environment using selective display."
  (interactive)
  (outline-flag-region (save-excursion (LaTeX-find-matching-begin) (point))
		       (save-excursion (LaTeX-find-matching-end) (point))
		       ?\r))

(defun LaTeX-show-environment ()
  "Show current LaTeX environment."
  (interactive)
  (outline-flag-region (save-excursion (LaTeX-find-matching-begin) (point))
		       (save-excursion (LaTeX-find-matching-end) (point))
		       ?\n))

(defun LaTeX-insert-environment (environment &optional extra)
  "Insert environment of type ENV, with optional argument EXTRA."
  (if (TeX-active-mark)
      (progn 
	(if (< (mark) (point))
	    (exchange-point-and-mark))
	(insert TeX-esc "begin" TeX-grop environment TeX-grcl)
	(LaTeX-indent-line)
	(if extra (insert extra))
	(newline)
	(goto-char (mark))
	(insert TeX-esc "end" TeX-grop environment TeX-grcl)
	(LaTeX-indent-line)
	(end-of-line 0)
	(or (string-match "^verbatim" environment)
	    (LaTeX-format-environment nil)))
    (insert TeX-esc "begin" TeX-grop environment TeX-grcl)
    (LaTeX-indent-line)
    (if extra (insert extra))
    (newline-and-indent)
    (newline)
    (insert TeX-esc "end" TeX-grop environment TeX-grcl)
    (LaTeX-indent-line)
    (end-of-line 0)))

(defun LaTeX-modify-environment (environment)
  ;; Modify current environment.
  (save-excursion
    (LaTeX-find-matching-end)
    (re-search-backward (concat (regexp-quote TeX-esc)
				"end"
				(regexp-quote TeX-grop)
				" *\\([a-zA-Z*]*\\)"
				(regexp-quote TeX-grcl))
			(save-excursion (beginning-of-line 1) (point)))
    (replace-match (concat TeX-esc "end" TeX-grop environment TeX-grcl) t t)
    (beginning-of-line 1)
    (LaTeX-find-matching-begin)
    (re-search-forward (concat (regexp-quote TeX-esc)
			       "begin"
			       (regexp-quote TeX-grop)
			       " *\\([a-zA-Z*]*\\)"
			       (regexp-quote TeX-grcl))
		       (save-excursion (end-of-line 1) (point)))
    (replace-match (concat TeX-esc "begin" TeX-grop environment TeX-grcl) t t)))

(defun LaTeX-current-environment (&optional arg)
  "Return the name (a string) of the enclosing LaTeX environment.
With optional ARG>=1, find that outer level."
  (setq arg (if arg (if (< arg 1) 1 arg) 1))
  (save-excursion
    (while (and
	    (/= arg 0)
	    (re-search-backward
	     (concat (regexp-quote TeX-esc) "begin" (regexp-quote TeX-grop)
		     "\\|"
		     (regexp-quote TeX-esc) "end" (regexp-quote TeX-grop)) 
	     nil t 1))
      (if (looking-at (concat (regexp-quote TeX-esc)
			      "end" (regexp-quote TeX-grop)))
	  (setq arg (1+ arg))
	(setq arg (1- arg))))
    (if (/= arg 0)
	"document"
      (search-forward TeX-grop)
      (let ((beg (point)))
	(search-forward TeX-grcl)
	(backward-char 1)
	(buffer-substring beg (point))))))

(defun TeX-near-bobp ()
  ;; Return t iff there's nothing but whitespace between (bob) and (point).
  (save-excursion
    (skip-chars-backward " \t\n")
    (bobp)))

;;; Environment Hooks

(defvar LaTeX-document-style-hook nil
  "List of hooks to run when inserting a document style environment.

To insert a hook here, you must insert it in the appropiate style file.")

(defun LaTeX-env-document (&optional ignore)
  "Create new LaTeX document."

  (TeX-insert-macro (if (string-equal LaTeX-version "2")
			"documentstyle"
		      "documentclass"))

  (newline 3)
  (end-of-line 0)
  (LaTeX-insert-environment "document")
  (run-hooks 'LaTeX-document-style-hook)
  (setq LaTeX-document-style-hook nil))

(defvar LaTeX-float "htbp"
  "*Default float when creating figure and table environments.")
 (make-variable-buffer-local 'LaTeX-float)

(defvar LaTeX-figure-label "fig:"
  "*Default prefix to figure labels.")
 (make-variable-buffer-local 'LaTeX-figure-label)

(defvar LaTeX-table-label "tab:"
  "*Default prefix to table labels.")
 (make-variable-buffer-local 'LaTeX-table-label)

(defvar LaTeX-default-format ""
  "Specifies the default format string for array and tabular environments.")
 (make-variable-buffer-local 'LaTeX-default-format)

(defvar LaTeX-default-position ""
  "Specifies the default position string for array and tabular environments.")
 (make-variable-buffer-local 'LaTeX-default-position)

(defun LaTeX-env-item (environment)
  "Insert ENVIRONMENT and the first item."
  (LaTeX-insert-environment environment)
  (if (TeX-active-mark)
      (progn
	(LaTeX-find-matching-begin)
	(end-of-line 1))
    (end-of-line 0))
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(defun LaTeX-env-figure (environment)
  "Create ENVIRONMENT with \\label and \\caption commands."
  (let ((float (read-string "Float to: " LaTeX-float))
	(caption (read-string "Caption: "))
	(label (completing-read "Label: "
				(LaTeX-label-list)
				nil nil
				(cond ((string= "figure" environment)
				       LaTeX-figure-label)
				      ((string= "table" environment)
				       LaTeX-table-label)
				      (t nil))))
        ; gf: ask if this should be centered
        (center (y-or-n-p "Center: ")))

    (setq LaTeX-float (if (zerop (length float))
			  LaTeX-float
			float))
	  
    (LaTeX-insert-environment environment
			      (concat LaTeX-optop LaTeX-float LaTeX-optcl))
    
    (if (or (zerop (length label))
	    (and (string= "figure" environment)
		 (equal LaTeX-figure-label label))
	    (and (string= "table" environment)
		 (equal LaTeX-table-label label)))
	()
      (newline-and-indent)
      (insert TeX-esc "label" TeX-grop label TeX-grcl)
      (LaTeX-add-labels label)
      (end-of-line 0)
      (LaTeX-indent-line))

    (if (zerop (length caption))
	()
      (newline-and-indent)
      (insert TeX-esc "caption" TeX-grop caption TeX-grcl)
      (end-of-line 0)
      (LaTeX-indent-line))
    
    ;; gf: add center if requested
    (if center
	(progn
	  (LaTeX-insert-environment "center")
	  (insert "\\leavevmode")
	  (newline-and-indent)))
    
    ;; gf: Add tabular if we're in a floating table
    (if (string= environment "table") (LaTeX-env-array "tabular"))))

(defun LaTeX-env-array (environment)
  "Insert ENVIRONMENT with position and column specifications 
like array and tabular."
  (let ((pos (read-string "Position: "))
	(fmt (read-string "Format: " LaTeX-default-format)))
    (setq LaTeX-default-position pos)
      (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			        (if (not (zerop (length pos)))
				    (format "[%s]" pos))
				(format "{%s}" fmt)))
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-label (environment)
  "Insert ENVIRONMENT and prompt for label."
  (let ((label (completing-read "Label: " (LaTeX-label-list))))
    (LaTeX-insert-environment environment)
    (if (not (zerop (length label)))
	(progn
	  (insert TeX-esc "label" TeX-grop label TeX-grcl)
	  (LaTeX-add-labels label)
	  (newline-and-indent)))))

(defun LaTeX-env-list (environment)
  "Insert ENVIRONMENT and the first item."
  (let ((label (read-string "Default Label: ")))
    (LaTeX-insert-environment environment
			      (format "{%s}{}" label))
    (end-of-line 0)
    (delete-char 1)
    (delete-horizontal-space))
  (LaTeX-insert-item))

(defun LaTeX-env-minipage (environment)
  "Create new LaTeX minipage."
  (let ((pos (read-string "Position: " LaTeX-default-position))
	(width (read-string "Width: ")))
    (setq LaTeX-default-position pos)
    (if (zerop (length width))
	(setq width "4cm"))
    (LaTeX-insert-environment environment
			      (concat (if (not (zerop (length pos)))
					  (format "[%s]" pos))
				      (format "{%s}" width)))
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-tabular* (environment)
  "Insert ENVIRONMENT with width, position and column specifications."
  (let ((width (read-string "Width: "))
	(pos (read-string "Position: " LaTeX-default-position))
	(fmt (read-string "Format: " LaTeX-default-format)))
    (setq LaTeX-default-position pos)
    (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			        (if (not (zerop (length width)))
				    (format "{%s}" width))
			        (if (not (zerop (length pos)))
					  (format "[%s]" pos))
				(format "{%s}" fmt)))
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-picture (environment)
  "Insert ENVIRONMENT with width, height specifications."
  (let ((width (read-string "Width: "))
	(height (read-string "Height: "))
	(x-offset (read-string "X Offset: "))
	(y-offset (read-string "Y Offset: ")))
    (if (zerop (length x-offset))
	(setq x-offset "0"))
    (if (zerop (length y-offset))
	(setq y-offset "0"))
    (LaTeX-insert-environment environment
			      (concat (format "(%s,%s)" width height)
				      (if (not (and (string= x-offset "0")
						    (string= y-offset "0")))
					  (format "(%s,%s)" x-offset y-offset))))
				      
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-bib (environment)
  "Insert ENVIRONMENT with label for bibitem."
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "Label for BibItem: " "99")
				    TeX-grcl))
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

;;; Item hooks

(defvar LaTeX-item-list nil
  "An list of environments where items have a special syntax. 
The cdr is the name of the function, used to insert this kind of items.")

(defun LaTeX-insert-item ()
  "Insert a new item in an environment.
You may use LaTeX-item-list to change the routines used to insert the item."
  (interactive "*")
  (let ((environment (LaTeX-current-environment)))
    (newline)
    (if (assoc environment LaTeX-item-list)
	(funcall (cdr (assoc environment LaTeX-item-list)))
      (TeX-insert-macro "item"))
    (LaTeX-indent-line)))

(defun LaTeX-item-argument ()
  "Insert a new item with an optional argument."
  (let ((TeX-arg-item-label-p t))
    (TeX-insert-macro "item")))

(defun LaTeX-item-bib ()
  "Insert a new bibitem."
  (TeX-insert-macro "bibitem"))

;;; Parser

(defvar LaTeX-auto-minimal-regexp-list
  '(("\\\\document\\(style\\|class\\)\
\\(\\[\\(\\([^#\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)\\]\\)?\
{\\([^#\\\\\\.\n\r]+\\)}"
     (3 5) LaTeX-auto-style))
  "Minimal list of regular expressions matching LaTeX macro definitions.")

(defvar LaTeX-auto-label-regexp-list
  '(("\\\\label{\\([^\n\r%\\{}]+\\)}" 1 LaTeX-auto-label))
  "List of regular expression matching LaTeX labels only.")

(defvar LaTeX-auto-regexp-list 
  (append
   '(("\\\\newcommand{?\\\\\\([a-zA-Z]+\\)}?\\[\\([0-9]+\\)\\]\
\\[\\([^\]\\\\\n\r]+\\)\\]"
      (1 2 3) LaTeX-auto-optional)
     ("\\\\newcommand{?\\\\\\([a-zA-Z]+\\)}?\\[\\([0-9]+\\)\\]"
      (1 2) LaTeX-auto-arguments)
     ("\\\\newcommand{?\\\\\\([a-zA-Z]+\\)}?" 1 TeX-auto-symbol)
     ("\\\\newenvironment{?\\([a-zA-Z]+\\)}?\\[\\([0-9]+\\)\\]"
      (1 2) LaTeX-auto-env-args)
     ("\\\\newenvironment{?\\([a-zA-Z]+\\)}?" 1 LaTeX-auto-environment)
     ("\\\\newtheorem{\\([a-zA-Z]+\\)}" 1 LaTeX-auto-environment)
     ("\\\\input{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\include{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\usepackage\\(\\[[^\]\\\\]*\\]\\)?\
{\\(\\([^#}\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)}"
      (2) LaTeX-auto-style)
     ("\\\\bibitem{\\([a-zA-Z][^, \n\r\t%\"#'()={}]*\\)}" 1 LaTeX-auto-bibitem)
     ("\\\\bibitem\\[[^][\n\r]+\\]{\\([a-zA-Z][^, \n\r\t%\"#'()={}]*\\)}"
      1 LaTeX-auto-bibitem)
     ("\\\\bibliography{\\([^#}\\\\\n\r]+\\)}" 1 LaTeX-auto-bibliography))
   LaTeX-auto-label-regexp-list
   LaTeX-auto-minimal-regexp-list)
  "List of regular expression matching common LaTeX macro definitions.")

(defun LaTeX-auto-prepare ()
  ;; Prepare for LaTeX parsing.
  (setq LaTeX-auto-arguments nil
	LaTeX-auto-optional nil
	LaTeX-auto-env-args nil
	LaTeX-auto-style nil
	LaTeX-auto-end-symbol nil))

(add-hook 'TeX-auto-prepare-hook 'LaTeX-auto-prepare)
  
(defun LaTeX-auto-cleanup ()
  ;; Cleanup after LaTeX parsing.

  ;; Cleanup BibTeX files
  (setq LaTeX-auto-bibliography
	(apply 'append (mapcar (function (lambda (arg)
					   (TeX-split-string "," arg)))
			       LaTeX-auto-bibliography)))
    
  ;; Cleanup document styles and packages
  (if (null LaTeX-auto-style)
      ()
    (while LaTeX-auto-style
      (let* ((entry (car LaTeX-auto-style))
	     (options (nth 0 entry))
	     (style (nth 1 entry)))

	;; Next document style.
	(setq LaTeX-auto-style (cdr LaTeX-auto-style))

	;; Get the options.
	(setq options (TeX-split-string 
		       "\\([ \t\r\n]\\|%[^\n\r]*[\n\r]\\|,\\)+"
		       options))

	;; Strip empty options.
	(if (string-equal (car options) "")
	    (setq options (cdr options)))
	(let ((index options))
	  (while (cdr-safe index)
	    (if (string-equal (car (cdr index)) "")
		(setcdr index (cdr (cdr index)))
	      (setq index (cdr index)))))

	;; Add them, to the style list.
	(setq TeX-auto-file (append options TeX-auto-file))

	;; The second argument if present is a normal style file.
	(if (null style)
	    ()
	  (setq TeX-auto-file (cons style TeX-auto-file))

	  ;; And a special "art10" style file combining style and size.
	  (setq TeX-auto-file
		(cons (concat 
		       (cond ((string-equal "article" style)
			      "art")
			     ((string-equal "book" style)
			      "bk")
			     ((string-equal "report" style)
			      "rep")
			     ((string-equal "jarticle" style)
			      "jart")
			     ((string-equal "jbook" style)
			      "jbk")
			     ((string-equal "jreport" style)
			      "jrep")
			     ((string-equal "j-article" style)
			      "j-art")
			     ((string-equal "j-book" style)
			      "j-bk")
			     ((string-equal "j-report" style )
			      "j-rep")
			     (t style))
		       (cond ((member "11pt" options)
			      "11")
			     ((member "12pt" options)
			      "12")
			     (t
			      "10")))
		      TeX-auto-file))))))
    
  ;; Cleanup optional arguments
  (mapcar (function (lambda (entry)
		      (setq TeX-auto-symbol
			    (cons (list (nth 0 entry)
					(string-to-int (nth 1 entry)))
				  TeX-auto-symbol))))
	  LaTeX-auto-arguments)

  ;; Cleanup default optional arguments
  (mapcar (function (lambda (entry)
		      (setq TeX-auto-symbol
			    (cons (list (nth 0 entry)
					(vector "argument")
					(1- (string-to-int (nth 1 entry))))
				  TeX-auto-symbol))))
	  LaTeX-auto-optional)

  ;; Cleanup environments arguments
  (mapcar (function (lambda (entry)
		      (setq LaTeX-auto-environment
			    (cons (list (nth 0 entry)
					(string-to-int (nth 1 entry)))
				  LaTeX-auto-environment))))
	  LaTeX-auto-env-args)
    
  ;; Cleanup use of def to add environments
  ;; NOTE: This uses an O(N^2) algorithm, while an O(N log N)
  ;; algorithm is possible.
  (mapcar (function (lambda (symbol)
		      (if (not (TeX-member symbol TeX-auto-symbol 'equal))
			  ;; No matching symbol, insert in list
			  (setq TeX-auto-symbol
				(cons (concat "end" symbol) TeX-auto-symbol))
			;; Matching symbol found, remove from list
			(if (equal (car TeX-auto-symbol) symbol)
			    ;; Is it the first symbol?
			    (setq TeX-auto-symbol (cdr TeX-auto-symbol))
			  ;; Nope!  Travel the list
			  (let ((list TeX-auto-symbol))
			    (while (consp (cdr list))
			      ;; Until we find it.
			      (if (equal (car (cdr list)) symbol)
				  ;; Then remove it.
				  (setcdr list (cdr (cdr list))))
			      (setq list (cdr list)))))
			;; and add the symbol as an environment.
			(setq LaTeX-auto-environment
			      (cons symbol LaTeX-auto-environment)))))
	  LaTeX-auto-end-symbol))

(add-hook 'TeX-auto-cleanup-hook 'LaTeX-auto-cleanup)

(TeX-auto-add-type "label" "LaTeX")
(TeX-auto-add-type "bibitem" "LaTeX")
(TeX-auto-add-type "environment" "LaTeX")
(TeX-auto-add-type "bibliography" "LaTeX" "bibliographies")

(fset 'LaTeX-add-bibliographies-auto
      (symbol-function 'LaTeX-add-bibliographies))
(defun LaTeX-add-bibliographies (&rest bibliographies)
  "Add BIBLIOGRAPHIES to the list of known bibliographies and style files."
  (apply 'LaTeX-add-bibliographies-auto bibliographies)
  (apply 'TeX-run-style-hooks bibliographies))

(fset 'LaTeX-add-environments-auto
      (symbol-function 'LaTeX-add-environments))
(defun LaTeX-add-environments (&rest environments)
  "Add ENVIRONMENTS to the list of known environments."
  (apply 'LaTeX-add-environments-auto environments)
  (setq LaTeX-menu-changed t))

;;; BibTeX

;;;###autoload
(defun BibTeX-auto-store ()
  "This function should be called from bibtex-mode-hook.
It will setup BibTeX to store keys in an auto file."
  ;; We want this to be early in the list, so we do not
  ;; add it before we enter BibTeX mode the first time. 
  (if (boundp 'local-write-file-hooks)
      (add-hook 'local-write-file-hooks 'TeX-safe-auto-write)
    (add-hook 'write-file-hooks 'TeX-safe-auto-write))
  (make-local-variable 'TeX-auto-update)
  (setq TeX-auto-update 'BibTeX)
  (make-local-variable 'TeX-auto-untabify)
  (setq TeX-auto-untabify nil)
  (make-local-variable 'TeX-auto-regexp-list)
  (setq TeX-auto-regexp-list BibTeX-auto-regexp-list))

(defvar BibTeX-auto-regexp-list
  '(("@[Ss][Tt][Rr][Ii][Nn][Gg]" 1 ignore)
    ("@[a-zA-Z]+[{(][ \t]*\\([a-zA-Z][^, \n\r\t%\"#'()={}]*\\)"
     1 LaTeX-auto-bibitem))
  "List of regexp-list expressions matching BibTeX items.")

;;; Macro Argument Hooks

(defun TeX-arg-conditional (optional expr then else)
  "Implement if EXPR THEN ELSE.

If EXPR evaluate to true, parse THEN as an argument list, else parse
ELSE as an argument list."
  (TeX-parse-arguments (if (eval expr) then else)))

(defun TeX-arg-free (optional &optional &rest args)
  "Parse its arguments but use no braces when they are inserted."
  (let ((< "")
	(> ""))
    (if (equal (length args) 1)
	(TeX-parse-argument optional (car args))
      (TeX-parse-argument optional args))))

(defun TeX-arg-literal (optional &optional &rest args)
  "Insert its arguments into the buffer.  
Used for specifying extra syntax for a macro."
  (apply 'insert args))

(defun TeX-arg-string (optional &optional prompt input)
  "Prompt for a string."
  (TeX-argument-insert
   (read-string (TeX-argument-prompt optional prompt "Text") input)
   optional))

(defun TeX-arg-eval (optional &rest args)
  "Evaluate args and insert value in buffer."
  (TeX-argument-insert (eval args) optional))

(defun TeX-arg-label (optional &optional prompt definition)
  "Prompt for a label completing with known labels."
  (let ((label (completing-read (TeX-argument-prompt optional prompt "Key")
				(LaTeX-label-list))))
    (if (and definition (not (string-equal "" label)))
	(LaTeX-add-labels label))
    (TeX-argument-insert label optional optional)))

(defun TeX-arg-macro (optional &optional prompt definition)
  "Prompt for a TeX macro with completion."
  (let ((macro (completing-read (TeX-argument-prompt optional prompt
						     (concat "Macro: "
							     TeX-esc)
						     t)
				(TeX-symbol-list))))
    (if (and definition (not (string-equal "" macro)))
	(TeX-add-symbols macro))
    (TeX-argument-insert macro optional TeX-esc)))

(defun TeX-arg-environment (optional &optional prompt definition)
  "Prompt for a LaTeX environment with completion."
  (let ((environment (completing-read (TeX-argument-prompt optional prompt
							   "Environment")
				      (TeX-symbol-list))))
    (if (and definition (not (string-equal "" environment)))
	(LaTeX-add-environments environment))

    (TeX-argument-insert environment optional)))

(defun TeX-arg-cite (optional &optional prompt definition)
  "Prompt for a BibTeX citation with completion."
  (let ((bibitem (completing-read (TeX-argument-prompt optional prompt "Key")
				(LaTeX-bibitem-list))))
    (if (and definition (not (string-equal "" bibitem)))
	(LaTeX-add-bibitems bibitem))
    (TeX-argument-insert bibitem optional optional)))

(defun TeX-arg-counter (optional &optional prompt definition)
  "Prompt for a LaTeX counter."
  ;; Completion not implemented yet.
  (TeX-argument-insert
   (read-string (TeX-argument-prompt optional prompt "Counter"))
   optional))

(defun TeX-arg-savebox (optional &optional prompt definition)
  "Prompt for a LaTeX savebox."
  ;; Completion not implemented yet.
  (TeX-argument-insert
   (read-string (TeX-argument-prompt optional prompt
				     (concat "Savebox: " TeX-esc)
				     t))
   optional TeX-esc))

(defun TeX-arg-file (optional &optional prompt)
  "Prompt for a filename in the current directory."
  (TeX-argument-insert (read-file-name (TeX-argument-prompt optional
							    prompt "File")
				       "" "" nil)
		       optional))

(defun TeX-arg-define-label (optional &optional prompt)
  "Prompt for a label completing with known labels."
  (TeX-arg-label optional prompt t))

(defun TeX-arg-define-macro (optional &optional prompt)
  "Prompt for a TeX macro with completion."
  (TeX-arg-macro optional prompt t))

(defun TeX-arg-define-environment (optional &optional prompt)
  "Prompt for a LaTeX environment with completion."
  (TeX-arg-environment optional prompt t))

(defun TeX-arg-define-cite (optional &optional prompt)
  "Prompt for a BibTeX citation."
  (TeX-arg-cite optional prompt t))

(defun TeX-arg-define-counter (optional &optional prompt)
  "Prompt for a LaTeX counter."
  (TeX-arg-counter optional prompt t))

(defun TeX-arg-define-savebox (optional &optional prompt)
  "Prompt for a LaTeX savebox."
  (TeX-arg-savebox optional prompt t))

(defvar LaTeX-style-list '(("book")
			   ("article")
			   ("letter")
			   ("slides")
			   ("report"))
  "*List of document styles.")

  (make-variable-buffer-local 'LaTeX-style-list)

(defun TeX-arg-document (optional &optional ignore)
  "Insert arguments to documentstyle and documentclass."
  (let ((style (completing-read 
		(concat "Document style: (default " LaTeX-default-style ") ")
		LaTeX-style-list))
	(options (read-string "Options: "
			      (if (stringp LaTeX-default-options)
				  LaTeX-default-options
				(mapconcat 'identity
					   LaTeX-default-options
					   ",")))))
    (if (zerop (length style))
	(setq style LaTeX-default-style))
    (if (not (zerop (length options)))
	(insert LaTeX-optop options LaTeX-optcl))
    (insert TeX-grop style TeX-grcl))

  ;; remove old information
  (TeX-remove-style)

  ;; defined in individual style hooks
  (TeX-update-style))

(defun TeX-arg-input-file (optional &optional prompt local)
  "Prompt for a tex or sty file.
First optional argument is the promt, the second is a flag.  If the
flag is set, only complete with local files."
  (message "Searching for files...")
  (let ((file (if TeX-check-path
		  (completing-read
		   (TeX-argument-prompt optional prompt "File")
		   (mapcar 'list (TeX-search-files TeX-check-path
						   TeX-file-extensions t t)))
		(read-file-name
		 (TeX-argument-prompt optional prompt "File")))))
    (if (null file)
	(setq file ""))
    (if (not (string-equal "" file))
	(TeX-run-style-hooks file))
    (TeX-argument-insert file optional)))

(defvar BibTeX-global-style-files nil
  "*Association list of BibTeX style files.

If nil, AUC TeX will search for them.")

(defun TeX-arg-bibstyle (optional &optional prompt)
  "Prompt for a BibTeX style file."
  (message "Searching for BibTeX styles...")
  (or BibTeX-global-style-files
      (setq BibTeX-global-style-files
	    (mapcar 'list
		    (TeX-search-files (append TeX-macro-private
					      TeX-macro-global)
				      BibTeX-style-extensions t t))))

  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "BibTeX style")
		    (append (mapcar 'list
				    (TeX-search-files '(".")
						      BibTeX-style-extensions
						      t t))
			    BibTeX-global-style-files))
   optional))

(defvar BibTeX-global-files nil
  "*Association list of BibTeX files.

If nil, AUC TeX will search for them.")

(defun TeX-arg-bibliography (optional &optional prompt)
  "Prompt for a BibTeX database file."
  (message "Searching for BibTeX files...")
  (or BibTeX-global-files
      (setq BibTeX-global-files
	    (mapcar 'list (TeX-search-files nil BibTeX-file-extensions t t))))
  
  (let ((file (completing-read
	       (TeX-argument-prompt optional prompt "BibTeX file")
	       (append (mapcar 'list
				    (TeX-search-files '(".")
						      BibTeX-file-extensions
						      t t))
		      BibTeX-global-files))))
    (TeX-argument-insert file optional)
    (if (string-equal file "")
	()
      (let ((styles (TeX-split-string "," file)))
	(LaTeX-add-bibliographies styles)))))

(defun TeX-arg-corner (optional &optional prompt)
  "Prompt for a LaTeX side or corner position with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("l") ("r") ("t") ("b") ("tl") ("tr") ("bl") ("br"))
		    nil t)
   optional))

(defun TeX-arg-lr (optional &optional prompt)
  "Prompt for a LaTeX side with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("l") ("r"))
		    nil t)
   optional))

(defun TeX-arg-tb (optional &optional prompt)
  "Prompt for a LaTeX side with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("t") ("b"))
		    nil t)
   optional))

(defun TeX-arg-pagestyle (optional &optional prompt)
  "Prompt for a LaTeX pagestyle with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Pagestyle")
		    '(("plain") ("empty") ("headings") ("myheadings")))
   optional))

(defun TeX-arg-verb (optional &optional ignore)
  "Prompt for delimiter and text."
  (let ((del (read-quoted-char "Delimiter: "))
	(text (read-from-minibuffer "Text: ")))
    (insert del text del)))

(defun TeX-arg-pair (optional first second)
  "Insert a pair of number, prompted by FIRST and SECOND.

The numbers are surounded by parenthesizes and separated with a
comma."
  (insert "(" (read-string (concat first  ": ")) ","
	      (read-string (concat second ": ")) ")"))

(defun TeX-arg-size (optional)
  "Insert width and height as a pair."
  (TeX-arg-pair optional "Width" "Height"))

(defun TeX-arg-coordinate (optional)
  "Insert x and y coordinate as a pair."
 (TeX-arg-pair optional "X position" "Y position"))

(defconst TeX-braces-default-association
  '(("[" . "]")
    ("\\{" . "\\}")
    ("(" . ")")
    ("|" . "|")
    ("\\|" . "\\|")
    ("/" . "/")
    ("\\backslash" . "\\backslash")
    ("\\lfloor" . "\\rfloor")
    ("\\lceil" . "\\rceil")
    ("\\langle" . "\\rangle")))

(defvar TeX-braces-user-association nil
  "A list of your personal association of brace symbols for \\left and \\right

The car of each entry is the brace used with \\left,
the cdr is the brace used with \\right.")

(defvar TeX-braces-association
  (append TeX-braces-user-association
          TeX-braces-default-association)
    "A list of association of brace symbols for \\left and \\right.
The car of each entry is the brace used with \\left,
the cdr is the brace used with \\right.")

(defvar TeX-left-right-braces
  '(("[") ("]") ("\\{") ("\\}") ("(") (")") ("|") ("\\|")
    ("/") ("\\backslash") ("\\lfloor") ("\\rfloor")
    ("\\lceil") ("\\rceil") ("\\langle") ("\\rangle")
    ("\\uparrow") ("\\Uparrow") ("\\downarrow") ("\\Downarrow")
    ("\\updownarrow") ("\\Updownarrow") ("."))
  "List of symbols which can follow the \\left or \\right command")

(defun TeX-arg-insert-braces (optional &optional prompt)
  (let ((left-brace (completing-read
                     (TeX-argument-prompt optional prompt "Which brace")
                     TeX-left-right-braces)))
    (insert left-brace " ")
    (save-excursion
      (let ((right-brace (cdr (assoc left-brace
                                     TeX-braces-association))))
        (insert " " TeX-esc "right")
        (if (and TeX-arg-right-insert-p
                 right-brace)
            (insert right-brace)
          (insert (completing-read
                   (TeX-argument-prompt optional prompt "Which brace")
                   TeX-left-right-braces)))))))

;;; Indentation

(defvar LaTeX-indent-level 2
  "*Indentation of begin-end blocks in LaTeX.")

(defvar LaTeX-item-indent -2
  "*Extra indentation for lines beginning with an item.")

(defvar LaTeX-item-regexp "\\(bib\\)?item\\b"
  "*Regular expression matching macros considered items.")

(defun LaTeX-indent-line ()
  "Indent the line containing point, as LaTeX source.
Add LaTeX-indent-level indentation in each \\begin{ - \\end{ block.
Lines starting with an item is given an extra indentation of
LaTeX-item-indent."
  (interactive)
  (let ((indent (LaTeX-indent-calculate)))
    (save-excursion
      (if (/= (current-indentation) indent)
	  (let ((beg (progn
		       (beginning-of-line)
		       (point))))
	    (back-to-indentation)
	    (delete-region beg (point))
	    (indent-to indent))))
    (if (< (current-column) indent)
	(back-to-indentation))))

(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "*r\nP")
  (save-restriction
    (goto-char from)
    (skip-chars-forward " \n")
    (LaTeX-indent-line)
    (beginning-of-line)
    (narrow-to-region (point) to)
    (setq from (point))
    
    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.
    
    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    (while (re-search-forward "[.?!][])\"']*$" nil t)
      (insert ? ))
    ;; The change all newlines to spaces.
    (subst-char-in-region from (point-max) ?\n ?\ )
    ;; Flush excess spaces, except in the paragraph indentation.
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward "   *" nil t)
      (delete-region
       (+ (match-beginning 0)
	  (if (save-excursion
		(skip-chars-backward " ])\"'")
		(memq (preceding-char) '(?. ?? ?!)))
	      2 1))
       (match-end 0)))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0))
      (while (not (eobp))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  (skip-chars-backward "^ \n")
	  (if (if (zerop prefixcol)
		  (bolp)
		(>= prefixcol (current-column)))
	      (skip-chars-forward "^ \n")
	    (forward-char -1)))
	(delete-horizontal-space)
	(if (equal (preceding-char) ?\\)
	    (insert ? ))
	(insert ?\n)
	(LaTeX-indent-line)
	(setq prefixcol (current-column))
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))
	)
      (goto-char (point-max))
      (delete-horizontal-space))))


(defun LaTeX-format-paragraph (prefix)
  "Fill and indent paragraph at or after point.
Prefix arg means justify as well."
  (interactive "*P")
  (save-excursion
    (beginning-of-line)
    (forward-paragraph)
    (or (bolp) (newline 1))
    (and (eobp) (open-line 1))
    (let ((end (point-marker))
	  (start (progn
		   (backward-paragraph)
		   (point))))
      (LaTeX-fill-region-as-paragraph start end prefix))))

(defun LaTeX-format-region (from to &optional justify what)
  "Fill and indent each of the paragraphs in the region as LaTeX text.
Prefix arg (non-nil third arg, if called from program)
means justify as well. Fourth arg WHAT is a word to be displayed when
formatting."
  (interactive "*r\nP")
  (save-restriction
    (save-excursion
      (let ((length (- to from))) 
	(goto-char from)
	(beginning-of-line)
	(while (< (point) to)
	  (message "Formatting%s ... %d%%"
		   (if (not what)
		       ""
		     what)
		   (/ (* 100 (- (point) from)) length))
	  (save-excursion (LaTeX-format-paragraph justify))
	  (forward-paragraph 2)
	  (if (not (eobp))
	      (backward-paragraph)))))
    (message "Finished")))

(defun LaTeX-find-matching-end ()
  "Move point to the \\end of the current environment"
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
	(level 1))
    (beginning-of-line 1)
    (if (looking-at (concat " *" (regexp-quote TeX-esc) "begin\\b"))
	(end-of-line 1))
    (while (and (> level 0) (re-search-forward regexp nil t))
      (if (= (char-after (1+ (match-beginning 0))) ?b);;begin
	  (setq level (1+ level))
	(setq level (1- level))))
    (if (= level 0)
	(search-forward "}")
      (error "Can't locate end of current environment"))))

(defun LaTeX-find-matching-begin ()
  "Move point to the \\begin of the current environment"
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
	(level 1))
    (beginning-of-line 1)
    (if (looking-at (concat " *" (regexp-quote TeX-esc) "begin\\b"))
	(end-of-line 1))
    (while (and (> level 0) (re-search-backward regexp nil t))
      (if (= (char-after (1+ (match-beginning 0))) ?e);;end
	  (setq level (1+ level))
	(setq level (1- level))))
    (or (= level 0)
	(error "Can't locate beginning of current environment"))))

(defun LaTeX-mark-environment ()
  "Set mark to end of current environment and point to the matching begin
will not work properly if there are unbalanced begin-end pairs in
comments and verbatim environments"
  (interactive)
  (let ((cur (point)))
    (LaTeX-find-matching-end)
    (beginning-of-line 2)
    (set-mark (point))
    (goto-char cur)
    (LaTeX-find-matching-begin)))

(defun LaTeX-format-environment (justify)
  "Fill and indent current environment as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-mark-environment)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-format-region
     (region-beginning)
     (region-end)
     justify
     (concat " environment " (TeX-match-buffer 1)))))

(defun LaTeX-format-section (justify)
  "Fill and indent current logical section as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-mark-section)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-format-region
     (region-beginning)
     (region-end)
     justify
     (concat " section " (TeX-match-buffer 1)))))

(defun LaTeX-mark-section ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "\\(" (LaTeX-outline-regexp)
			      "\\|\\'\\)"))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "\\(" (LaTeX-outline-regexp)
			      "\\|\\`\\)")))

(defun LaTeX-format-buffer (justify)
  "Fill and indent current buffer as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-format-region
     (point-min)
     (point-max)
     justify
     (concat " buffer " (buffer-name)))))

(defvar LaTeX-indent-environment-list
  '(("verbatim" current-indentation)
    ("verbatim*" current-indentation))
    "Alist of environments with special indentation.
The second element in each entry is the function to calculate the
indentation level in columns.")

(defvar LaTeX-indent-environment-check t
  "*If non-nil, check for any special environments.")

(defvar LaTeX-left-comment-regexp "%%%"
  "*Regexp matching comments that should be placed on the left margin.")

(defvar LaTeX-right-comment-regexp "%[^%]"
  "*Regexp matching comments that should be placed to the right margin.")

(defvar LaTeX-ignore-comment-regexp nil
  "*Regexp matching comments that whose indentation should not be touched.")

(defun LaTeX-indent-calculate ()
  ;; Return the correct indentation of line of LaTeX source. (I hope...)
  (save-excursion
    (back-to-indentation)
    (cond ((looking-at (concat (regexp-quote TeX-esc)
			       "\\(begin\\|end\\){verbatim\\*?}"))
	   ;; \end{verbatim} must be flush left, otherwise an unwanted
	   ;; empty line appears in LaTeX's output.
	   0)
	  ((and LaTeX-left-comment-regexp
		(looking-at LaTeX-left-comment-regexp))
	   ;; Comments to the left margin.
	   0)
	  ((and LaTeX-right-comment-regexp
                (looking-at LaTeX-right-comment-regexp))
           ;; Comments to the right margin.
	   comment-column)
	  ((and LaTeX-ignore-comment-regexp
                (looking-at LaTeX-ignore-comment-regexp))
           ;; Comments best left alone.
	   (current-indentation))
	  ((and LaTeX-indent-environment-check
		;; Special environments.
		(let ((entry (assoc (LaTeX-current-environment)
				    LaTeX-indent-environment-list)))
		  (and entry (funcall (nth 1 entry))))))
	  ((looking-at (concat "\\("
			       (regexp-quote TeX-esc)
			       "end *"
			       (regexp-quote TeX-grop)
			       "\\|"
			       (regexp-quote TeX-esc)
			       "right\\W\\)"))
	   ;; Backindent after \end{ and \right.
	   (- (LaTeX-indent-calculate-last) LaTeX-indent-level))
	  ((looking-at (concat (regexp-quote TeX-esc) LaTeX-item-regexp))
	   ;; Items.
	   (+ (LaTeX-indent-calculate-last) LaTeX-item-indent))
	  (t (LaTeX-indent-calculate-last)))))

(defun LaTeX-indent-calculate-last ()
  "Return the correct indentation of a normal line of text.
The point is supposed to be at the beginning of the current line."
  (save-restriction
    (widen)
    (skip-chars-backward "\n\t ")
    (move-to-column (current-indentation))

    ;; Ignore comments.
    (while (and (looking-at "%") (not (bobp)))
      (skip-chars-backward "\n\t ")
      (if (not (bobp))
	  (move-to-column (current-indentation))))

    (cond ((bobp) 0)
	  ((looking-at (concat (regexp-quote TeX-esc) "begin{document}"))
	   ;; I dislike having all of the document indented...
	   (current-indentation))
	  ((looking-at (concat (regexp-quote TeX-esc) "begin"
			       (regexp-quote TeX-grop)
			       "verbatim\\*?"
			       (regexp-quote TeX-grcl)))
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc) "end"
			       (regexp-quote TeX-grop)
			       "verbatim\\*?"))
	   ;; If I see an \end{verbatim} in the previous line I skip
	   ;; back to the preceding \begin{verbatim}.
	   (save-excursion
	     (if (re-search-backward (concat (regexp-quote TeX-esc)
					     "begin *"
					     (regexp-quote TeX-grop)
					     "verbatim\\*?"
					     (regexp-quote TeX-grcl)) 0 t)
		 (LaTeX-indent-calculate-last)
	       0)))
	  (t (+ (TeX-brace-count-line)
		(cond ((looking-at (concat "\\("
					   (regexp-quote TeX-esc) "begin *"
					   (regexp-quote TeX-grop)
					   "\\|"
					   (regexp-quote TeX-esc)
					   "left\\W\\)"))
		       (+ (current-indentation) LaTeX-indent-level))
		      ((looking-at (concat (regexp-quote TeX-esc)
					   LaTeX-item-regexp))
		       (- (current-indentation) LaTeX-item-indent))
		      (t (current-indentation))))))))

;;; Keymap

(defvar LaTeX-mode-map
  (let ((map (copy-keymap TeX-mode-map)))
    
    ;; Standard
    (define-key map "\n"      'reindent-then-newline-and-indent)
    
    ;; From latex.el
    (define-key map "\t"      'LaTeX-indent-line)
    (define-key map "\eq"     'LaTeX-format-paragraph) ;*** Alias
    (define-key map "\eg"     'LaTeX-format-region) ;*** Alias
    (define-key map "\e\C-e"  'LaTeX-find-matching-end)
    (define-key map "\e\C-a"  'LaTeX-find-matching-begin)
    
    (define-key map "\C-c\C-q\C-p" 'LaTeX-format-paragraph)
    (define-key map "\C-c\C-q\C-r" 'LaTeX-format-region)
    (define-key map "\C-c\C-q\C-s" 'LaTeX-format-section)
    (define-key map "\C-c\C-q\C-e" 'LaTeX-format-environment)
    
    (define-key map "\C-c."    'LaTeX-mark-environment) ;*** Dubious
    (define-key map "\C-c*"    'LaTeX-mark-section) ;*** Dubious

    (define-key map "\C-c\C-e" 'LaTeX-environment)
    (define-key map "\C-c\n"   'LaTeX-insert-item)
    (or (key-binding "\e\r")
	(define-key map "\e\r"    'LaTeX-insert-item)) ;*** Alias
    (define-key map "\C-c]" 'LaTeX-close-environment)
    (define-key map "\C-c\C-s" 'LaTeX-section)

    ;; Outline commands...
    ;; We want to use the right prefix, if possible.
    (let ((outline (cond ((not (boundp 'outline-minor-mode-prefix))
			  (lookup-key map "\C-c"))
			 ((keymapp (lookup-key map outline-minor-mode-prefix))
			  (lookup-key map outline-minor-mode-prefix))
			 (t
			  (define-key map
			    outline-minor-mode-prefix (make-sparse-keymap))
			  (lookup-key map outline-minor-mode-prefix)))))
      (define-key outline "\C-z" 'LaTeX-hide-environment)
      (define-key outline "\C-x" 'LaTeX-show-environment))

    ;; From tex-math.el
    (define-key map "\C-c~"    'LaTeX-math-mode) ;*** Dubious
    
    map)
  "Keymap used in LaTeX-mode.")

(defvar LaTeX-environment-menu-name "Insert Environment  (C-c C-e)")

(defun LaTeX-environment-menu-entry (entry)
  ;; Create an entry for the environment menu.
  (vector (car entry) (list 'LaTeX-environment-menu (car entry)) t))

(defvar LaTeX-environment-modify-menu-name "Change Environment  (C-u C-c C-e)")

(defun LaTeX-environment-modify-menu-entry (entry)
  ;; Create an entry for the change environment menu.
  (vector (car entry) (list 'LaTeX-modify-environment (car entry)) t))

(defun LaTeX-section-enable-symbol (LEVEL)
  ;; Symbol used to enable section LEVEL in the menu bar.
  (intern (concat "LaTeX-section-" (int-to-string (nth 1 entry)) "-enable")))

(defun LaTeX-section-enable (entry)
  ;; Enable or disable section ENTRY from LaTeX-section-list.
  (let ((level (nth 1 entry)))
    (set (LaTeX-section-enable-symbol level)
	 (>= level LaTeX-largest-level))))

(defun LaTeX-section-menu (level)
  ;; Insert section from menu.
  (let ((LaTeX-section-hook (delq 'LaTeX-section-heading
				  (copy-sequence LaTeX-section-hook))))
    (LaTeX-section level)))

(defun LaTeX-section-menu-entry (entry)
  ;; Create an entry for the section menu.
  (let ((enable (LaTeX-section-enable-symbol (nth 1 entry))))
    (set enable t)
    (vector (car entry) (list 'LaTeX-section-menu (nth 1 entry)) enable)))

(defun LaTeX-section-menu-create ()
  ;; Create a menu over LaTeX sections.
  (append '("Section  (C-c C-s)")
	  (mapcar 'LaTeX-section-menu-entry LaTeX-section-list)))

(defvar LaTeX-menu-changed nil)
;; Need to update LaTeX menu.
(make-variable-buffer-local 'LaTeX-menu-changed)

(defun LaTeX-menu-update ()
  ;; Update entries on AUC TeX menu.
  (or (eq major-mode 'latex-mode)
      (null LaTeX-menu-changed)
      (progn
	(TeX-update-style)
	(setq LaTeX-menu-changed nil)
	(mapcar 'LaTeX-section-enable LaTeX-section-list)
	(easy-menu-change '("LaTeX") LaTeX-environment-modify-menu-name
			  (mapcar 'LaTeX-environment-modify-menu-entry
				  (LaTeX-environment-list)))
	(easy-menu-change '("LaTeX") LaTeX-environment-menu-name
			  (mapcar 'LaTeX-environment-menu-entry
				  (LaTeX-environment-list))))))

(add-hook 'activate-menubar-hook 'LaTeX-menu-update)

(easy-menu-define LaTeX-mode-menu
    LaTeX-mode-map
    "Menu used in LaTeX mode."
  (list "LaTeX"
	(list LaTeX-environment-menu-name)
	(list LaTeX-environment-modify-menu-name)
	(LaTeX-section-menu-create)
	["Macro..." TeX-insert-macro t]
	["Complete" TeX-complete-symbol t]
	["Item" LaTeX-insert-item t]
	(list "Insert Font"
	      ["Emphasize"  (TeX-font nil ?\C-e) "C-c C-f C-e"]
	      ["Bold"       (TeX-font nil ?\C-b) "C-c C-f C-b"]
	      ["Typewriter" (TeX-font nil ?\C-t) "C-c C-f C-t"]
	      ["Small Caps" (TeX-font nil ?\C-c) "C-c C-f C-c"]
	      ["Italic"     (TeX-font nil ?\C-i) "C-c C-f C-i"]
	      ["Slanted"    (TeX-font nil ?\C-s) "C-c C-f C-s"]
	      ["Roman"      (TeX-font nil ?\C-r) "C-c C-f C-r"])
	(list "Change Font"
	      ["Emphasize"  (TeX-font t ?\C-e) "C-u C-c C-f C-e"]
	      ["Bold"       (TeX-font t ?\C-b) "C-u C-c C-f C-b"]
	      ["Typewriter" (TeX-font t ?\C-t) "C-u C-c C-f C-t"]
	      ["Small Caps" (TeX-font t ?\C-c) "C-u C-c C-f C-c"]
	      ["Italic"     (TeX-font t ?\C-i) "C-u C-c C-f C-i"]
	      ["Slanted"    (TeX-font t ?\C-s) "C-u C-c C-f C-s"]
	      ["Roman"      (TeX-font t ?\C-r) "C-u C-c C-f C-r"])
	["Delete Font" (TeX-font t ?\C-d) "C-c C-f C-d"]
	"-"
	["Save Document" TeX-save-document t]
	(TeX-command-create-menu "Command on Master File  (C-c C-c)"
				 'TeX-command-master)
	(TeX-command-create-menu "Command on Buffer  (C-c C-b)"
				 'TeX-command-buffer)
	(TeX-command-create-menu "Command on Region (C-c C-r)"
				 'TeX-command-region)
	["Next Error" TeX-next-error t]
	(list "TeX Output"
	      ["Kill Job" TeX-kill-job t]
	      ["Toggle debug of boxes" TeX-toggle-debug-boxes t]
	      ["Switch to original file" TeX-home-buffer t]
	      ["Recenter Output Buffer" TeX-recenter-output-buffer t])
	"--"
	(list "Formatting and Marking"
	      ["Format Environment" LaTeX-format-environment t]
	      ["Format Paragraph" LaTeX-format-paragraph t]
	      ["Format Region" LaTeX-format-region t]
	      ["Format Section" LaTeX-format-section t]
	      ["Mark Environment" LaTeX-mark-environment t]
	      ["Mark Section" LaTeX-mark-section t]
	      ["Beginning of Environment" LaTeX-find-matching-begin t]
	      ["End of Environment" LaTeX-find-matching-end t]
	      ["Hide Environment" LaTeX-hide-environment t]
	      ["Show Environment" LaTeX-show-environment t])
	;; ["Uncomment" TeX-un-comment t]
	["Uncomment Region" TeX-un-comment-region t]
	;; ["Comment Paragraph" TeX-comment-paragraph t]
	["Comment Region" TeX-comment-region t]
	["Switch to Master file" TeX-home-buffer t]
	["Math Mode" LaTeX-math-mode t]
	["Documentation" TeX-goto-info-page t]
	["Submit bug report" TeX-submit-bug-report t]
	["Reset Buffer" TeX-normal-mode t]
	["Reset AUC TeX" (TeX-normal-mode t) "C-u C-c C-n"]))

;;; Mode

(defvar LaTeX-version "2"
  "LaTeX version.  Currently recognized is \"2\" and \"2e\".")

(defvar TeX-arg-cite-note-p nil
  "*If non-nil, ask for optional note in citations.")

(defvar TeX-arg-footnote-number-p nil
  "*If non-nil, ask for optional number in footnotes.")

(defvar TeX-arg-item-label-p nil
  "*If non-nil, always ask for optional label in items.
Otherwise, only ask in description environments.")

(defvar TeX-arg-right-insert-p t
  "*If non-nil, always insert automatically the corresponding
\\right if \\left is inserted.")

;;;###autoload
(defun latex-mode ()
  "Major mode for editing files of input for LaTeX.
See info under AUC TeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of LaTeX-mode-hook."
  (interactive)
  (LaTeX-common-initialization)
  (setq mode-name "LaTeX")
  (setq major-mode 'latex-mode)  
  (setq TeX-command-default "LaTeX")
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'LaTeX-mode-hook)

  ;; Defeat filladapt if auto-fill-mode is set in text-mode-hook.
  (and (boundp 'filladapt-function-table)
       (eq auto-fill-function 'do-auto-fill)
       (setq auto-fill-function
	     (cdr (assoc 'do-auto-fill filladapt-function-table)))))

(defvar LaTeX-header-end
  (concat (regexp-quote TeX-esc) "begin *" TeX-grop "document" TeX-grcl)
  "Default end of header marker for LaTeX documents.")

(defvar LaTeX-trailer-start
  (concat (regexp-quote TeX-esc) "end *" TeX-grop "document" TeX-grcl)
  "Default start of trailer marker for LaTeX documents.")

(defun LaTeX-common-initialization ()
  ;; Common initialization for LaTeX derived modes.
  (VirTeX-common-initialization)
  (set-syntax-table LaTeX-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'LaTeX-indent-line)
  (use-local-map LaTeX-mode-map)
  (easy-menu-add LaTeX-mode-menu LaTeX-mode-map)

  (or LaTeX-largest-level 
      (setq LaTeX-largest-level (LaTeX-section-level "section")))
  
  (setq TeX-header-end LaTeX-header-end
	TeX-trailer-start LaTeX-trailer-start)

  (require 'outline)
  (make-local-variable 'outline-level)
  (setq outline-level 'LaTeX-outline-level)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp (LaTeX-outline-regexp t))
  
  (make-local-variable 'TeX-auto-full-regexp-list)
  (setq TeX-auto-full-regexp-list 
	(append LaTeX-auto-regexp-list plain-TeX-auto-regexp-list))

  (setq paragraph-start
	(concat
	 "\\("
	 "^.*[^" (regexp-quote TeX-esc) "]%.*$\\|"
	 "^%.*$\\|"
	 "^[ \t]*$"
	 "\\|"
	 "^[ \t]*"
	 (regexp-quote TeX-esc)
	 "\\("
	 "\\[\\|\\]\\|"  ; display math delimitors
	 "begin\\|end\\|item\\|part\\|chapter\\|label\\|caption\\|"
	 "section\\|subsection\\|subsubsection\\|par\\|noindent\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix\\|" (regexp-quote TeX-esc)
	 "\\)"
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
	 "\\[\\|\\]\\|"  ; display math delimitors
	 "begin\\|end\\|part\\|chapter\\|label\\|caption\\|"
	 "section\\|subsection\\|subsubsection\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix\\|" (regexp-quote TeX-esc)
	 "\\)"
	 "\\)"))
  (setq selective-display t)

  (make-local-variable 'LaTeX-item-list)
  (setq LaTeX-item-list '(("description" . LaTeX-item-argument)
			  ("thebibliography" . LaTeX-item-bib)))

  (setq TeX-complete-list
	(append '(("\\\\cite\\[[^]\n\r\\%]*\\]{\\([^{}\n\r\\%,]*\\)"
		   1 LaTeX-bibitem-list "}")
		  ("\\\\cite{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
		  ("\\\\cite{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]\\)"
		   2 LaTeX-bibitem-list)
		  ("\\\\nocite{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
		  ("\\\\nocite{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]\\)"
		   2 LaTeX-bibitem-list)
		  ("\\\\ref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
		  ("\\\\pageref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
		  ("\\\\begin{\\([A-Za-z]*\\)" 1 LaTeX-environment-list "}")
		  ("\\\\end{\\([A-Za-z]*\\)" 1 LaTeX-environment-list "}")
		  ("\\\\renewcommand{\\\\\\([A-Za-z]*\\)"
		   1 LaTeX-symbol-list "}")
		  ("\\\\renewenvironment{\\([A-Za-z]*\\)"
		   1 LaTeX-environment-list "}"))
		TeX-complete-list))

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
     t [ (TeX-arg-eval progn (if (eq (save-excursion
				       (backward-char 2)
				       (preceding-char)) ?\])
				 ()
			       (TeX-arg-counter t "Within counter"))
		       "") ])
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
     TeX-arg-coordinate
     (TeX-arg-pair "X delta" "Y delta")
     "Number of copies"
     t)
   '("oval" TeX-arg-size [ TeX-arg-corner "Portion" ])
   '("put" TeX-arg-coordinate t)
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
   '("bibliography" TeX-arg-bibliography)
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
   '("left" TeX-arg-insert-braces)

   ;; These have no special support, but are included in case the
   ;; auto files are missing. 

   "LaTeX" "SLiTeX" "samepage" "newline" "smallskip" "medskip"
   "bigskip" "stretch" "nonumber" "centering" "raggedright"
   "raggedleft" "kill" "pushtabs" "poptabs" "protect" "arraystretch"
   "hline" "vline" "cline" "thinlines" "thicklines" "and" "makeindex"
   "makeglossary" "reversemarginpar" "normalmarginpar"
   "raggedbottom" "flushbottom" "sloppy" "fussy" "newpage"
   "clearpage" "cleardoublepage" "twocolumn" "onecolumn")

  (TeX-run-style-hooks "LATEX")

  (if (string-equal LaTeX-version "2")
      ()
    (TeX-add-symbols
     '("newcommand" TeX-arg-define-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("renewcommand" TeX-arg-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("usepackage" [ "Options" ] (TeX-arg-input-file "Package"))
     '("documentclass" TeX-arg-document))))

(provide 'latex)

;;; latex.el ends here
