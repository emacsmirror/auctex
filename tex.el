;;; tex.el --- Support for TeX documents.

;; Maintainer: Per Abrahamsen <auc-tex@iesd.auc.dk>
;; Version: $Id: tex.el,v 5.23 1994-08-09 00:00:13 amanda Exp $
;; Keywords: wp

;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;; Copyright (C) 1987 Lars Peter Fischer
;; Copyright (C) 1991 Kresten Krab Thorup
;; Copyright (C) 1993, 1994 Per Abrahamsen 
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

;;; Site Customization
;;
;; The following variables are likely to need to be changed for your
;; site.  It is suggested that you do this by *not* changing this
;; file, but instead copy those definitions you need to change to
;; `tex-site.el'. 

(defvar TeX-lisp-directory "/usr/local/lib/emacs/site-lisp/auctex/"
  "*The directory where the AUC TeX lisp files are located.")

;; Change this to point to the place where the TeX macros are stored
;; at yourt site.
(defvar TeX-macro-global
  '("/usr/local/lib/tex/inputs/" "/usr/local/lib/tex/generate/")
  "*Directories containing the sites TeX macro files and style files.

The directory names *must* end with a slash.")

;; How to print.

(defvar TeX-print-command "dvips %s -P%p"
  "*Command used to print a file. 

First %p is expanded to the printer name, then ordinary expansion is
performed as specified in TeX-expand-list.")

(defvar TeX-queue-command "lpq -P%p"
  "*Command used to show the status of a printer queue. 

First %p is expanded to the printer name, then ordinary expansion is
performed as specified in TeX-expand-list.")

;; This is the major configuration variable.  Most sites will only
;; need to change the second string in each entry, which is the name
;; of a command to send to the shell.  If you use other formatters
;; like AMSLaTeX or AMSTeX, you can add those to the list.  See
;; TeX-expand-list for a description of the % escapes

(defvar TeX-command-list
  ;; You may have to remove the single quotes around the command
  ;; arguments if you use DOS.
  (list (list "TeX" "tex '\\nonstopmode\\input %t'" 'TeX-run-TeX nil t)
	(list "TeX Interactive" "tex %t" 'TeX-run-interactive nil t)
	(list "LaTeX" "%l '\\nonstopmode\\input{%t}'" 'TeX-run-LaTeX nil t)
	(list "LaTeX Interactive" "%l %t" 'TeX-run-interactive nil t)
	(list "LaTeX2e" "latex2e '\\nonstopmode\\input{%t}'"
	      'TeX-run-LaTeX nil t)
	(if (or window-system (getenv "DISPLAY"))
	    (list "View" "%v " 'TeX-run-background t nil)
	  (list "View" "dvi2tty -q -w 132 %s " 'TeX-run-command t nil))
	(list "Print" "%p " 'TeX-run-command t nil)
	(list "Queue" "%q" 'TeX-run-background nil nil)
	(list "File" "dvips %d -o %f " 'TeX-run-command t nil)
	(list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil nil)
	(list "Index" "makeindex %s" 'TeX-run-command nil t)
	(list "Check" "lacheck %s" 'TeX-run-compile nil t)
	(list "Spell" "<ignored>" 'TeX-run-ispell nil nil)
	(list "Other" "" 'TeX-run-command t t)
	;; Not part of standard TeX.
	(list "Makeinfo" "makeinfo %t" 'TeX-run-compile nil t)
	(list "AmSTeX" "amstex '\\nonstopmode\\input %t'"
	      'TeX-run-TeX nil t))
  "*List of commands to execute on the current document.

Each element is a list, whose first element is the name of the command
as it will be presented to the user.  

The second element is the string handed to the shell after being
expanded. The expansion is done using the information found in
TeX-expand-list. 

The third element is the function which actually start the process.
Several such hooks has been defined:

TeX-run-command: Start up the process and show the output in a
separate buffer.  Check that there is not two commands running for the
same file.  Return the process object. 

TeX-run-format: As TeX-run-command, but assume the output is created
by a TeX macro package.  Return the process object. 

TeX-run-TeX: For TeX output.

TeX-run-LaTeX: For LaTeX output.

TeX-run-interactive: Run TeX or LaTeX interactively.

TeX-run-BibTeX: For BibTeX output.

TeX-run-compile: Use `compile' to run the process.  

TeX-run-shell: Use `shell-command' to run the process.

TeX-run-discard: Start the process in the background, discarding its
output.

TeX-run-background: Start the process in the background, show output
in other window.

TeX-run-dviout: Special hook for the Japanese dviout previewer for
PC-9801.

To create your own hook, define a function taking three arguments: The
name of the command, the command string, and the name of the file to
process.  It might be useful to use TeX-run-command in order to
create an asynchronous process.

If the fourth element is non-nil, the user will get a chance to
modify the expanded string.

If the fifth element is non-nil, the TeX-region file will be rebuild
before the command is started.")

;; You may want to change the default LaTeX version for your site.
(defvar LaTeX-version "2e"
  "Default LaTeX version.  Currently recognized is \"2\" and \"2e\".")

;; You may want special options to the view command depending on the
;; style options.  Only works if parsing is enabled.

(defvar LaTeX-command-style
  (if (string-equal LaTeX-version "2")
      ;; There is a lot of different LaTeX 2 based formats.
      '(("^foils$" "foiltex")
	("^ams" "amslatex")
	("^slides$" "slitex")
	("^plfonts\\|plhb$" "platex")
	("^latex2e$" "latex2e")
	("." "latex"))
    ;; They have all been combined in LaTeX 2e.
    '(("." "latex")))
  "*List of style options and LaTeX commands.

If the first element (a regular expresion) matches the name of one of
the style files, any occurrence of the string %l in a command in
TeX-command-list will be replaced with the second element.  The first
match is used, if no match is found the %l is replaced with the empty
string.")

;; Enter the names of the printers available at your site, or nil if
;; you only have one printer.

(defvar TeX-printer-list
  '(("Local" "dvips -f %s | lpr" "lpq")
    ("lw") ("ps"))
  "*List of available printers.

The first element of each entry is the printer name.

The second element is the command used to print to this
printer.  It defaults to the value of TeX-print-command.

The third element is the command used to examine the print queue for
this printer.  It defaults to the value of TeX-queue-command.

Any occurence of `%p' in the second or third element is expanded to
the printer name given in the first element, then ordinary expansion
is performed as specified in TeX-expand-list.")

;; The name of the most used printer.  

(defvar TeX-printer-default (or (getenv "PRINTER")
				(and TeX-printer-list
				     (car (car TeX-printer-list)))
				"lw")
  "*Default printer to use with TeX-command.")

;; You may want special options to the view command depending on the
;; style options.  Only works if parsing is enabled.

(defvar TeX-view-style '(("^a5$" "xdvi %d -paper a5")
			 ("^landscape$" "xdvi %d -paper a4r -s 4")
			 ;; The latest xdvi can show embedded postscript.
			 ;; If you don't have that, uncomment next line.
			 ;; ("^epsf$" "ghostview %f")
			 ("." "xdvi %d"))
  "*List of style options and view options.

If the first element (a regular expresion) matches the name of one of
the style files, any occurrence of the string %v in a command in
TeX-command-list will be replaced with the second element.  The first
match is used, if no match is found the %v is replaced with the empty
string.")

;; This is the list of expansion for the commands in
;; TeX-command-list.  Not likely to be changed, but you may e.g. want
;; to handle .ps files. 

(defvar TeX-expand-list 
  (list (list "%p" 'TeX-printer-query)	;%p must be the first entry
	(list "%q" (function (lambda ()
		     (TeX-printer-query TeX-queue-command 2))))
	(list "%v" 'TeX-style-check TeX-view-style)
	(list "%l" 'TeX-style-check LaTeX-command-style)
	(list "%s" 'file)
	(list "%t" 'file 't)
	(list "%d" 'file "dvi")
	(list "%f" 'file "ps"))
  "*List of expansion strings for TeX command names.

Each entry is a list with two or more elements.  The first element is
the string to be expanded.  The second element is the name of a
function returning the expanded string when called with the remaining
elements as arguments.  The special value `file' will be expanded to
the name of the file being processed, with an optional extension.")

;;; Import

(or (assoc TeX-lisp-directory (mapcar 'list load-path))	;No `member' yet.
    (setq load-path (cons TeX-lisp-directory load-path)))

(require 'auc-ver)
(require 'auc-menu)

(cond ((< (string-to-int emacs-version) 19)
       (require 'tex-18))
      ((string-match "Lucid" emacs-version)
       (require 'tex-lcd))
      (t
       (require 'tex-19)))

(defvar no-doc
  "This function is part of AUC TeX, but has not yet been loaded.
Full documentation will be available after autoloading the function."
  "Documentation for autoload functions.")

;; This hook will store bibitems when you save a BibTeX buffer.
(defvar bibtex-mode-hook nil)
(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)
(autoload 'BibTeX-auto-store "latex" no-doc t)

;; Bind latex-help globally. 
(autoload 'latex-help "ltx-help" no-doc t)
(define-key help-map "\C-l" 'latex-help)

(autoload 'LaTeX-math-mode "ltx-math" no-doc t)
(autoload 'japanese-plain-tex-mode "tex-jp" no-doc t)
(autoload 'japanese-latex-mode "tex-jp" no-doc t)
(autoload 'japanese-slitex-mode "tex-jp" no-doc t)
(autoload 'texinfo-mode "tex-info" no-doc t)
(autoload 'latex-mode "latex" no-doc t)

;;; Buffer

(defvar TeX-command-BibTeX "BibTeX"
  "*The name of the BibTeX entry in TeX-command-list.")
  (make-variable-buffer-local 'TeX-command-BibTeX)

(defvar TeX-command-Show "View"
  "*The default command to show (view or print) a TeX file.
Must be the car of an entry in TeX-command-list.")
  (make-variable-buffer-local 'TeX-command-Show)

(defvar TeX-command-Print "Print"
  "The name of the Print entry in TeX-command-Print.")

(defvar TeX-command-Queue "Queue"
  "The name of the Queue entry in TeX-command-Queue.")

(autoload 'TeX-region-create "tex-buf" no-doc nil)
(autoload 'TeX-save-document "tex-buf" no-doc t)
(autoload 'TeX-home-buffer "tex-buf" no-doc t)
(autoload 'TeX-command-region "tex-buf" no-doc t)
(autoload 'TeX-command-buffer "tex-buf" no-doc t)
(autoload 'TeX-command-master "tex-buf" no-doc t)
(autoload 'TeX-command "tex-buf" no-doc nil)
(autoload 'TeX-kill-job "tex-buf" no-doc t)
(autoload 'TeX-recenter-output-buffer "tex-buf" no-doc t)
(autoload 'TeX-next-error "tex-buf" no-doc t)
(autoload 'TeX-toggle-debug-boxes "tex-buf" no-doc t)
(autoload 'TeX-region-file "tex-buf" no-doc nil)

(defvar TeX-trailer-start nil
  "Regular expression delimiting start of trailer in a TeX file.")

 (make-variable-buffer-local 'TeX-trailer-start)

(defvar TeX-header-end nil
  "Regular expression delimiting end of header in a TeX file.")

 (make-variable-buffer-local 'TeX-header-end)

(defvar TeX-command-default nil
  "The default command for TeX-command in the current major mode.")

 (make-variable-buffer-local 'TeX-command-default)


;;; Master File

(defvar TeX-one-master "\\.tex$"
  "*Regular expression matching ordinary TeX files.

You should set this variable to match the name of all files, where
automatically adding a file variable with the name of the master file
is a good idea.  When AUC TeX add the name of the master file as a
file variable, it does not need to ask next time you edit the file.  

If you dislike AUC TeX automatically modifying your files, you can set
this variable to \"<none>\".")

(defun TeX-master-file (&optional extension)
  "Return the name of the master file for the current document.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value `t' means use `TeX-default-extension'.

Currently is will check for the presence of a ``Master:'' line in
the beginning of the file, but that feature will be phased out."
  (if (eq extension t)
      (setq extension TeX-default-extension))
  (let ((my-name (if (buffer-file-name)
                     (TeX-strip-extension nil (list TeX-default-extension) t)
                   "<none>")))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(cond
	 ;; Special value 't means it is own master (a free file).
	 ((equal TeX-master my-name) 
	  (setq TeX-master t))

	 ;; For files shared between many documents.
	 ((eq 'shared TeX-master)
	  (setq TeX-master
		(TeX-strip-extension
		 (read-file-name "Master file: (default this file) "
				 nil "///")
		 (list TeX-default-extension)
		 t))
	  (if (or (string-equal TeX-master "///")
		  (string-equal TeX-master ""))
	      (setq TeX-master t)))

	 ;; We might already know the name.
	 (TeX-master)

	 ;; Support the ``Master:'' line (under protest!)
	 ((re-search-forward
	   "^%% *[Mm]aster:?[ \t]*\\([^ \t\n]+\\)" 500 t)
	  (setq TeX-master
		(TeX-strip-extension (TeX-match-buffer 1)
				     (list TeX-default-extension)))
	  (if TeX-convert-master
	      (progn
		(beginning-of-line)
		(kill-line 1)
		(TeX-add-local-master))))

	 ;; Is this a master file?
	 ((re-search-forward TeX-header-end 10000 t)
	  (setq TeX-master my-name))

	 ;; Ask the user (but add it as a local variable).
	 (t
	  (setq TeX-master
		(TeX-strip-extension
		 (condition-case name
		     (read-file-name "Master file: (default this file) "
				     nil "<default>")
		   (quit "<quit>"))
		 (list TeX-default-extension)
		 t))
	  (cond ((string-equal TeX-master "<quit>")
		 (setq TeX-master t))
		((or (string-equal TeX-master "<default>")
		     (string-equal TeX-master ""))
		 (setq TeX-master t)
		 (TeX-add-local-master))
		(t
		 (TeX-add-local-master)))))))
  
    (let ((name (if (eq TeX-master t)
		    my-name
		  TeX-master)))
      
      (if (TeX-match-extension name)
      ;; If it already have an extension...
	  (if (equal extension TeX-default-extension)
	      ;; Use instead of the default extension
	      (setq extension nil)
	    ;; Otherwise drop it.
	    (setq name (TeX-strip-extension name))))

      (if extension
	  (concat name "." extension)
	name))))

(defvar TeX-master t
  "*The master file associated with the current buffer.
If the file being edited is actually included from another file, you
can tell AUC TeX the name of the master file by setting this variable.
If there are multiple levels of nesting, specify the top level file. 

If this variable is nil, AUC TeX will query you for the name.

If the variable is t, AUC TeX will assume the file is a master file
itself.

If the variable is 'shared, AUC TeX will query for the name, but not
change the file.  

It is suggested that you use the File Variables (see the info node in
the Emacs manual) to set this variable permanently for each file.")

 (make-variable-buffer-local 'TeX-master)

(defvar TeX-convert-master t
  "*If not nil, automatically convert ``Master:'' lines to file variables.
This will be done when AUC TeX first try to use the master file.")

(defun TeX-add-local-master ()
  "Add local variable for TeX-master."

  (if (and (buffer-file-name)
           (string-match TeX-one-master
                         (file-name-nondirectory (buffer-file-name)))
           (not buffer-read-only))
      (progn
        (goto-char (point-max))
        (if (re-search-backward (concat "^\\([^\n]+\\)Local " "Variables:")
                                (- (point-max) 3000) t)
            (let ((prefix (TeX-match-buffer 1)))
              (re-search-forward (regexp-quote (concat prefix
                                                        "End:")))
              (beginning-of-line 1)
              (insert prefix "TeX-master: " (prin1-to-string TeX-master) "\n"))
          (insert "\n% Local " "Variables: \n"
                  "% mode: " (substring (symbol-name major-mode) 0 -5)
		  "\n"
                  "% TeX-master: " (prin1-to-string TeX-master) "\n"
                  "% End: \n")))))

;;; Style Paths

(or (string-match "/\\'" TeX-lisp-directory)
    (setq TeX-lisp-directory (concat TeX-lisp-directory "/")))

(defvar TeX-auto-global (concat TeX-lisp-directory "auto/")
  "*Directory containing automatically generated information.
Must end with a slash.

For storing automatic extracted information about the TeX macros
shared by all users of a site.")  

(defvar TeX-style-global (concat TeX-lisp-directory "style/")
  "*Directory containing hand generated TeX information.
Must end with a slash.

These correspond to TeX macros shared by all users of a site.")

(defvar TeX-auto-local "auto/"
  "*Directory containing automatically generated TeX information.
Must end with a slash.

This correspond to TeX macros found in the current directory.")

(defvar TeX-style-local "style/"
  "*Directory containing hand generated TeX information.
Must end with a slash.

These correspond to TeX macros found in the current directory.")

(defun TeX-split-string (char string)
  "Returns a list of strings. given REGEXP the STRING is split into 
sections which in string was seperated by REGEXP.

Examples:

      (TeX-split-string \"\:\" \"abc:def:ghi\")
          -> (\"abc\" \"def\" \"ghi\")

      (TeX-split-string \" *\" \"dvips -Plw -p3 -c4 testfile.dvi\")

          -> (\"dvips\" \"-Plw\" \"-p3\" \"-c4\" \"testfile.dvi\")

If CHAR is nil, or \"\", an error will occur."

  (let ((regexp char)
        (start 0)
        (result '()))
    (while (string-match regexp string start)
      (let ((match (string-match regexp string start)))
        (setq result (cons (substring string start match) result))
        (setq start (match-end 0))))
    (setq result (cons (substring string start nil) result))
    (nreverse result)))

(defun TeX-parse-path (env)
  ;; Return a list if private TeX directories found in environment
  ;; variable ENV.  
  (let* ((value (getenv env))
	 (entries (and value (TeX-split-string ":" value)))
	 entry
	 answers) 
    (while entries
      (setq entry (car entries))
      (setq entries (cdr entries))
      (or (string-match "/$" entry)
	  (setq entry (concat entry "/")))
      (or (not (string-match "^/" entry))
	  (member entry TeX-macro-global)
	  (string-equal "/" entry)
	  (setq answers (cons entry answers))))
    answers))

(defvar TeX-macro-private (append (TeX-parse-path "TEXINPUTS")
				  (TeX-parse-path "BIBINPUTS"))
  "*Directories where you store your personal TeX macros.
Each must end with a slash.")

(defvar TeX-auto-private (mapcar (function (lambda (entry)
					     (concat entry TeX-auto-local)))
				 TeX-macro-private)
  "*List of directories containing automatically generated information.
Must end with a slash.

These correspond to the personal TeX macros.")

(if (stringp TeX-auto-private)		;Backward compatibility
    (setq TeX-auto-private (list TeX-auto-private)))

(defvar TeX-style-private (mapcar (function (lambda (entry)
					      (concat entry
						      TeX-style-local)))
				  TeX-macro-private)
  "*List of directories containing hand generated information.
Must end with a slash.

These correspond to the personal TeX macros.")

(if (stringp TeX-style-private)		;Backward compatibility
    (setq TeX-style-private (list TeX-style-private)))

(defvar TeX-style-path
  (let ((path))
    (mapcar (function (lambda (file) (if file (setq path (cons file path)))))
	    (append (list TeX-auto-global TeX-style-global)
		    TeX-auto-private TeX-style-private
		    (list TeX-auto-local TeX-style-local)))
    path)
  "*List of directories to search for AUC TeX style files.")

(defvar TeX-check-path (append (list "./") TeX-macro-private TeX-macro-global)
  "*Directory path to search for dependencies.

If nil, just check the current file.
Used when checking if any files have changed.")

;;; Style Files

(defvar TeX-style-hook-list nil
  "List of TeX style hooks currently loaded.

Each entry is a list where the first element is the name of the style,
and the remaining elements are hooks to be run when that style is
active.")

(defvar TeX-byte-compile t
  "*Not nil means try to byte compile auto files before loading.")

(defun TeX-load-style (style)
  "Search for and load each definition for style in TeX-style-path."
  (cond ((assoc style TeX-style-hook-list)) ; We already found it
	((string-match "\\`\\(.+/\\)\\([^/]*\\)\\'" style) ;Complex path
	 (let* ((dir (substring style (match-beginning 1) (match-end 1)))
		(style (substring style (match-beginning 2) (match-end 2)))
		(TeX-style-path (append (list (concat dir TeX-auto-local)
					      (concat dir TeX-style-local))
					TeX-style-path)))
	   (TeX-load-style style)))
	(t				;Relative path
	 ;; Insert empty list to mark the fact that we have searched.
	 (setq TeX-style-hook-list (cons (list style) TeX-style-hook-list))
	 ;; Now check each element of the path
	 (mapcar (function (lambda (name)
		    (TeX-load-style-file (if (string-match "/$" name)
					     (concat name style)
					   (concat name "/" style)))))
		 TeX-style-path))))

(defun TeX-load-style-file (file)
  ;; Load FILE checking for a lisp extensions.
  (let ((el (concat file ".el"))
	(elc (concat file ".elc")))
    (cond ((and (null TeX-byte-compile)
		(file-readable-p el))
	   (load-file el))
	  ((file-newer-than-file-p el elc)
	   (if (not (file-writable-p elc))
	       (load-file el)
	     (byte-compile-file el)
	     (load-file elc)))
	  ((file-readable-p elc)
	   (load-file elc))
	  ((file-readable-p el)
	   (load-file el)))))

(defun TeX-add-style-hook (style hook)
  "Give STYLE yet another HOOK to run."
  (let ((entry (assoc style TeX-style-hook-list)))
    (if (null entry)
        (setq TeX-style-hook-list (cons (list style hook) TeX-style-hook-list))
      (setcdr entry (cons hook (cdr entry))))))

(defun TeX-unload-style (style)
  "Forget that we once loaded STYLE."
  (cond ((null (assoc style TeX-style-hook-list)))
        ((equal (car (car TeX-style-hook-list)) style)
         (setq TeX-style-hook-list (cdr TeX-style-hook-list)))
        (t
         (let ((entry TeX-style-hook-list))
           (while (not (equal (car (car (cdr entry))) style))
             (setq entry (cdr entry)))
           (setcdr entry (cdr (cdr entry)))))))

(defvar TeX-virgin-style (if (and TeX-auto-global
				  (file-directory-p TeX-auto-global))
			     "virtex"
			   "NoVirtexSymbols")
  "Style all documents use.")

(defvar TeX-active-styles nil
  "List of styles currently active in the document.")

 (make-variable-buffer-local 'TeX-active-styles)

(defun TeX-run-style-hooks (&rest styles)
  "Run the TeX following style hooks."
  (mapcar (function
	   (lambda (style)
	     (if (TeX-member style TeX-active-styles 'string-equal) 
		 ()                   ;Avoid recursion.
	       (setq TeX-active-styles
		     (cons style TeX-active-styles))
	       (TeX-load-style style)
	       (if (string-match "\\`\\(.+/\\)\\([^/]*\\)\\'" style)
		   (setq style		; Complex path
			 (substring style (match-beginning 2) (match-end 2))))
	       (mapcar 'funcall
		       (cdr-safe (assoc style TeX-style-hook-list))))))
	  styles))

(defvar TeX-parse-self nil
  "*Parse file after loading it if no style hook is found for it.")

(defvar TeX-style-hook-applied-p nil
  "Nil, unless the style specific hooks have been applied.")
 (make-variable-buffer-local 'TeX-style-hook-applied-p)

(defun TeX-update-style (&optional force)
  "Run style specific hooks for the current document.

Only do this if it has not been done before, or if optional argument
FORCE is not nil."

  (if (or (eq TeX-auto-update 'BibTeX)	; Not a real TeX buffer
	  (and (not force) TeX-style-hook-applied-p))
      ()
    (setq TeX-style-hook-applied-p t)
    (message "Applying style hooks...")
    (apply 'TeX-run-style-hooks (list (TeX-strip-extension nil nil t)
				      (TeX-master-file)))
    (if (and TeX-parse-self
	     (null (cdr-safe (assoc (TeX-strip-extension nil nil t)
				    TeX-style-hook-list))))
	(TeX-auto-apply))
    
    (message "Applying style hooks... done")))

(defvar TeX-remove-style-hook nil
  "List of hooks to call when we remove the style specific information.")
 (make-variable-buffer-local 'TeX-remove-style-hook)

(defun TeX-remove-style ()
  "Remnove all style specific information."
  (setq TeX-style-hook-applied-p nil)
  (run-hooks 'TeX-remove-style-hooks)
  (setq TeX-active-styles (list TeX-virgin-style)))

(defun TeX-style-list ()
  "Return a list of all styles (subfils) use by the current document."
  (TeX-update-style)
  TeX-active-styles)

;;; Special Characters

(defvar TeX-esc "\\" "The TeX escape character.")
 (make-variable-buffer-local 'TeX-esc)

(defvar TeX-grop "{" "The TeX group opening character.")
 (make-variable-buffer-local 'TeX-grop)

(defvar TeX-grcl "}" "The TeX group closing character.")
 (make-variable-buffer-local 'TeX-grcl)

;;; Symbols

;; Must be before keymaps.

(defvar TeX-complete-word 'ispell-complete-word
  "*Function to call for completing non-macros in tex-mode.")

(defvar TeX-complete-list nil
  "List of ways to complete the preceding text.

Each entry is a list with the following elements:

0. Regexp matching the preceding text.
1. A number indicating the subgroup in the regexp containing the text.
2. A function returning an alist of possible completions.
3. Text to append after a succesful completion.

Or alternatively:

0. Regexp matching the preceding text.
1. Function to do the actual completion.")

(defun TeX-complete-symbol ()
  "Perform completion on TeX/LaTeX symbol preceding point."
  (interactive "*")
  (let ((list TeX-complete-list)
	entry)
    (while list
      (setq entry (car list)
	    list (cdr list))
      (if (TeX-looking-at-backward (car entry) 250)
	  (setq list nil)))
    (if (numberp (nth 1 entry))
	(let* ((sub (nth 1 entry))
	       (close (nth 3 entry))
	       (begin (match-beginning sub))
	       (end (match-end sub))
	       (pattern (TeX-match-buffer 0))
	       (symbol (buffer-substring begin end))
	       (list (funcall (nth 2 entry)))
	       (completion (try-completion symbol list)))
	  (cond ((eq completion t)
		 (and close
		      (not (looking-at (regexp-quote close)))
		      (insert close)))
		((null completion)
		 (error "Can't find completion for \"%s\"" pattern))
		((not (string-equal symbol completion))
		 (delete-region begin end)
		 (insert completion)
		 (and close
		      (eq (try-completion completion list) t)
		      (not (looking-at (regexp-quote close)))
		      (insert close)))
		(t
		 (message "Making completion list...")
		 (let ((list (all-completions symbol list nil)))
		   (with-output-to-temp-buffer "*Completions*"
		     (display-completion-list list)))
		 (message "Making completion list...done"))))
      (funcall (nth 1 entry)))))

(defvar TeX-default-macro "ref"
  "*The default macro when creating new ones with TeX-insert-macro.")

 (make-variable-buffer-local 'TeX-default-macro)

(defvar TeX-insert-braces t
  "*If non-nil, append a empty pair of braces after inserting a macro.")

(defun TeX-math-mode-p ()
  "Are we in TeX math mode?"
  ;; This should check for dollar signs, but thats to hard for now.
  (and (boundp 'LaTeX-math-mode) LaTeX-math-mode))

(defun TeX-insert-macro (symbol)
  "Insert TeX macro with completion.

AUC TeX knows of some macros, and may query for extra arguments."
  (interactive (list (completing-read (concat "Macro (default "
					      TeX-default-macro
					      "): " 
					      TeX-esc)
				      (TeX-symbol-list))))
  (cond ((string-equal symbol "")
	 (setq symbol TeX-default-macro))
	((interactive-p)
	 (setq TeX-default-macro symbol)))
  (TeX-parse-macro symbol (cdr-safe (assoc symbol (TeX-symbol-list)))))

(defvar TeX-electric-macro-map nil)

(if TeX-electric-macro-map
    ()
  (setq TeX-electric-macro-map (copy-keymap minibuffer-local-completion-map))
  (define-key TeX-electric-macro-map " " 'minibuffer-complete-and-exit))

(defun TeX-electric-macro ()
  "Insert TeX macro with completion.

AUC TeX knows of some macros, and may query for extra arguments.
Space will complete and exit."
  (interactive)
  (if (memq (preceding-char) '(?\\ ?.))
      (call-interactively 'self-insert-command)
    (let ((minibuffer-local-completion-map TeX-electric-macro-map))
      (call-interactively 'TeX-insert-macro))))

(defun TeX-parse-macro (symbol args)
  "How to parse TeX macros which takes one or more arguments."

  ;; First argument is the name of the macro.  

  ;; If called with no additional arguments, insert macro with point
  ;; inside braces.  Otherwise, each argument of this function should
  ;; match an argument to the TeX macro.  What is done depend on the
  ;; argument type.

  ;; string: Use the string as a prompt to prompt for the argument.

  ;; number: Insert that many braces, leave point inside the first.

  ;; nil: Insert empty braces.

  ;; t: Insert empty braces, leave point between the braces.

  ;; other symbols: Call the symbol as a function.  You can define
  ;; your own hook, or use one of the predefined argument hooks.  If
  ;; you add new hooks, you can assume that point is placed directly
  ;; after the previous argument, or after the macro name if this is
  ;; the first argument.  Please leave point located efter the
  ;; argument you are inserting.  If you want point to be located
  ;; somewhere else after all hooks have been processed, set the value
  ;; of `exit-mark'.  It will point nowhere, until the argument hook
  ;; set it.  By convention, these hook all start with `TeX-arg-'.

  ;; list: If the car is a string, insert it as a prompt and the next
  ;; element as initial input.  Otherwise, call the car of the list
  ;; with the remaining elements as arguments.

  ;; vector: Optional argument.  If it has more than one element,
  ;; parse it as a list, otherwise parse the only element as above.
  ;; Use square brackets instead of curly braces, and is not inserted
  ;; on empty user input.

  (insert TeX-esc symbol)
  (let ((exit-mark (make-marker))
	(position (point)))
    (TeX-parse-arguments args)
    (cond ((marker-position exit-mark)
	   (goto-char (marker-position exit-mark))
	   (set-marker exit-mark nil))
	  ((and TeX-insert-braces
		(equal position (point))
		(string-match "[a-zA-Z]+" symbol)
		(not (TeX-math-mode-p)))
	   (insert TeX-grop TeX-grcl)))))

(defun TeX-arg-string (optional &optional prompt input)
  "Prompt for a string."
  (TeX-argument-insert
   (read-string (TeX-argument-prompt optional prompt "Text") input)
   optional))

(defun TeX-parse-arguments (args)
  "Parse TeX macro arguments.

See TeX-parse-macro for details."
  (let ((last-optional-rejected nil))
    (while args
      (if (vectorp (car args))
	  (if last-optional-rejected
	      ()
	    (let ((< LaTeX-optop)
		  (> LaTeX-optcl))
	      (TeX-parse-argument t (if (equal (length (car args)) 1)
					(aref (car args) 0)
				      (append (car args) nil)))))
	(let ((< TeX-grop)
	      (> TeX-grcl))
	  (setq last-optional-rejected nil)
	  (TeX-parse-argument nil (car args))))
      (setq args (cdr args)))))

(defun TeX-parse-argument (optional arg)
  "Depending on OPTIONAL, insert TeX macro argument ARG in curly braces.
If OPTIONAL is set, only insert if there is anything to insert, and
then use scare brackets.

See TeX-parse-macro for details."
  
  (cond ((stringp arg)
	 (TeX-arg-string optional arg))
	((numberp arg)
	 (if (< arg 1)
	     ()
	   (TeX-parse-argument optional t)
	   (while (> arg 1)
	     (TeX-parse-argument optional nil)
	     (setq arg (- arg 1)))))
	((null arg)
	 (insert < >))
	((eq arg t)
	 (insert  < )
	 (set-marker exit-mark (point))
	 (insert >))
	((symbolp arg)
	 (funcall arg optional))
	((listp arg)
	 (let ((head (car arg))
	       (tail (cdr arg)))
	   (cond ((stringp head)
		  (apply 'TeX-arg-string optional arg))
		 ((symbolp head)
		  (apply head optional tail))
		 (t (error "Unknown list argument type %s"
			   (prin1-to-string head))))))
	(t (error "Unknown argument type %s" (prin1-to-string arg)))))

(defun TeX-argument-insert (name optional &optional prefix)
  "Insert NAME surrounded by curly braces.

If OPTIONAL, only insert it if not empty, and then use scuare brackets."
  (if (and optional (string-equal name ""))
      (setq last-optional-rejected t)
    (insert <)
    (if prefix
	(insert prefix))
    (if (and (string-equal name "")
	     (null (marker-position exit-mark)))
	(set-marker exit-mark (point))
      (insert name))
    (insert >)))

(defun TeX-argument-prompt (optional prompt default &optional complete)
  "Return a argument prompt.

If OPTIONAL is not nil then the prompt will start with ``(Optional) ''.

PROMPT will be used if not nil, otherwise use DEFAULT.

Unless optional argument COMPLETE is non-nil, ``: '' will be appended."
  (concat (if optional "(Optional) " "")
	  (if prompt prompt default)
	  (if complete "" ": ")))

;;; The Mode

(defvar TeX-format-list
  '(("LATEX" latex-mode 
     "\\\\\\(begin\\|section\\|chapter\\|documentstyle\\|documentclass\\)\\b")
    ("TEX" plain-tex-mode "."))
  "*List of format packages to consider when choosing a TeX mode.

A list with a entry for each format package available at the site.

Each entry is a list with three elements.

1. The name of the format package.
2. The name of the major mode.
3. A regexp typically matched in the beginning of the file.

When entering tex-mode, each regexp is tried in turn in order to find
when major mode to enter.")

(defvar TeX-default-mode 'latex-mode
  "*Mode to enter for a new file when it can't be determined whether
the file is plain TeX or LaTeX or what.")

(defvar TeX-force-default-mode nil
  "*If set to nil, try to infer the mode of the file from its content.")

;;;###autoload
(defun tex-mode ()
  "Major mode for editing files of input for TeX or LaTeX.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or TeX-force-default-mode is not set to nil, 
      TeX-default-mode is chosen 
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, latex-mode is selected.
   3) Otherwise, use plain-tex-mode "
  (interactive)

  (funcall (if (or (equal (buffer-size) 0)
                   TeX-force-default-mode)
               TeX-default-mode
             (save-excursion
               (goto-char (point-min))
               (let ((comment-start-skip ;Used by TeX-in-comment
		      (concat
		       "\\(\\(^\\|[^\\]\\)\\("
		       (regexp-quote TeX-esc)
		       (regexp-quote TeX-esc)
		       "\\)*\\)\\(%+ *\\)"))
		     (entry TeX-format-list)
                     answer)
                 (while (and entry (not answer))
                   (if (re-search-forward (nth 2 (car entry))
                                          10000 t)
		       (if (not (TeX-in-comment))
			   (setq answer (nth 1 (car entry))))
		     (setq entry (cdr entry))))
                 (if answer
                     answer
                   TeX-default-mode))))))

;;;###autoload
(defun plain-tex-mode ()
  "Major mode for editing files of input for plain TeX.
See info under AUC TeX for documentation.

Special commands:
\\{TeX-mode-map}
 
Entering plain-tex-mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of plain-TeX-mode-hook."
  (interactive)
  (plain-TeX-common-initialization)
  (setq mode-name "TeX")
  (setq major-mode 'plain-tex-mode)
  (setq TeX-command-default "TeX")
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'plain-TeX-mode-hook))

;;;###autoload
(defun ams-tex-mode ()
  "Major mode for editing files of input for AmS TeX.
See info under AUC TeX for documentation.

Special commands:
\\{TeX-mode-map}
 
Entering AmS-tex-mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of AmS-TeX-mode-hook."
  (interactive)
  (plain-TeX-common-initialization)
  (setq mode-name "AmS TeX")
  (setq major-mode 'ams-tex-mode)
  (setq TeX-command-default "AmSTeX")
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'plain-TeX-mode-hook))

(defun VirTeX-common-initialization ()
  ;; Initialize
  (kill-all-local-variables)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq indent-tabs-mode nil)
  (setq words-include-escapes t)

  ;; Ispell support
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex)
  (make-local-variable 'ispell-tex-p)
  (setq ispell-tex-p t)

  ;; Redefine some standard varaibles
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip
	(concat
	 "\\(\\(^\\|[^\\]\\)\\("
	 (regexp-quote TeX-esc)
	 (regexp-quote TeX-esc)
	 "\\)*\\)\\(%+ *\\)"))
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'TeX-comment-indent)
  (make-local-variable 'compile-command)
  (if (boundp 'compile-command)
      ()
    (setq compile-command "make"))
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes nil)

  ;; Symbol completion.
  (make-local-variable 'TeX-complete-list)
  (setq TeX-complete-list
	(list (list "\\\\\\([a-zA-Z]*\\)"
		    1 'TeX-symbol-list (if TeX-insert-braces "{}"))
	      (list "" TeX-complete-word)))
  
  ;; We want this to be early in the list, so we do not add it before
  ;; we enter TeX mode  the first time.
  (if (boundp 'local-write-file-hooks)
      (add-hook 'local-write-file-hooks 'TeX-safe-auto-write)
    (add-hook 'write-file-hooks 'TeX-safe-auto-write))
  (make-local-variable 'TeX-auto-update)
  (setq TeX-auto-update t))

(defun plain-TeX-common-initialization ()
  ;; Common initialization for plain TeX like modes.
  (VirTeX-common-initialization)
  (use-local-map plain-TeX-mode-map)
  (easy-menu-add plain-TeX-mode-menu plain-TeX-mode-map)
  (set-syntax-table TeX-mode-syntax-table)
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
	 "\\[\\|\\]"			; display math delimitors
	 "\\)"
	 "\\|"
	 "^[ \t]*\\$\\$"		; display math delimitor
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
	 "tableofcontents\\|appendix\\|" (regexp-quote TeX-esc)
	 "\\)"
	 "\\)"))
  (setq TeX-header-end (regexp-quote "%**end of header"))
  (setq TeX-trailer-start (regexp-quote (concat TeX-esc "bye")))
  (TeX-run-style-hooks "TEX"))

;;; Hilighting

(if (boundp 'hilit-patterns-alist)
    (let ((latex-patterns (cdr-safe (assq 'latex-mode hilit-patterns-alist)))
	  (plain-tex-patterns (cdr-safe (assq 'plain-tex-mode
					      hilit-patterns-alist))))
      (if (and latex-patterns plain-tex-patterns)
	  (setq hilit-patterns-alist
		(append (list (cons 'ams-tex-mode plain-tex-patterns))
			hilit-patterns-alist)))))

;;; Parsing

(defvar TeX-auto-parser '((styles TeX-auto-file TeX-run-style-hooks)))
;; Alist of parsed information.  
;; Each entry is a list with the following elements:
;; 
;; 0. Name of information type.
;; 1. Name of temporary variable used when parsing.
;; 2. Name of function to add information to add to #3.
;; 3. Name of variable holding buffer local information.
;; 4. Name of variable indicating that #3 has changed.


(defconst TeX-auto-parser-temporary 1)
(defconst TeX-auto-parser-add 2)
(defconst TeX-auto-parser-local 3)
(defconst TeX-auto-parser-change 4)

(defun TeX-auto-add-type (name prefix &optional plural)
  "Add information about name to the parser using PREFIX.

Optional third argument PLURAL is the plural form of TYPE.  
By default just add  a `s'.

This function create a set of variables and functions to maintain a
separate type of information in the parser."
  (let* ((names (or plural (concat name "s")))
	 (tmp (intern (concat prefix "-auto-" name)))
	 (add (intern (concat prefix "-add-" names)))
	 (local (intern (concat prefix "-" name "-list")))
	 (change (intern (concat prefix "-" name "-changed"))))
    (setq TeX-auto-parser
	  (cons (list name tmp add local change) TeX-auto-parser))
    (set local nil)
    (make-variable-buffer-local local)
    (set change nil)
    (make-variable-buffer-local change)
    (fset add (list 'lambda '(&rest entries)
		    (concat "Add information about " (upcase name)
			    " to the current buffer.")
		    (list 'TeX-auto-add-information name 'entries)))
    (fset local (list 'lambda nil
		      (concat "List of " names
			      " active in the current buffer.")
		      (list 'TeX-auto-list-information name)))
    (add-hook 'TeX-remove-style-hook
	      (list 'lambda nil (list 'setq (symbol-name local) nil)))))

(defun TeX-auto-add-information (name entries)
  ;; For NAME in `TeX-auto-parser' add ENTRIES.
  (let* ((entry (assoc name TeX-auto-parser))
	 (change (nth TeX-auto-parser-change entry))
	 (change-value (symbol-value change))
	 (local (nth TeX-auto-parser-local entry))
	 (local-value (symbol-value local)))
    (if change-value
	(set local (cons entries local-value))
      (set change t)
      (set local (list entries local-value)))))

(defun TeX-auto-list-information (name)
  ;; Return information in `TeX-auto-parser' about NAME.
  (TeX-update-style)
  (let* ((entry (assoc name TeX-auto-parser))
	 (change (nth TeX-auto-parser-change entry))
	 (change-value (symbol-value change))
	 (local (nth TeX-auto-parser-local entry)))
    (if (not change-value)
	()
      (set change nil)
      ;; Sort it
      (message "Sorting " name "...")
      (set local
	   (sort (mapcar 'TeX-listify (apply 'append (symbol-value local)))
		 'TeX-car-string-lessp))
      ;; Make it unique
      (message "Removing duplicates...") 
      (let ((entry (symbol-value local)))
	(while (and entry (cdr entry))
	  (let ((this (car entry))
		(next (car (cdr entry))))
	    (if (not (string-equal (car this) (car next)))
		(setq entry (cdr entry))
	      ;; We have to equal symbols.  Use the one with
	      ;; most arguments.
	      (if (> (length next) (length this))
		  (setcdr this (cdr next)))
	      (setcdr entry (cdr (cdr entry)))))))
      (message "Removing duplicates... done"))
    (symbol-value local)))

(TeX-auto-add-type "symbol" "TeX")

(defvar TeX-auto-apply-hook nil
  "Hook run when a buffer is parsed and the information is applied.")

(defun TeX-auto-apply ()
  ;; Parse and apply TeX information in the current buffer.
  (TeX-auto-parse)
  (run-hooks 'TeX-auto-apply-hook)
  (mapcar 'TeX-auto-apply-entry TeX-auto-parser))

(defun TeX-auto-apply-entry (entry)
  ;; Apply the information in an entry in `TeX-auto-parser'.
  (let ((value (symbol-value (nth TeX-auto-parser-temporary entry)))
	(add (nth TeX-auto-parser-add entry)))
    (if value (apply add value))))

(defun TeX-safe-auto-write ()
  ;; Call TeX-auto-write safely
  (condition-case name
      (and (boundp 'TeX-auto-update)
	   TeX-auto-update
	   (TeX-auto-write))
    (error nil))
  ;; Continue with the other write file hooks.
  nil)

(defvar TeX-auto-save nil
  "*Automatically save style information when saving the buffer.")

(defvar TeX-auto-untabify t
  "*Automatically untabify when saving the buffer.")

(defun TeX-auto-write ()
  ;; Save all relevant TeX information from the current buffer.
  (if TeX-auto-untabify
      (untabify (point-min) (point-max)))
  (if (and TeX-auto-save TeX-auto-local)
      (let* ((file (concat TeX-auto-local
			   (if (string-match "/$" TeX-auto-local) "" "/")
			   (TeX-strip-extension nil TeX-all-extensions t)
			   ".el"))
	     (dir (file-name-directory file)))
	;; Create auto directory if possible.
	(if (not (file-exists-p dir))
	    (condition-case name
		(make-directory (substring dir 0 -1))
	      (error nil)))
	(if (file-writable-p file)
	    (save-excursion
	      (TeX-update-style)
	      (TeX-auto-store file))
	  (message "Can't write style information.")))))

(defvar TeX-macro-default (car-safe TeX-macro-private)
  "*Default directory to search for TeX macros.")

(defvar TeX-auto-default (car-safe TeX-auto-private)
  "*Default directory to place automatically generated TeX information.")

;;;###autoload
(defun TeX-auto-generate (tex auto)
  "Generate style file for TEX and store it in AUTO.  
If TEX is a directory, generate style files for all files in the directory."
  (interactive (list (setq TeX-macro-default
                           (expand-file-name (read-file-name
                                              "TeX file or directory: "
                                              TeX-macro-default
                                              TeX-macro-default 'confirm)))
                     (setq TeX-auto-default
                           (expand-file-name (read-file-name
                                              "AUTO lisp directory: "
                                              TeX-auto-default
                                              TeX-auto-default 'confirm)))))
  (cond ((not (file-readable-p tex)))
	((string-match TeX-ignore-file tex))
        ((file-directory-p tex)
         (let ((files (directory-files tex))
               (default-directory (concat (if (string-match "^/" tex)
                                              ""
                                            default-directory)
                                          (if (string-match "/$" tex)
                                              tex
                                            (concat tex "/")))))
           (mapcar (function (lambda (file)
		      (if (or TeX-file-recurse
			      (not (file-directory-p file)))
			  (TeX-auto-generate file auto))))
                   files)))
        ((TeX-match-extension tex (append TeX-file-extensions
					  BibTeX-file-extensions))
         (save-excursion
           (set-buffer (find-file-noselect tex))
           (message "Parsing %s..." tex)
           (TeX-auto-store (concat auto
                                   (if (string-match "/$" auto) "" "/")
                                   (TeX-strip-extension tex
							TeX-all-extensions
							t)
                                   ".el"))
           (kill-buffer (current-buffer))
           (message "Parsing %s... done" tex)))))

;;;###autoload
(defun TeX-auto-generate-global ()
  "Create global auto directory for global TeX macro definitions."
  (interactive)
  (make-directory (if (string-match "/$" TeX-auto-global)
		      (substring TeX-auto-global 0 -1)
		    TeX-auto-global))
  (mapcar (function (lambda (macro) (TeX-auto-generate macro TeX-auto-global)))
          TeX-macro-global)
  (byte-recompile-directory TeX-auto-global 0))

(defun TeX-auto-store (file)
  ;; Extract information for auc tex from current buffer and store it in FILE.
  (TeX-auto-parse)
  
  (if (member nil (mapcar 'TeX-auto-entry-clear-p TeX-auto-parser))
      (let ((style (TeX-strip-extension nil TeX-all-extensions t)))
        (TeX-unload-style style)
	(save-excursion
	  (set-buffer (find-file-noselect file))
	  (erase-buffer)
	  (insert "(TeX-add-style-hook \"" style "\"\n"
		  " (function\n"
		  "  (lambda ()")
	  (mapcar 'TeX-auto-insert TeX-auto-parser)
	  (insert ")))\n\n")
	  (save-buffer 0)
	  (kill-buffer (current-buffer))))
    (if (file-exists-p (concat file "c"))
	(delete-file (concat file "c")))
    (if (file-exists-p file)
	(delete-file file))))

(defun TeX-auto-entry-clear-p (entry)
  ;; Check if the temporary for `TeX-auto-parser' entry ENTRY is clear.
  (null (symbol-value (nth TeX-auto-parser-temporary entry))))

(defun TeX-auto-insert (entry)
  ;; Insert code to initialize ENTRY from `TeX-auto-parser'.
  (let ((name (symbol-name (nth TeX-auto-parser-add entry)))
	(list (symbol-value (nth TeX-auto-parser-temporary entry))))
    (if (null list)
	()
      (insert "\n    (" name)
      (while list
	(newline-and-indent)
	(if (stringp (car list))
	    (insert (prin1-to-string (car list)))
	  (insert "'" (prin1-to-string (car list))))
	(setq list (cdr list)))
      (insert ")")
      (if (> (current-column) fill-column)
	  (do-auto-fill)))))

(defvar TeX-auto-ignore
  '("csname" "filedate" "fileversion" "docdate" "next" "labelitemi"
    "labelitemii" "labelitemiii" "labelitemiv" "labelitemv"
    "labelenumi" "labelenumii" "labelenumiii" "labelenumiv"
    "labelenumv" "theenumi" "theenumii" "theenumiii" "theenumiv"
    "theenumv" "document" "par" "do" "expandafter")
  "List of symbols to ignore when scanning a TeX style file.")

(defun TeX-auto-add-regexp (regexp)
  "Add REGEXP to TeX-auto-regexp-list if not already a member."
  (if (symbolp TeX-auto-regexp-list)
      (setq TeX-auto-regexp-list (symbol-value TeX-auto-regexp-list)))
  (or (memq regexp TeX-auto-regexp-list)
      (setq TeX-auto-regexp-list (cons regexp TeX-auto-regexp-list))))

(defvar TeX-auto-empty-regexp-list
  '(("<IMPOSSIBLE>\\(\\'\\`\\)" 1 ignore))
  "List of regular expressions guaranteed to match nothing.")

(defvar plain-TeX-auto-regexp-list
  '(("\\\\def\\\\\\([a-zA-Z]+\\)[^a-zA-Z@]" 1 TeX-auto-symbol-check)
    ("\\\\let\\\\\\([a-zA-Z]+\\)[^a-zA-Z@]" 1 TeX-auto-symbol-check)
    ("\\\\font\\\\\\([a-zA-Z]+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\chardef\\\\\\([a-zA-Z]+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\new\\(count|dimen|muskip|skip\\)\\\\\\([a-z]+\\)[^a-zA-Z@]"
     2 TeX-auto-symbol)
    ("\\\\newfont{?\\\\\\([a-zA-Z]+\\)}?" 1 TeX-auto-symbol)
    ("\\\\typein\\[\\\\\\([a-zA-Z]+\\)\\]" 1 TeX-auto-symbol)
    ("\\\\input +\\(\\.*[^#%\\\\\\.\n\r]+\\)\\(\\.[^#%\\\\\\.\n\r]+\\)?"
     1 TeX-auto-file)
    ("\\\\mathchardef\\\\\\([a-zA-Z]+\\)[^a-zA-Z@]" 1 TeX-auto-symbol))
  "List of regular expression matching common LaTeX macro definitions.")

(defvar TeX-auto-full-regexp-list plain-TeX-auto-regexp-list
  "Full list of regular expression matching TeX macro definitions.")

(defvar TeX-auto-prepare-hook nil
  "List of hooks to be called before parsing a TeX file.")

(defvar TeX-auto-cleanup-hook nil
  "List of hooks to be called after partsing a TeX file.")

(defvar TeX-auto-parse-length 999999
  "*Maximal length of TeX file that will be parsed.")
  (make-variable-buffer-local 'TeX-auto-parse-length)

(defun TeX-auto-parse ()
  "Parse TeX information in current buffer.

Call the functions in TeX-auto-prepare-hook before parsing, and the
functions in TeX-auto-cleanup-hook after parsing."

  (let ((case-fold-search nil)
	(regexp-list (if (symbolp TeX-auto-regexp-list)
			 (symbol-value TeX-auto-regexp-list)
		       TeX-auto-regexp-list)))

    (mapcar 'TeX-auto-clear-entry TeX-auto-parser)
    (run-hooks 'TeX-auto-prepare-hook)
    
    ;; Parse
    (save-excursion
      (goto-char (min (point-max) TeX-auto-parse-length))
      ;; Extract the information.
      (let ((regexp (concat "\\("
			    (mapconcat 'car regexp-list "\\)\\|\\(")
			    "\\)")))
	(while (re-search-backward regexp nil t)
	  (if (TeX-in-comment)
	      ()
	    (let* ((entry (TeX-member nil regexp-list
				      (function (lambda (a b)
						  (looking-at (nth 0 b))))))
		   (symbol (nth 2 entry))
		   (match (nth 1 entry)))
	      (if (fboundp symbol)
		  (funcall symbol match)
		(set symbol (cons (if (listp match)
				      (mapcar 'TeX-match-buffer match)
				    (TeX-match-buffer match))
				  (symbol-value symbol)))))))))
    
    ;; Cleanup ignored symbols.
    
    ;; NOTE: This is O(N M) where it could be O(N log N + M log M) if we 
    ;; sorted the lists first.
    (while (member (car TeX-auto-symbol) TeX-auto-ignore)
      (setq TeX-auto-symbol (cdr TeX-auto-symbol)))
    (let ((list TeX-auto-symbol))
      (while (and list (cdr list))
	(if (member (car (cdr list)) TeX-auto-ignore)
	    (setcdr list (cdr (cdr list)))
	  (setq list (cdr list)))))
    
    (run-hooks 'TeX-auto-cleanup-hook)))

(defun TeX-auto-clear-entry (entry)
  ;; Set the temporary variable in ENTRY to nil.
  (set (nth TeX-auto-parser-temporary entry) nil))

(defvar LaTeX-auto-end-symbol nil)

(defun TeX-auto-symbol-check (match)
  "Add MATCH to TeX-auto-symbols.
Check for potential LaTeX environments."
  (let ((symbol (if (listp match)
                    (mapcar 'TeX-match-buffer match)
                  (TeX-match-buffer match))))
    (if (and (stringp symbol)
             (string-match "^end\\(.+\\)$" symbol))
        (setq LaTeX-auto-end-symbol
              (cons (substring symbol (match-beginning 1) (match-end 1))
                    LaTeX-auto-end-symbol))
      (setq TeX-auto-symbol (cons symbol TeX-auto-symbol)))))

;;; Utilities
;;
;; Some of these functions has little to do with TeX, but nonetheless we
;; should use the "TeX-" prefix to avoid name clashes.

(defvar TeX-auto-regexp-list 'TeX-auto-full-regexp-list
  "*List of regular expresions used for parsing the current file.")
  (make-variable-buffer-local 'TeX-auto-regexp-list)

(defvar TeX-file-extensions '("tex" "sty" "ltx" "texi")
  "*File extensions used by manually generated TeX files.")

(defvar TeX-all-extensions '("[^.\n]+")
  "All possible file extensions.")

(defvar TeX-default-extension "tex"
  "*Default extension for TeX files.")

  (make-variable-buffer-local 'TeX-default-extension)

(defvar BibTeX-file-extensions '("bib")
  "Valid file extensions for BibTeX files.")

(defvar BibTeX-style-extensions '("bst")
  "Valid file extensions for BibTeX styles.")

(defvar TeX-ignore-file "\\(^\\|/\\)\\(\\.\\|\\.\\.\\|RCS\\|SCCS\\|CVS\\)$"
  "*Regular expression matching file names to ignore.

These files or directories will not be considered when searching for
TeX files in a directory.")

(defvar TeX-file-recurse t
  "*If not nil, search TeX directories recursivly.")

(defun TeX-match-extension (file &optional extensions)
  "Return non-nil if FILE has an one of EXTENSIONS.

If EXTENSIONS is not specified or nil, the value of
TeX-file-extensions is used instead."

  (if (null extensions)
      (setq extensions TeX-file-extensions))

  (let ((regexp (concat "\\.\\("
                        (mapconcat 'identity extensions "\\|")
                        "\\)$")))
    (string-match regexp file)))

(defun TeX-strip-extension (&optional string extensions nodir nostrip)
  "Return STRING without any trailing extension in EXTENSIONS.
If NODIR is set, also remove directory part of STRING. 
If NOSTRIP is set, do not remove extension after all.
STRING defaults to the name of the current buffer.
EXTENSIONS defaults to TeX-file-extensions."
  
  (if (null string)
      (setq string (or (buffer-file-name) "<none>")))
  
  (if (null extensions)
      (setq extensions TeX-file-extensions))
  
  (let ((strip (if (and (not nostrip)
                        (TeX-match-extension string extensions))
                   (substring string 0 (match-beginning 0))
                 string)))
    (if nodir
        (file-name-nondirectory strip)
      strip)))

(defun TeX-search-files (&optional directories extensions nodir strip)
  "Return a list of all reachable files in DIRECTORIES ending with EXTENSIONS.
If optional argument NODIR is set, remove directory part.
If optional argument STRIP is set, remove file extension.
If optional argument DIRECTORIES is set, search in those directories. 
Otherwise, search in all TeX macro directories.
If optional argument EXTENSIONS is not set, use TeX-file-extensions"

  (if (null extensions)
      (setq extensions TeX-file-extensions))
  
  (if (null directories)
      (setq directories
	    (cons "./" (append TeX-macro-private TeX-macro-global))))
  
  (let (match)
    
    (while directories
      (let* ((directory (car directories))
             (content (and directory
			   (file-readable-p directory)
			   (file-directory-p directory)
			   (directory-files directory))))
        
        (setq directories (cdr directories))
	
        (while content
          (let ((file (concat directory (car content))))
	    
            (setq content (cdr content))
            (cond ((string-match TeX-ignore-file file))
		  ((not (file-readable-p file)))
                  ((file-directory-p file)
		   (if TeX-file-recurse
		       (setq directories
			     (cons (concat file "/") directories))))
                  ((TeX-match-extension file extensions)
                   (setq match (cons (TeX-strip-extension file
							  extensions
							  nodir
							  (not strip))
                                     match))))))))
    
    match))

(defun TeX-car-string-lessp (a b)
  (string-lessp (car a) (car b)))

(defun TeX-listify (a)
  (if (listp a) a (list a)))

(defun TeX-member (elt list how)
  "Returns the member ELT in LIST.  Comparison done with HOW.

Return nil if ELT is not a member of LIST."
  (while (and list (not (funcall how elt (car list))))
    (setq list (cdr list)))
  (car-safe list))

(defun TeX-assoc (elem list)
  "Like assoc, except case incentive."
  (let ((case-fold-search t))
    (TeX-member elem list
		(function (lambda (a b)
		  (string-match (concat "^" (regexp-quote a) "$")
				(car b)))))))

(defun TeX-match-buffer (n)
  "Return the substring corresponding to the N'th match.

See match-data for details."
  (if (match-beginning n)
      (buffer-substring (match-beginning n) (match-end n))
    ""))

(defun TeX-function-p (arg)
  "Return non-nil if ARG is collable as a function."
  (or (and (fboundp 'byte-code-function-p)
	   (byte-code-function-p arg))
      (and (listp arg)
	   (eq (car arg) 'lambda))
      (and (symbolp arg)
	   (fboundp arg))))

(defun TeX-looking-at-backward (regexp &optional limit)
  ;; Return non-nil if the text before point matches REGEXP.
  ;; Optional second argument LIMIT gives a max number of characters
  ;; to look backward for.
  (let ((pos (point)))
    (save-excursion
      (and (re-search-backward regexp
			       (if limit (max (point-min) (- (point) limit)))
			       t)
	   (eq (match-end 0) pos)))))

;;; Syntax Table

(defvar TeX-mode-syntax-table (make-syntax-table)
  "Syntax table used while in TeX mode.")

 (make-variable-buffer-local 'TeX-mode-syntax-table)

(progn ; Define TeX-mode-syntax-table.
  (modify-syntax-entry (string-to-char TeX-esc)
		           "\\" TeX-mode-syntax-table)
  (modify-syntax-entry ?\f ">"  TeX-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  TeX-mode-syntax-table)
  (modify-syntax-entry (string-to-char TeX-grop)
		           (concat "(" TeX-grcl)
		                TeX-mode-syntax-table)  
  (modify-syntax-entry (string-to-char TeX-grcl)
		           (concat ")" TeX-grop)
			        TeX-mode-syntax-table)  
  (modify-syntax-entry ?%  "<"  TeX-mode-syntax-table)
  (modify-syntax-entry ?\" "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?&  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?_  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?@  "_"  TeX-mode-syntax-table)
  (modify-syntax-entry ?~  " "  TeX-mode-syntax-table)
  (modify-syntax-entry ?'  "w"  TeX-mode-syntax-table))

;;; Keymap

(defvar TeX-electric-escape nil
  "If this is non-nil when AUC TeX is loaded, the TeX escape
character ``\\'' will be bound to `TeX-electric-macro'.")

(defvar TeX-mode-map nil
  "Keymap for common TeX and LaTeX commands.")

(if TeX-mode-map 
    ()
  (setq TeX-mode-map (make-sparse-keymap))

  ;; Standard
  (define-key TeX-mode-map "\177"     'backward-delete-char-untabify)
  (define-key TeX-mode-map "\C-c}"    'up-list)
  (define-key TeX-mode-map "\C-c#"    'TeX-normal-mode)
  (define-key TeX-mode-map "\C-c\C-n" 'TeX-normal-mode)
  (define-key TeX-mode-map "\C-c?"    'describe-mode)
  (define-key TeX-mode-map "\C-c\C-i" 'TeX-goto-info-page)

  ;; From tex.el
  (define-key TeX-mode-map "\""       'TeX-insert-quote)
  (define-key TeX-mode-map "$"        'TeX-insert-dollar)
  (define-key TeX-mode-map "."        'TeX-insert-punctuation)
  (define-key TeX-mode-map ","        'TeX-insert-punctuation)
  (define-key TeX-mode-map "\C-c{"    'TeX-insert-braces)
  (define-key TeX-mode-map "\C-c\C-f" 'TeX-font)
  (define-key TeX-mode-map "\C-c\C-m" 'TeX-insert-macro)
  (if TeX-electric-escape
      (define-key TeX-mode-map "\\" 'TeX-electric-macro))
  (define-key TeX-mode-map "\e\t"   'TeX-complete-symbol) ;*** Emacs 19 way
  
  (define-key TeX-mode-map "\C-c;"    'TeX-comment-region)
  (define-key TeX-mode-map "\C-c%"    'TeX-comment-paragraph)

  (define-key TeX-mode-map "\C-c'"    'TeX-comment-paragraph) ;*** Old way
  (define-key TeX-mode-map "\C-c:"    'TeX-un-comment-region) ;*** Old way
  (define-key TeX-mode-map "\C-c\""   'TeX-un-comment) ;*** Old way

  ;; From tex-buf.el
  (define-key TeX-mode-map "\C-c\C-d" 'TeX-save-document)
  (define-key TeX-mode-map "\C-c\C-r" 'TeX-command-region)
  (define-key TeX-mode-map "\C-c\C-b" 'TeX-command-buffer)
  (define-key TeX-mode-map "\C-c\C-c" 'TeX-command-master)
  (define-key TeX-mode-map "\C-c\C-k" 'TeX-kill-job)
  (define-key TeX-mode-map "\C-c\C-l" 'TeX-recenter-output-buffer)
  (define-key TeX-mode-map "\C-c^" 'TeX-home-buffer)
  (define-key TeX-mode-map "\C-c`"    'TeX-next-error)
  (define-key TeX-mode-map "\C-c\C-w" 'TeX-toggle-debug-boxes))

(defvar plain-TeX-mode-map (copy-keymap TeX-mode-map)
  "Keymap used in plain TeX mode.")

(defvar TeX-command-force nil)
;; If non-nil, TeX-command-query will return the value of this
;; variable instead of quering the user. 

(defun TeX-command-menu (name file)
  ;; Execute TeX-command-list NAME on FILE from a menu.
  (let ((TeX-command-force name))
    (funcall file)))

(defun TeX-command-menu-print (printer command name file)
  ;; On PRINTER print FILE from a menu.
  (let ((TeX-printer-default printer)
	(TeX-printer-list nil)
	(TeX-print-command command))
    (TeX-command-menu name file)))

(defun TeX-command-menu-printer-entry (entry)
  ;; Return TeX-printer-list ENTRY as a menu item.
  (vector (nth 0 entry)
	  (list 'TeX-command-menu-print
		(nth 0 entry)
		(or (nth lookup entry) command)
		name
		(list 'quote file))
	  t))

(defun TeX-command-menu-entry (entry)
  ;; Return TeX-command-list ENTRY as a menu item.
  (let ((name (car entry)))
    (cond ((and (string-equal name TeX-command-Print)
		TeX-printer-list)
	   (let ((command TeX-print-command)
		 (lookup 1))
	     (append (list TeX-command-Print)
		     (mapcar 'TeX-command-menu-printer-entry
			     TeX-printer-list))))
	  ((and (string-equal name TeX-command-Queue)
		TeX-printer-list)
	   (let ((command TeX-queue-command)
		 (lookup 2))
	     (append (list TeX-command-Queue)
		     (mapcar 'TeX-command-menu-printer-entry
			     TeX-printer-list))))
	  (t
	   (vector name (list 'TeX-command-menu name (list 'quote file)) t)))))

(defun TeX-command-create-menu (name file)
  ;; Create menu NAME for each entry TeX-command-list for FILE.
  (append (list name)
	  (mapcar 'TeX-command-menu-entry TeX-command-list)))

(easy-menu-define plain-TeX-mode-menu
    plain-TeX-mode-map
    "Menu used in plain TeX mode."
  (list "TeX"
	["Macro..." TeX-insert-macro t]
	["Complete" TeX-complete-symbol t]
	["Save Document" TeX-save-document t]
	(TeX-command-create-menu "Command on Master File  (C-c C-c)"
				 'TeX-command-master)
	(TeX-command-create-menu "Command on Buffer  (C-c C-b)"
				 'TeX-command-buffer)
	(TeX-command-create-menu "Command on Region  (C-c C-r)"
				 'TeX-command-region)
	["Next Error" TeX-next-error t]
	["Kill Job" TeX-kill-job t]
	["Toggle debug of boxes" TeX-toggle-debug-boxes t]
	["Switch to original file" TeX-home-buffer t]
	["Recenter Output Buffer" TeX-recenter-output-buffer t]
	;; ["Uncomment" TeX-un-comment t]
	["Uncomment Region" TeX-un-comment-region t]
	;; ["Comment Paragraph" TeX-comment-paragraph t]
	["Comment Region" TeX-comment-region t]
	["Switch to Master file" TeX-home-buffer t]
	["Documentation" TeX-goto-info-page t]
	["Submit bug report" TeX-submit-bug-report t]
	["Reset Buffer" TeX-normal-mode t]
	["Reset AUC TeX" (TeX-normal-mode t) :keys "C-u C-c C-n"]))

;;; Comments

(defun TeX-un-comment-region (start end level)
  "Remove up to LEVEL comment characters from each line in the region."
  (interactive "*r\np") 
  (comment-region start end (- level)))

(defun TeX-un-comment (level)
  "Delete up to LEVEL %'s from the beginning of each line in a comment."
  (interactive "*p")
  (save-excursion
    ; Find first comment line
    (re-search-backward (concat "^[^" comment-start "]") nil 'limit)
    (let ((beg (point)))
      (forward-line 1)
      ; Find last comment line
      (re-search-forward (concat "^[^" comment-start "]") nil 'limit)
      ; Uncomment region
      (comment-region beg (point) (- level)))))

(fset 'TeX-comment-region 'comment-region)

(defun TeX-comment-paragraph (level)
  "Inserts LEVEL %'s at the beginning of every line in the current paragraph."
  (interactive "*p")
  (if (< level 0)
      (TeX-un-comment (- level))
    (save-excursion
      (mark-paragraph)
      (comment-region (point) (mark) level))))

(defun TeX-in-comment ()
  ;; Return non-nil if point is in a comment.
  (if (or (bolp)
	  (null comment-start-skip)
	  (eq (preceding-char) ?\r))
      nil
    (save-excursion
      (let ((pos (point)))
	(re-search-backward "^\\|\r" nil t)
	(or (looking-at comment-start-skip)
	    (re-search-forward comment-start-skip pos t))))))

;;; Indentation

(defun TeX-brace-count-line ()
  "Count number of open/closed braces."
  (save-excursion
    (save-restriction
      (let ((count 0))
	(narrow-to-region (point)
			  (save-excursion
			    (re-search-forward "[^\\\\]%\\|\n\\|\\'")
			    (backward-char)
			    (point)))
	
	(while (re-search-forward "\\({\\|}\\|\\\\.\\)" nil t)
	  (cond
	   ((string= "{" (TeX-match-buffer 1))
	    (setq count (+ TeX-brace-indent-level count)))
	   ((string= "}" (TeX-match-buffer 1))
	    (setq count (- count TeX-brace-indent-level)))))
	count))))

(defvar TeX-brace-indent-level 2
  "*The level of indentation produced by a open brace.")

(defun TeX-comment-indent ()
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
	 comment-column)))

;;; Fonts

(defvar TeX-font-list '((?\C-b "{\\bf " "}")
			(?\C-c "{\\sc " "}")
			(?\C-e "{\\em " "\\/}")
			(?\C-i "{\\it " "\\/}")
			(?\C-r "{\\rm " "}")
			(?\C-s "{\\sl " "\\/}")
			(?\C-t "{\\tt " "}")
			(?\C-d "" "" t))
  "*List of fonts used by TeX-font.

Each entry is a list with three elements.  The first element is the
key to active the font.  The second element is the string to insert
before point, and the third element is the string to insert after
point.  An optional fourth element means always replace if not nil.")

(defun TeX-describe-font-entry (entry)
  ;; A textual description of an ENTRY in TeX-font-list.
  (concat (format "%8s\t" (key-description (char-to-string (nth 0 entry))))
	  (if (nth 3 entry)
	      "-- delete font"
	    (format "%10s %s" (nth 1 entry) (nth 2 entry)))))
  
(defun TeX-font (replace what)
  "Insert template for font change command.
If REPLACE is not nil, replace current font.  WHAT determines the font
to use, as specified by TeX-font-list."
  (interactive "*P\nc")
  (TeX-update-style)
  (let* ((entry (assoc what TeX-font-list)))
    (setq replace (or replace (nth 3 entry)))
    (cond ((null entry)
	   (let ((help (concat "Font list:\n\n"
			       (mapconcat 'TeX-describe-font-entry
					  TeX-font-list "\n"))))
	     (with-output-to-temp-buffer "*Help*"
	       (set-buffer "*Help*")
	       (insert help))))
	  (replace
	   (TeX-font-replace (nth 1 entry) (nth 2 entry)))
	  ((TeX-active-mark)
	   (save-excursion
	     (cond ((> (mark) (point))
		    (insert (nth 1 entry))
		    (goto-char (mark))
		    (insert (nth 2 entry)))
		   (t
		    (insert (nth 2 entry))
		    (goto-char (mark))
		    (insert (nth 1 entry))))))
	  (t
	   (insert (nth 1 entry))
	   (save-excursion
	     (insert (nth 2 entry)))))))
(defun TeX-font-replace (start end)
  "Replace font specification around point with START and END."
  (save-excursion
    (while (not (looking-at "{\\\\[a-zA-Z]+ "))
      (up-list -1))
    (forward-sexp)
    (save-excursion
      (replace-match start t t))
    (if (save-excursion
	  (backward-char 3)
	  (if (looking-at (regexp-quote "\\/}"))
	      (progn
		(delete-char 3)
		nil)
	    t))
	(delete-backward-char 1))
    (insert end)))

;;; Dollars
;;
;; Originally stolen from VorTeX.
;; Copyright (C) 1986, 1987, 1988 Pehong Chen (phc@renoir.berkeley.edu)

(defvar TeX-dollar-sign ?$
  "*Character user to enter and leaver math mode in TeX.")

(defconst TeX-dollar-string (char-to-string TeX-dollar-sign))

(defconst TeX-dollar-regexp 
  (concat "^" (regexp-quote TeX-dollar-string) "\\|[^" TeX-esc "]"
	  (regexp-quote TeX-dollar-string)))
  
(defvar TeX-dollar-list nil)
  (make-variable-buffer-local 'TeX-match-dollar-on)

(defvar TeX-par-start nil)
  (make-variable-buffer-local 'TeX-par-start)

(defvar TeX-par-end nil)
  (make-variable-buffer-local 'TeX-par-end)

(defvar TeX-symbol-marker nil)

(defvar TeX-symbol-marker-pos 0)

(defun TeX-bouncing-point (m)
  (save-excursion
    (if (pos-visible-in-window-p)
	(sit-for 1)
      (let* ((pos1 (point))
             (pos2 (+ pos1 m))
             (sym (buffer-substring pos1 pos2))
             (msg1 (progn (beginning-of-line) (buffer-substring (point) pos1)))
             (msg2 (progn (end-of-line) (buffer-substring pos2 (point)))))
        (message "%s`%s'%s" msg1 sym msg2)))))

(defun TeX-locate-delimiter (pos sym symlst)
  (let ((marker nil)
        (marker-pos 0)
        (pair t)
        (head nil))
    (catch 'loop
      (while symlst
        (setq marker (car symlst))
        (setq marker-pos (1- (marker-position marker)))
        (if (and (/= pos marker-pos) (= (char-after marker-pos) sym))
	    (if (> pos marker-pos)
		(progn
		  (setq TeX-symbol-marker-pos marker-pos)
		  (setq TeX-symbol-marker marker) 
		  (setq head (cons marker head))
		  (setq pair (not pair)))
	      (if pair (setq TeX-symbol-marker nil))
	      (throw 'loop (append (reverse head)
				   (cons (set-marker (make-marker) (1+ pos)) 
					 symlst)))))
        (setq symlst (cdr symlst)))
      (if pair (setq TeX-symbol-marker nil))
      (reverse (cons (set-marker (make-marker) (1+ pos)) head)))))

(defun TeX-dollar-verify ()
  ;; Verify if the current paragraph is the same as last.
  ;; If so, do nothing, otherwise reset TeX-par-start and TeX-par-end and
  ;; reconstruct the symbol-list.
  (let ((start (save-excursion
                 (if (re-search-backward paragraph-separate nil t)
		     (point)
                   1)))
        (end (save-excursion
               (if (re-search-forward paragraph-separate nil t)
		   (1+ (point))
                 (1+ (point-max)))))
        (init nil))
    (if (null TeX-par-start)
	(setq TeX-par-start (set-marker (make-marker) 1)))
    (if (/= (marker-position TeX-par-start) start)
	(progn
	  (set-marker TeX-par-start start)
	  (setq init t)))
    (if (null TeX-par-end)
	(setq TeX-par-end (set-marker (make-marker) 1)))
    (if (/= (marker-position TeX-par-end) end)
	(progn
	  (set-marker TeX-par-end end)
	  (setq init t)))
    (if init
	(save-excursion
	  (setq TeX-dollar-list nil)
	  (goto-char start)
	  (while (re-search-forward TeX-dollar-regexp end t)
	    (setq TeX-dollar-list
		  (append TeX-dollar-list
			  (list (set-marker (make-marker)
					    (if (= (following-char)
						   TeX-dollar-sign)
						(progn
						  (forward-char 1)
						  (point))
					      (point)))))))))))

(defun TeX-insert-dollar (&optional arg)
  "Insert dollar sign.  

Show matching dollar sign if this dollar sign end the TeX math mode.  
Ensure double dollar signs match up correctly by inserting extra
dollar signs when needed.

With optional ARG, insert that many dollar signs."
  (interactive "P")
  (if arg
      (let ((count (prefix-numeric-value arg)))
	(if (listp arg)
	    (self-insert-command 1)	;C-u always inserts just one
	  (self-insert-command count)))
    (let ((pc (preceding-char))
	  (pos (point))
	  (pt (point))
	  (single t))
      (TeX-dollar-verify)
      (if (= pc (string-to-char TeX-esc))
	  (insert TeX-dollar-sign)
	(if (and (= pc TeX-dollar-sign)
		  (/= (char-after (- (point) 2)) (string-to-char TeX-esc)))
	    (progn
	      (setq single nil)
	      (if (and (> pos 2) (= (char-after (- pos 2)) TeX-dollar-sign))
		  (setq pt (1- pos))	; Doesn't echo 3rd $, if $$ already
		(backward-char 1) 
		(insert TeX-dollar-sign)
		(goto-char (1+ pos))))
	  (insert TeX-dollar-sign))
	(setq TeX-dollar-list
	      (TeX-locate-delimiter pt TeX-dollar-sign TeX-dollar-list))
	(if TeX-symbol-marker
	    (save-excursion
	      (goto-char TeX-symbol-marker-pos)
	      (if (and (= (preceding-char) TeX-dollar-sign)
		       (/= (char-after (- (point) 2)) TeX-dollar-sign))
		  (progn
		    (backward-char 1)
		    (if single
			(save-excursion
			  (goto-char pos)
			  (insert TeX-dollar-sign))))	; $$foo$`$'
		(if (not single)
		    (progn
		      (insert TeX-dollar-sign) ; `$'$foo$$
		      (backward-char 1))))
	      (TeX-bouncing-point (if single 1 2))))))))

;;; Simple Commands

(defun TeX-normal-mode (arg)
  "Remove all information about this buffer, and apply the style hooks again.
Save buffer first including style information.
With optional argument, also reload the style hooks."
  (interactive "*P")
  (if arg
      (setq TeX-style-hook-list nil))
  (let ((TeX-auto-save t))
    (if (buffer-modified-p)
	(save-buffer)
      (TeX-auto-write)))
  (normal-mode)
  (TeX-update-style))

(defvar TeX-open-quote "``"
  "*String inserted by typing \\[TeX-insert-quote] to open a quotation.")

(defvar TeX-close-quote "''"
  "*String inserted by typing \\[TeX-insert-quote] to close a quotation.")

(defvar TeX-quote-after-quote nil
  "*Behaviour of \\[TeX-insert-quote]. Nil means standard behaviour;
when non-nil, opening and closing quotes are inserted only after \".")

;;;###autoload
(defun TeX-insert-quote (arg)
  "Insert the appropriate quote marks for TeX.
Inserts the value of `TeX-open-quote' (normally ``) or `TeX-close-quote'
\(normally '') depending on the context.  If `TeX-quote-after-quote'
is non-nil, this insertion works only after \". 
With prefix argument, always inserts \" characters."
  (interactive "*P")
  (if arg
      (self-insert-command (prefix-numeric-value arg))
    (TeX-update-style)
    (if TeX-quote-after-quote
	(insert (cond ((bobp)
		       ?\")
		      ((not (= (preceding-char) ?\"))
		       ?\")
		      ((save-excursion
			 (forward-char -1)
			 (bobp))
		       (delete-backward-char 1)
		       TeX-open-quote)
		      ((save-excursion
			 (forward-char -2) ;;; at -1 there is double quote
			 (looking-at "[ \t\n]\\|\\s("))
		       (delete-backward-char 1)
		       TeX-open-quote)
		      (t
		       (delete-backward-char 1)
		       TeX-close-quote)))
      (insert (cond ((bobp)
		     TeX-open-quote)
		    ((= (preceding-char) (string-to-char TeX-esc))
		     ?\")
		    ((= (preceding-char) ?\")
		     ?\")
		    ((save-excursion
		       (forward-char (- (length TeX-open-quote)))
		       (looking-at (regexp-quote TeX-open-quote)))
		     (delete-backward-char (length TeX-open-quote))
		     ?\")
		    ((save-excursion
		       (forward-char (- (length TeX-close-quote)))
		       (looking-at (regexp-quote TeX-close-quote)))
		     (delete-backward-char (length TeX-close-quote))
		     ?\")
		    ((save-excursion
		       (forward-char -1)
		       (looking-at "[ \t\n]\\|\\s("))
		     TeX-open-quote)
		    (t
		     TeX-close-quote))))))

;; For the sake of BibTeX...
;;;###autoload
(fset 'tex-insert-quote 'TeX-insert-quote)

(defun TeX-insert-punctuation ()
  "Insert point or comma, cleaning up preceding space."
  (interactive)
  (if (TeX-looking-at-backward "\\\\/\\(}+\\)" 50)
      (replace-match "\\1" t))
  (call-interactively 'self-insert-command))

(defun TeX-insert-braces (arg)
  "Make a pair of braces around next ARG sexps and leave point inside.
No argument is equivalent to zero: just insert braces and leave point
between."
  (interactive "P")
  (insert TeX-grop)
  (save-excursion
    (if arg (forward-sexp (prefix-numeric-value arg)))
    (insert TeX-grcl)))

(defun TeX-goto-info-page ()
  "Read documentation for AUC TeX in the info system."
  (interactive)
  (require 'info)
  (Info-goto-node "(auctex)"))

;;;###autoload
(defun TeX-submit-bug-report ()
  "Submit via mail a bug report on AUC TeX"
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   "auc-tex@iesd.auc.dk"
   (concat "AUC TeX " AUC-TeX-version)
   (list 'window-system
	 'TeX-style-path
	 'TeX-auto-save
	 'TeX-parse-self
	 'TeX-master)
   nil nil
   "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen."))

;;; Ispell Support

;; The FSF ispell.el use this.
(defun ispell-tex-buffer-p ()
  (and (boundp 'ispell-tex-p) ispell-tex-p))

;; The FSF ispell.el might one day use this.
(setq ispell-enable-tex-parser t)

(defun TeX-run-ispell (command string file)
  "Run ispell on current TeX buffer."
  (cond ((and (string-equal file (TeX-region-file))
	      (fboundp 'ispell-region))
	 (call-interactively 'ispell-region))
	((string-equal file (TeX-region-file))
	 (call-interactively 'spell-region))
	((fboundp 'ispell-buffer)
	 (ispell-buffer))
	((fboundp 'ispell)
	 (ispell))
	(t 
	 (spell-buffer))))

;; Some versions of ispell 3 use this.
(defvar ispell-tex-major-modes nil)
(setq ispell-tex-major-modes
      (append '(plain-tex-mode ams-tex-mode latex-mode)
	      ispell-tex-major-modes))

(provide 'tex)

;;; tex.el ends here
