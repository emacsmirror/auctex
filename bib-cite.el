;; bib-cite.el - Display citation under cursor / Extract refs from BiBTeX file.

;; Copyright (C) 1994, 1995 Peter S. Galbraith
 
;; Author:    Peter S. Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Created:   06 July 1994
;; Version:   1.08 (16 January 95)
;; Keywords:  bibtex, cite, auctex 

;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;; ----------------------------------------------------------------------------
;;; Commentary:

;; New versions of this package (if they exist) may be found at:
;;   /anonymous@bathybius.meteo.mcgill.ca:/pub/users/rhogee/elisp/bib-cite.el

;; Operating Systems:
;;  Works in unix, DOS and OS/2.  Developped under Linux.
;;  VMS: I have no clue if this works under VMS. I don't know how emacs handle 
;;  logical names (i.e. for BIBINPUTS) but I am willing to fix this package for
;;  VMS if someone if willing to test it and answer questions.

;; Description:
;; ~~~~~~~~~~~
;;  This package is used in various TeX modes to display or edit references
;;  associated with \cite commands. Various flavors of \cite commands are
;;  allowed (as long as they contain the word `cite') and they may optionally
;;  have bracketed [] options.  Cross-references are displayed, and @string
;;  abbreviations are substituted or included.
;;
;;  The reference text is found (by emacs) in the bibtex source files listed in
;;  the \bibliography command.  The BiBTeX files can be located in a search
;;  path defined by an environment variable (typically BIBINPUTS, but you can
;;  change this).
;;
;;  All citations used in a document can also be listed in a new bibtex buffer
;;  by using bib-make-bibliography.  This is useful to make a bibtex file for a
;;  document from a large bibtex database.  In this case, cross-references are
;;  included, as well as the @string commands used. The @string abbreviations
;;  are not substituted.
;;
;;  The bibtex files can also be searched for entries matching a regular
;;  expression using bib-apropos.

;; Usage instructions:
;; ~~~~~~~~~~~~~~~~~~
;;  bib-display-citation:
;;
;;   With cursor on the \cite command itslef -> display all its citations.
;;   With cursor on a particular cite key within the brackets -> display that
;;   reference
;;
;;   Example:
;;
;;    \cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
;;      ^Display-all-citations          ^Display-this-citation
;;
;;  bib-display-citation-mouse:
;;
;;   Use Shift-Mouse1 to use bib-display-citation (as described above) on a
;;   citation under the mouse.
;; 
;;  bib-edit-citation:
;;
;;   Edit the BiBTeX file and entry associated with the cite key under the
;;   cursor.
;;
;;  bib-make-bibliography:
;;
;;   Extract citations used in the current buffer from the \bibliography{}
;;   file(s).  Put them into a new suitably-named buffer.
;;
;;   This buffer is not saved to a file.  It is your job to save it to whatever
;;   name you wish.  Note that auctex has a unique name space for LaTeX and
;;   BiBTeX files, so you should *not* name the bib file associated with
;;   example.tex as example.bib!  Rather, name it something like
;;   example-bib.bib.
;;
;;  bib-apropos:
;;
;;   Searches the \bibliography{} file(s) for entries containing a keyword
;;   and display them in the *help* buffer.  You can trim down your search
;;   by using bib-apropos in the *Help* buffer after the first invocation.
;;   the current buffer is also searched for keyword matches if it is in
;;   bibtex-mode. 
;;   
;;   It doesn't display cross-references nor does it substitute or display
;;   @string commands used.  It could easily be added, but it's faster this
;;   way.  Drop me a line if this would be a useful addition.

;; Installation instructions:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~
;;  All you need to do is add this line to your .emacs file (optionally in 
;;  your LaTeX-mode-hook for later loading):
;;
;;    (require 'bib-cite)
;;
;;  It can be used with auctex, or stand-alone.  If used with auctex on a
;;  multi-file document (and auctex's parsing is used), then all \bibliography
;;  commands in the document will be found and used.
;;
;;  If you use hilit19 (or hl319), then bib-display-citation will use it to
;;  highlight the display unless you turn this off with:
;;
;;    (setq bib-hilit-if-available nil)
;;
;;    You are welcome to tell me how to do the same using font-lock.
;;
;;  If you use DOS or OS/2, you may have to set the following variable:
;;
;;    (setq bib-dos-or-os2-variable t)
;;
;;    if bib-cite.el fails to determine that you are using DOS or OS/2.
;;  Try `C-h v bib-dos-or-os2-variable' to see if it needs to be set manually.
;;
;;  If your environment variable to find BiBTeX files is not BIBINPUTS, then
;;  reset it with the following variable (here, assuming it's TEXBIB instead):
;;
;;    (setq bib-bibtex-env-variable "TEXBIB")
;;
;;  If you do not wish bib-display-citations to substitute @string
;;  abbreviations, then set the following variable like so:
;;
;;    (setq bib-substitute-string-in-display nil)
;;
;;  Warnings are given when @string abbreviations are not defined in your bib
;;  files.  The exception is for months, usually defined in style files. If you
;;  use other definitions in styles file (e.g. journals), then you may add them
;;  to the `bib-substitute-string-in-display' list.

;; Defining keys:
;; ~~~~~~~~~~~~~
;;  The problem is what to choose for key defs!
;;  auctex uses all C-c C-letter combos except for letters o p t v y
;;   
;;  If you wanted to bind bib-display-citation to C-c C-p, you'd use:
;;   
;;    (define-key LaTeX-mode-map "\C-c\C-p" 'bib-display-citation)
;;   
;;  Of course, all the `C-c letter' keys (no control prefix on the second
;;  letter) are reserved for you, the end-user.  So, define whatever you want!
;;  e.g. `C-c p' like so:
;;   
;;    (define-key LaTeX-mode-map "\C-cp" 'bib-display-citation)
;;   
;;  The only catch is that LaTeX-mode-map only exists after auc-tex is loaded.
;;  If you don't load auctex in your .emacs , but rather load it later as you
;;  need it using an auto-load, then you'll need to put this key definition in
;;  a hook:
;;   
;;  (defun extra-LaTeX-mode-hook ()
;;    "My hook for auc-tex's LaTeX-mode-hook"
;;    (define-key LaTeX-mode-map "\C-cp" 'bib-display-citation)
;;    ;; Also set auto-fill-mode... what the heck!
;;    (auto-fill-mode t))
;;   
;;  (add-hook 'LaTeX-mode-hook 'extra-LaTeX-mode-hook)
;;
;;  If you don't use auc-tex, but rather emacs' tex-mode, then use the above,
;;  but substitute the following: LaTeX-mode-map by tex-mode-map
;;  LaTeX-mode-hook by tex-mode-hook

;; If you find circumstances in which this package fails, please let me know.

;; Things for me to do in later versions:
;; - allow trimming apropos finds by using it recursively in *help* buffer.
;; - implement string concatenation, with #[ \t\n]*STRING_NAME
;; - Create new command to substitute @string text in any bibtex buffer.

;; (Following LCD Archive Entry included for your later use --- 
;;  Not yet submitted but perhaps you can get your next version there)
;; LCD Archive Entry:
;; bib-cite.el|Peter Galbraith|rhogee@bathybius.meteo.mcgill.ca
;; |Display citation under cursor / Extract refs from BiBTeX file
;; Tue Nov 22 09:09:45 1994|1.06||
;;
;; ----------------------------------------------------------------------------
;;; Change log:
;; V1.08  Jan 16 95 - Peter Galbraith
;;     bib-apropos can be used within *Help* buffer to trim a search.
;; V1.07  Dec 13 94 - Peter Galbraith
;;   - Fixed: multi-line @string commands in non-inserted display.
;;   - Fixed: quoted \ character in @string commands.
;;   - BiBTeX comments should not affect bib-cite
;;   - Fixed bib-apropos (from Christoph Wedler <wedler@fmi.uni-passau.de>)
;;      Faster now, and avoids infinite loops.
;;   - Added bib-edit-citation to edit a bibtex files about current citation.
;;   - Allow space and newlines between citations: \cite{ entry1, entry2}
;;   - Added bib-substitute-string-in-display,  bib-string-ignored-warning
;;     and bib-string-regexp.
;;   - bib-display-citation (from Markus Stricker <stricki@vision.ee.ethz.ch>)
;;      Could not find entry with trailing spaces
;; V1.06  Nov 20 94 - Peter Galbraith
;;   - Fixed bib-apropos for:
;;        hilighting without invoking bibtex mode. 
;;        display message when no matches found.
;;        would search only last bib file listed (forgot to `goto-char 1')
;;   - Fixed bib-make-bibliography that would only see first citation in a 
;;     multi-key \cite command (found by Michail Rozman <roz@physik.uni-ulm.de>
;;   - bib-make-bibliography didn't see \cite[A-Z]* commands.
;;     Found by Richard Stanton <stanton@haas.berkeley.edu>
;;     ************************************************** 
;;   - * Completely rewritten code to support crossrefs *
;;     **************************************************
;;   - autodetection of OS/2 and DOS for bib-dos-or-os2-variable
;;   - Created bib-display-citation-mouse
;;   - bib-apropos works in bibtex-mode on the current buffer
;;   - bibtex entry may have comma on next line (!)
;;       @ARTICLE{Kiryati-91
;;         , YEAR          = {1991    }
;;         ...
;; V1.05  Nov 02 94 - Peter Galbraith
;;   - bug fix by rossmann@TI.Uni-Trier.DE (Jan Rossmann) 
;;     for (boundp 'TeX-check-path) instead of fboundp.  Thanks!
;;   - Translate environment variable set by bib-bibtex-env-variable.
;;     (suggested by Richard Stanton <stanton@haas.berkeley.edu>)
;;   - add bib-dos-or-os2-variable  to set environment variable path separator
;;   - Add key-defs for any tex-mode and auc-tex menu-bar entries. 
;;       [in auc-tec TeX-mode-map is common to both TeX and LaTeX at startup
;;        (but TeX-mode-map is only copied to LaTeX-mode-map at initilisation)
;;        in plain emacs, use tex-mode-map for both TeX and LaTeX.]
;;   - Add key def for bibtex-mode to create auc-tex's parsing file.
;;   - Fix bugs found by <thompson@loon.econ.wisc.edu>
;;     - fix bib-get-citation for options 
;;     - fix bib-get-citation for commas preceeded citation command
;;     - better regexp for citations and their keys.
;;     - Added @string support for any entry (not just journal entries).
;;       (I had to disallow numbers in @string keys because of years.  
;;        Is that ok?)
;;   - added bib-apropos
;; V1.04  Oct 24 94 - Peter Galbraith 
;;   - Don't require dired-aux, rather define the function we need from it.
;;   - Regexp-quote the re-search for keys.
;;   - Name the bib-make-bibliography buffer diffently than LaTeX buffer
;;     because auc-tex's parsing gets confused if same name base is used.
;; V1.03  Oct 24 94 - Peter Galbraith - require dired-aux for dired-split
;; V1.02  Oct 19 94 - Peter Galbraith
;;   - If using auc-tex with parsing activated, use auc-tex's functions
;;     to find all \bibliography files in a multi-file document.
;;   - Find bib files in pwd, BIBINPUTS environment variable path and
;;     TeX-check-path elisp variable path.
;;   - Have the parser ignore \bibliography that is on a commented `%' line.
;;     (patched by Karl Eichwalder <karl@pertron.central.de>)
;;   - Allow for spaces between entry type and key in bib files:
;;     (e.g  @Article{  key} )
;;     (suggested by Nathan E. Doss <doss@ERC.MsState.Edu>)
;;   - Allows options in \cite command (e.g. agu++ package \cite[e.g.][]{key})
;;   - Includes @String{} abbreviations for `journal' entries
;; V1.01 July 07 94 - Peter Galbraith - \bibliography command may have list of
;;                                      BibTeX files.  All must be readable.
;; V1.00 July 06 94 - Peter Galbraith - Created
;; ----------------------------------------------------------------------------
;;; Code:

;;>>>>>>User-Modifiable variables start here:

(defvar bib-hilit-if-available t
  "*Use hilit19 or hl319 to hilit bib-display-citation if available")

(defvar bib-bibtex-env-variable "BIBINPUTS"
  "*Environment variable setting the path where BiBTeX input files are found.
BiBTeX 0.99b manual says this should be TEXBIB.
The colon character (:) is the default path separator in unix, but you may
use semi-colon (;) for DOS or OS/2 if you set bib-dos-or-os2-variable to `t'.")

(defvar bib-dos-or-os2-variable (or (equal 'emx system-type)
                                    (equal 'ms-dos system-type))
;; Under OS/2 system-type equals emx
;; Under DOS  system-type equals ms-dos
  "*`t' if you use DOS or OS/2 for bib-make-bibliography/bib-display-citation

It tells bib-make-bibliography and bib-display-citation to translate
the BIBINPUTS environment variable using the \";\" character as
a path separator and to translate DOS' backslash to slash.
  
e.g. Use a path like \"c:\\emtex\\bibinput;c:\\latex\\bibinput\"

(You can change the environment variable which is searched by setting the 
elisp variable bib-bibtex-env-variable")

(defvar bib-substitute-string-in-display t
  "*Determines if bib-display-citation will substitute @string definitions.
If t, then the @string text is substituted.
If nil, the text is not substituted but the @string entry is included.")

(defvar bib-string-ignored-warning 
  '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "sept" "oct" "nov" 
   "dec")
  "*List of @string abbreviations for which a warning is given if not defined.
These are usually month abbreviations (or journals) defined in a style file.")

;;<<<<<<User-Modifiable variables end here.

;; @string starts with a letter and does not contain any of ""#%'(),={}
;; Here we do not check that the field contains only one string field and
;; nothing else.
(defvar bib-string-regexp
      "^[, \t]*[a-zA-Z]+[ \t]*=[ \t]*\\([a-zA-Z][^#%'(),={}\" \t\n]*\\)"
      "Regular expression for field containing a @string")

(defun bib-display-citation ()
  "Display BibTeX citation under cursor from \bibliography input file.
Example with cursor located over cite command or arguments:
\cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
   ^Display-all-citations          ^Display-this-citation"
  (interactive)
  (save-excursion
    (let* ((the-keys-obarray (bib-get-citekeys-obarray)) ;1st in case of error
           (work-buffer (get-buffer-create "*bibtex-work*"))
           (bib-buffer (bib-get-bibliography nil))
           (the-warnings (bib-get-citations the-keys-obarray
                                            bib-buffer
                                            work-buffer
                                            bib-substitute-string-in-display))
           (the-warn-point))
      (if the-warnings
          (progn 
            (set-buffer work-buffer)
            (goto-char 1)
            (insert the-warnings)
            (setq the-warn-point (point))))
      (with-output-to-temp-buffer 
          "*Help*" 
        (set-buffer work-buffer)
        (princ (buffer-substring 1 (point-max))))
      (if (and bib-hilit-if-available
               (fboundp 'hilit-highlight-region))
          (progn
            (set-buffer "*Help*")
            (hilit-highlight-region 
             (point-min) (point-max) 'bibtex-mode t)
            (if the-warn-point
                (hilit-region-set-face 
                 1 the-warn-point 
                 (cdr (assq 'error hilit-face-translation-table))))))
      (kill-buffer bib-buffer)
      (kill-buffer work-buffer))))

(defun bib-display-citation-mouse (EVENT)
  "Display BibTeX citation under mouse from \bibliography input file."
  (interactive "e")
  (mouse-set-point EVENT)
  (bib-display-citation))

(defun bib-apropos (keyword)
  "Display BibTeX entries containing a keyword from bibliography file.
The files specified in the \\bibliography command are searched unless
the current buffer is in bibtex-mode or is the Help buffer.  In those
cases, *it* is searched.  This allows you to trim down a search further 
by using bib-apropos sequentially."
  (interactive "sBibTeX apropos: ")
  (let* ((the-text)(key-point)(start-point)
         (new-buffer-f (and (not (string-match "^bib" mode-name))
                            (not (string-equal "*Help*" (buffer-name)))))
         (bib-buffer (or (and new-buffer-f (bib-get-bibliography nil))
                         (current-buffer))))
    (save-excursion
      (set-buffer bib-buffer)
      (goto-char (point-min))
      (while (and (re-search-forward "^[ \t]*@" nil t)
                  (re-search-forward keyword nil t))
        (setq key-point (point))        ;To make sure this is within entry
        (re-search-backward "^[ \t]*@" nil t)
        (setq start-point (point))
        (forward-list 1)
        (if (< (point) key-point)       ;And this is that test...
            (goto-char key-point)       ;Not within entry, skip it.
          (setq the-text 
                (cons (concat (buffer-substring start-point (point)) "\n")
                      the-text))))
      (if (not the-text)
          (message "Sorry, no matches found.")
        (with-output-to-temp-buffer "*Help*" 
          (mapcar 'princ (nreverse the-text)))
        (if (and bib-hilit-if-available
                 (fboundp 'hilit-highlight-region))
            (progn
              (set-buffer "*Help*")
              (hilit-highlight-region 
               (point-min) (point-max) 'bibtex-mode t))))
      (if new-buffer-f
          (kill-buffer bib-buffer)))))

(defun bib-make-bibliography ()
  "Extract citations used in the current buffer from \bibliography{} file.
Put them into a buffer named after the current buffer, with extension .bib.
This is usuful when you want to share a LaTeX file, and therefore want to
create a bibtex file containing only the references used in the document."
  (interactive)
  (let* ((the-keys-obarray (bib-buffer-citekeys-obarray)) ;1st in case error
         (new-buffer 
          (create-file-buffer 
           (concat (substring (buffer-name) 0 
                              (or (string-match "\\." (buffer-name))
                                  (length (buffer-name))))
                   "-bib.bib")))
         (bib-buffer (bib-get-bibliography nil))
         (the-warnings (bib-get-citations the-keys-obarray
                                          bib-buffer
                                          new-buffer
                                          nil)))
    (kill-buffer bib-buffer)
    (switch-to-buffer new-buffer)
    (bibtex-mode)
    (if (and bib-hilit-if-available
             (fboundp 'hilit-highlight-region))
        (hilit-highlight-buffer t))
    (if the-warnings
        (progn
          (with-output-to-temp-buffer "*Help*" (princ the-warnings))
          (if (and bib-hilit-if-available
                   (fboundp 'hilit-region-set-face))
              (save-excursion
                (set-buffer "*Help*")
                (hilit-region-set-face 
                 1 (point-max)
                 (cdr (assq 'error hilit-face-translation-table)))))))))

(defun bib-edit-citation ()
  "Find and and put edit point in bib file associated with a BibTeX citation
under cursor from \bibliography input file.
In a multi-entry cite command, the cursor should be on the actual cite key
desired (otherwise a random entry will be selected). 
e.g.: \cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
                        ^Display-this-citation"
  (interactive)
  (let* ((the-keys-obarray (bib-get-citekeys-obarray)) ;1st in case of error
         (bib-buffer (bib-get-bibliography t))
         (the-key)(the-file))
    (save-excursion
      (mapatoms                         ;Do this for each cite-key found...
       (function (lambda (cite-key) 
                   (setq the-key (symbol-name cite-key))))
       the-keys-obarray)
      (set-buffer bib-buffer)
      (goto-char (point-min))
      (if (not (re-search-forward 
                (concat "@[^{]+{[\t ]*" the-key "[ ,\n]") nil t))
          (progn 
            (kill-buffer bib-buffer)
            (error "Sorry, could not find bib entry for %s" the-key))
        (re-search-backward "%%%Filename: \\([^\n]*\\)" nil t)
        (setq the-file (buffer-substring (match-beginning 1)(match-end 1)))
        (kill-buffer bib-buffer)))
    (find-file the-file)
    (re-search-forward (concat "@[^{]+{[\t ]*" the-key "[ ,\n]") nil t)))
    
;; -------------------------------------------------------------------------
;; The following routines are non-interactive (not for the end-user)

;;    ... is truly remarkable, as shown in \citeN{Thomson77,Test56}. Every
;; \cite[{\it e.g.}]{Thomson77,Test56}

(defun bib-get-citations (keys-obarray bib-buffer new-buffer substitute)
  "Put citations of KEYS-OBARRAY from BIB-BUFFER into NEW-BUFFER,
Substitute strings if SUBSTITUTE is t
Return the-warnings as text."
  (let ((the-warnings)                  ;The only variable to remember...
        (case-fold-search t))           ;All other results go into new-buffer
    ;; bibtex is not case-sensitive for keys.
    (save-excursion
      (let ((the-text))
        (set-buffer bib-buffer)
        (mapatoms                         ;Do this for each cite-key found...
         (function 
          (lambda (cite-key) 
            (goto-char (point-min))
            (if (re-search-forward 
                 (concat "@[^{]+{[\t ]*" 
                         (regexp-quote (symbol-name cite-key)) 
                         "\\([, ]\\\|$\\)")
                ;;            ^^     ^  comma, space or end-of-line
                 nil t)
                (setq the-text (concat the-text
                                       (buffer-substring 
                                        (progn (beginning-of-line)(point))
                                        (progn (forward-sexp 2)(point)))
                                       "\n\n"))
              (setq the-warnings (concat the-warnings 
                                         "Cannot find entry for: " 
                                         (symbol-name cite-key) "\n")))))
         keys-obarray)
        (if (not the-text) 
            (error "Sorry, could not find any of the references"))
        ;; Insert the citations in the new buffer
        (set-buffer new-buffer)
        (insert the-text)
        (goto-char 1))

      ;; We are at beginning of new-buffer.
      ;; Now handle crossrefs
      (let ((crossref-obarray (make-vector 201 0)))
        (while (re-search-forward
                "[, \t]*crossref[ \t]*=[ \t]*\\(\"\\|\{\\)" nil t)
          ;;handle {text} or "text" cases
          (if (string-equal "{" (buffer-substring (match-beginning 1)
                                                  (match-end 1)))
              (re-search-forward "[^\}]+" nil t)
            (re-search-forward "[^\"]+" nil t))
          (intern (buffer-substring (match-beginning 0)(match-end 0)) 
                  crossref-obarray))
        ;; Now find the corresponding keys,
        ;; but add them only if not already in `keys-obarray'
        (set-buffer bib-buffer)
        (goto-char 1)
        (let ((the-text))
          (mapatoms                     ;Do this for each crossref key found...
           (function
            (lambda (crossref-key)
              (if (not (intern-soft (symbol-name crossref-key) keys-obarray))
                  (progn
                    ;; Not in keys-obarray, so not yet displayed.
                    (goto-char (point-min))
                    (if (re-search-forward 
                         (concat "@[^{]+{[\t ]*" 
                                 (regexp-quote (symbol-name crossref-key)) 
                                 "\\(,\\|$\\)") 
                         nil t)
                        (setq the-text 
                              (concat the-text
                                      (buffer-substring 
                                       (progn (beginning-of-line)(point))
                                       (progn (forward-sexp 2)(point)))
                                      "\n\n"))
                      (setq the-warnings 
                            (concat the-warnings 
                                    "Cannot find crossref entry for: " 
                                    (symbol-name crossref-key) "\n")))))))
           crossref-obarray)
          ;; Insert the citations in the new buffer
          (set-buffer new-buffer)
          (goto-char (point-max))
          (if the-text
              (insert the-text)))
        (goto-char 1))

      ;; Now we have all citations in new-buffer, collect all used @String keys
      ;; Ex:  journal =      JPO,
      (let ((strings-obarray (make-vector 201 0)))
        (while (re-search-forward bib-string-regexp nil t)
          (intern (buffer-substring (match-beginning 1)(match-end 1)) 
                  strings-obarray))
        ;; Now find the corresponding @String commands
        ;; Collect either the @string commands, or the string to substitute
        (set-buffer bib-buffer)
        (goto-char 1)
        (let ((string-alist)
              (the-text))
          (mapatoms                     ;Do this for each string-key found...
           (function
            (lambda (string-key)
              (goto-char (point-min))
              ;; search for @string{ key = {text}} or @string{ key = "text"}
              (if (re-search-forward
                   (concat "^[ \t]*@string{"
                           (regexp-quote (symbol-name string-key))
                           "[\t ]*=[\t ]*\\(\"\\|\{\\)")
                   nil t)
                  (progn
                    ;;handle {text} or "text" cases
                    (forward-char -1)
                    (if (string-equal "{" (buffer-substring (match-beginning 1)
                                                            (match-end 1)))
                        (re-search-forward "[^\}]+}" nil t)
                      (re-search-forward "[^\"]+\"" nil t))
                    (if substitute      ;Collect substitutions
                        (setq string-alist
                              (append
                               string-alist
                               (list 
                                (cons (symbol-name string-key) 
                                      (regexp-quote
                                       (buffer-substring 
                                        (1- (match-beginning 0))
                      ;;                 ^ this grabs the leading quote/bracket
                                        (match-end 0)))))))
                      ;;Collect the strings command themseves
                      (setq the-text
                            (concat the-text 
                                    (buffer-substring 
                                     (progn (forward-char 1)(point))
                                     (re-search-backward "^[ \t]*@string{" 
                                                         nil t))
                                    "\n"))))
                ;; @string entry not found
                (if (not (member (symbol-name string-key) 
                                 bib-string-ignored-warning))
                    (setq the-warnings 
                          (concat the-warnings 
                                  "Cannot find @String entry for: " 
                                  (symbol-name string-key) "\n"))))))
           strings-obarray)
          ;; Now we have `the-text' of @string commands, 
          ;; or the `string-alist' to substitute.
          (set-buffer new-buffer)
          (if substitute
              (while string-alist
                (goto-char 1)
                (let ((the-key (car (car string-alist)))
                      (the-string (cdr (car string-alist))))
                  (while (re-search-forward 
                          (concat "\\(^[, \t]*[a-zA-Z]+[ \t]*=[ \t]*\\)" 
                                  the-key 
                                  "\\([, \t\n]\\)")
                          nil t)
                    (replace-match (concat "\\1" the-string "\\2") t)))
                (setq string-alist (cdr string-alist)))
            ;; substitute is nil; Simply insert text of @string commands
            (goto-char 1)
            (if the-text 
                (insert the-text "\n")))
          (goto-char 1))))

    ;; We are done!
    ;; Return the warnings...
    the-warnings))

(defun bib-get-citekeys-obarray ()
  "Return obarray of citation key (within curly brackets) under cursor."
  (save-excursion
    ;; First find *only* a key *within a cite command
    (let ((the-point (point))
          (keys-obarray (make-vector 201 0)))
      ;; First try to match a cite command
      (if (and (skip-chars-backward "\\\\a-zA-Z")
               (looking-at "\\\\[a-zA-Z]*cite[a-zA-Z]*"))
          (progn
            ;;skip over any optional arguments to \cite[][]{key} command
            (skip-chars-forward "\\\\a-zA-Z")
            (while (looking-at "\\[")
                (forward-sexp 1))
            (re-search-forward "{[ \n]*\\([^,} \n]+\\)" nil t)
            (intern (buffer-substring (match-beginning 1)(match-end 1))
                    keys-obarray)
            (while (and (skip-chars-forward " \n") ;no effect on while
                        (looking-at ","))
              (forward-char 1)
              ;;The following re-search skips over leading spaces
              (re-search-forward "\\([^,} \n]+\\)" nil t) 
              (intern (buffer-substring (match-beginning 1)(match-end 1))
                      keys-obarray)))
            
        ;; Assume we are on the keyword
        (goto-char the-point)
        (let ((the-start (re-search-backward "[\n{, ]" nil t))
              (the-end (progn (goto-char the-point)
                              (re-search-forward "[\n}, ]" nil t))))
          (if (and the-start the-end)
              (intern (buffer-substring (1+ the-start) (1- the-end))
                      keys-obarray)
            ;; Neither...
            (goto-char the-point)
            (error "Sorry, can't find a citation here..."))))
      keys-obarray)))

(defun bib-buffer-citekeys-obarray ()
  "Extract citations keys used in the current buffer"
  (let ((keys-obarray (make-vector 201 0)))
    (save-excursion
      (goto-char (point-min))
      ;; Following must allow for \cite[e.g.][]{key} !!!
      ;; regexp for \cite{key1,key2} was "\\\\[a-Z]*cite[a-Z]*{\\([^,}]+\\)"
      (while (re-search-forward "\\\\[a-zA-Z]*cite[a-zA-Z]*\\(\\[\\|{\\)" 
                                nil t)
        (backward-char 1)
        (while (looking-at "\\[")       ; ...so skip all bracketted options
          (forward-sexp 1))
        ;; then lookup first key
        (if (looking-at "{[ \n]*\\([^,} \n]+\\)")
            (progn
              (intern (buffer-substring (match-beginning 1)(match-end 1)) 
                      keys-obarray)
              (goto-char (match-end 1))
              (while (and (skip-chars-forward " \n")
                          (looking-at ","))
                (forward-char 1)
                (re-search-forward "\\([^,} \n]+\\)" nil t) 
                (intern (buffer-substring (match-beginning 1)(match-end 1))
                        keys-obarray)))))
      (if keys-obarray
          keys-obarray
        (error "Sorry, could not find any citation keys in this buffer.")))))

;; --------------------------------------------------------------------------
;; The following routines make a temporary bibliography buffer 
;; holding all bibtex files found.

(defun bib-get-bibliography (include-filenames-f)
  "Returns a new bibliography buffer holding all bibtex files in the document.

If using auc-tex, and either TeX-parse-self is set or C-c C-n is used to 
parse the document, then the entire multifile document will be searched 
for \bibliography commands.

If this fails, the current buffer is searched for the first \bibliography 
command.

If include-filenames-f is true, include as a special header the filename
of each bib file."
  (let ((bib-list (or (and (fboundp 'LaTeX-bibliography-list)
                           (boundp 'TeX-auto-update)
                           (LaTeX-bibliography-list))
;; LaTeX-bibliography-list (if bound) returns an unformatted list of
;; bib files used in the document, but only if parsing is turned on
;; or C-c C-n was used.
                      (bib-bibliography-list)))
        (bib-buffer (get-buffer-create "*bibtex-bibliography*"))
        (the-name)(the-warnings)(the-file))
    ;;We have a list of bib files
    ;;Search for them, include them, list those not readable
    (while bib-list
      (setq the-name (car (car bib-list))) ;Extract the string only
      (setq bib-list (cdr bib-list))
      (setq the-name 
            (substring the-name 
                       (string-match "[^ ]+" the-name) ;remove leading spaces
                       (string-match "[ ]+$" the-name))) ;remove trailing space 
      (if (not (string-match "\\.bib$" the-name))
          (setq the-name (concat the-name ".bib")))
      (setq the-file
            (or
             (and (file-readable-p (expand-file-name (concat "./" the-name)))
                  (expand-file-name (concat "./" the-name)))
             (psg-checkfor-file-list the-name 
                                     (psg-list-env bib-bibtex-env-variable))
             (and (boundp 'TeX-check-path)
                  (psg-checkfor-file-list the-name TeX-check-path))))
      (if the-file
          (progn 
            (save-excursion
              (set-buffer bib-buffer)
              (goto-char (point-max))
              (if include-filenames-f
                  (insert "%%%Filename: " the-file "\n"))
              (insert-file-contents the-file nil)
              (goto-char 1)))
        (setq the-warnings 
              (concat the-warnings "Could not read file: " the-name "\n"))))
    (if the-warnings
        (progn
          (with-output-to-temp-buffer "*Help*" 
            (princ the-warnings))
          (kill-buffer bib-buffer)
          (error 
           "Sorry, can't find all bibtex files in \\bibliography command"))
      bib-buffer)))

(defun bib-bibliography-list ()
  "Return list of bib files listed in first \bibliography command in buffer
Similar output to auc-tex's LaTeX-bibliography-list
The first element may contain trailing whitespace (if there was any in input)
although BiBTeX doesn't allow it!"
  (save-excursion
    (goto-char 1)
    (if (not (re-search-forward "^[ \t]*\\\\bibliography{[ \t]*\\([^},]+\\)" 
                                nil t))
        (error "Sorry, can't find \\bibliography command anywhere")
      (let ((the-list (list (buffer-substring 
                             (match-beginning 1)(match-end 1))))
            (doNext t))
        (while doNext
          (if (looking-at ",")
              (setq the-list
                    (append the-list
                            (list (buffer-substring 
                                   (progn (skip-chars-forward ", ")(point))
                                   (progn (re-search-forward "[,}]" nil t)
                                          (backward-char 1)
                                          (skip-chars-backward ", ")
                                          (point))))))
            (setq doNext nil)))
        (mapcar 'list the-list)))))

;; ---------------------------------------------------------------------------
;; Key definitions start here... (but not for xemacs!)

;; BibTeX-mode key def to create auc-tex's parsing file. 
(defun bib-create-auto-file ()
  "Force the creation of the auc-tex auto file for a bibtex buffer"
  (interactive)
  (if (not (require 'latex))
      (error "Sorry, This is only useful if you have auc-tex")) 
  (let ((TeX-auto-save t))
    (TeX-auto-write)))

;;; >>>This section can be removed if incorporated into auctex
;;; >>>by defining the menus in auc-tex (with auto-loads)
;;; pull-down menu key defs

(if (and (not (string-match "XEmacs\\|Lucid" emacs-version))
         (string-equal "19" (substring emacs-version 0 2))
         window-system)
    (progn
      ;; to auc-tex auto file for a bibtex buffer
      (if (boundp 'bibtex-mode-map)
          (progn
            (define-key-after 
              (lookup-key bibtex-mode-map [menu-bar move/edit]) 
              [bib-nil]
              '("---" . nil)
              '"--")
            (define-key-after 
              (lookup-key bibtex-mode-map [menu-bar move/edit]) 
              [bib-apropos]
              '("Search Apropos" . bib-apropos)
              'bib-nil)
            (define-key-after 
              (lookup-key bibtex-mode-map [menu-bar move/edit]) 
              [auc-tex-parse]
              '("Create auc-tex auto parsing file" . bib-create-auto-file)
              'bib-apropos))
        (if (and (fboundp 'add-hook) (fboundp 'remove-hook))
            (progn
              (defun bib-bibtex-mode-hook ()
                "hook created by bib-cite.el"
                (define-key-after 
                  (lookup-key bibtex-mode-map [menu-bar move/edit]) 
                  [bib-nil]
                  '("---" . nil)
                  '"--")
                (define-key-after 
                  (lookup-key bibtex-mode-map [menu-bar move/edit]) 
                  [bib-apropos]
                  '("Search Apropos" . bib-apropos)
                  'bib-nil)
                (define-key-after 
                  (lookup-key bibtex-mode-map [menu-bar move/edit]) 
                  [auc-tex-parse]
                  '("Create auc-tex auto parsing file" . 
                    bib-create-auto-file)
                  'bib-apropos)
                (remove-hook 'bibtex-mode-hook 'bib-bibtex-mode-hook))
              (add-hook 'bibtex-mode-hook 'bib-bibtex-mode-hook))))

      ;;for plain tex-mode
      (if (boundp 'tex-mode-map)
          (progn                        ; ...already loaded
            (define-key tex-mode-map [S-mouse-1] 'bib-display-citation-mouse)
            (define-key tex-mode-map [menu-bar tex bib-make-bibliography]
              '("Make BiBTeX bibliography buffer" . bib-make-bibliography))
            (define-key tex-mode-map [menu-bar tex bib-display-citation]
              '("Display BiBTeX citation under point" . bib-display-citation))
            (define-key tex-mode-map [menu-bar tex bib-edit-citation]
              '("Edit BiBTeX citation under point" . bib-edit-citation))
            (define-key tex-mode-map [menu-bar tex bib-apropos]
              '("Search BiBTeX files" . bib-apropos)))
        ;; tex-mode is not already loaded, so put into a hook for later loading
        (if (and (fboundp 'add-hook) (fboundp 'remove-hook))
            (progn
              (defun bib-tex-mode-hook ()
                "hook created by bib-cite.el"
                (define-key tex-mode-map [S-mouse-1] 
                  'bib-display-citation-mouse)
                (define-key
                  tex-mode-map [menu-bar tex bib-make-bibliography]
                  '("Make BiBTeX bibliography buffer" . bib-make-bibliography))
                (define-key
                  tex-mode-map [menu-bar tex bib-display-citation]
                  '("Display BiBTeX citation under point" . 
                    bib-display-citation))
                (define-key tex-mode-map [menu-bar tex bib-edit-citation]
                  '("Edit BiBTeX citation under point" . bib-edit-citation))
                (define-key tex-mode-map [menu-bar tex bib-apropos]
                  '("Search BiBTeX files" . bib-apropos))
                (remove-hook 'tex-mode-hook 'bib-tex-mode-hook))
              (add-hook 'tex-mode-hook 'bib-tex-mode-hook))))

      ;;for loaded auc-tex's LaTeX-mode
      ;; Use define-key-after (must be compatible with alpha version as well)
      (if (boundp 'LaTeX-mode-map)
          (progn
            (define-key LaTeX-mode-map [S-mouse-1] 'bib-display-citation-mouse)
            (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
              [bib-nil]
              '("---" . nil)
              '"--")
            (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
              [bib-make-bibliography]
              '("Make BiBTeX bibliography buffer" . bib-make-bibliography)
              'bib-nil)
            (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
              [bib-display-citation]
              '("Display BiBTeX citation under point" . bib-display-citation)
              'bib-make-bibliography)
            (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
              [bib-edit-citation]
              '("Edit BiBTeX citation under point" . bib-edit-citation)
              'bib-make-bibliography)
            (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX])
              [bib-apropos]
              '("Search BiBTeX files" . bib-apropos)
              'bib-display-citation))
        ;; not already loaded, so put into a hook for later loading
        (if (and (fboundp 'add-hook) (fboundp 'remove-hook))
            (progn
              (defun bib-LaTeX-mode-hook ()
                "hook created by bib-cite.el"
                (define-key LaTeX-mode-map [S-mouse-1] 
                  'bib-display-citation-mouse)
                (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
                  [bib-nil]
                  '("---" . nil)
                  '"--")
                (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
                  [bib-make-bibliography]
                  '("Make BiBTeX bibliography buffer" . bib-make-bibliography)
                  'bib-nil)
                (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
                  [bib-display-citation]
                  '("Display BiBTeX citation under point" . 
                    bib-display-citation)
                  'bib-make-bibliography)
                (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX]) 
                  [bib-edit-citation]
                  '("Edit BiBTeX citation under point" . bib-edit-citation)
                  'bib-make-bibliography)
                (define-key-after (lookup-key LaTeX-mode-map [menu-bar LaTeX])
                  [bib-apropos]
                  '("Search BiBTeX files" . bib-apropos)
                  'bib-display-citation)
                (remove-hook 'LaTeX-mode-hook 'bib-LaTeX-mode-hook))
              (add-hook 'LaTeX-mode-hook 'bib-LaTeX-mode-hook))))

;;;>>>This doesn't work... why?
      ;;if auc-tex's LaTeX-mode is not loaded.  It will get the key defs
      ;;from auc-tec's TeX-mode-map.  Is this a problem?
;;;<<<This doesn't work... why?
      (if (boundp 'TeX-mode-map)
          (progn
            (define-key TeX-mode-map [S-mouse-1] 'bib-display-citation-mouse)
            (define-key TeX-mode-map [menu-bar TeX bib-display-citation]
              '("Display BiBTeX citation under point" . bib-display-citation))
            (define-key TeX-mode-map [menu-bar TeX bib-edit-citation]
              '("Edit BiBTeX citation under point" . bib-edit-citation))
            (define-key TeX-mode-map [menu-bar TeX bib-make-bibliography]
              '("Make BiBTeX bibliography buffer" . bib-make-bibliography))
            (define-key TeX-mode-map [menu-bar TeX bib-apropos]
              '("Search BiBTeX files" . bib-apropos)))
        (if (and (fboundp 'add-hook) (fboundp 'remove-hook))
            (progn
              (defun bib-TeX-mode-hook ()
                "hook created by bib-cite.el"
                (define-key TeX-mode-map [S-mouse-1] 
                  'bib-display-citation-mouse)
                (define-key TeX-mode-map [menu-bar TeX bib-make-bibliography]
                  '("Make BiBTeX bibliography buffer" . bib-make-bibliography))
                (define-key TeX-mode-map [menu-bar TeX bib-display-citation]
                  '("Display BiBTeX citation under point" . 
                    bib-display-citation))
                (define-key TeX-mode-map [menu-bar TeX bib-edit-citation]
                  '("Edit BiBTeX citation under point" . bib-edit-citation))
                (define-key TeX-mode-map [menu-bar TeX bib-apropos]
                  '("Search BiBTeX files" . bib-apropos))
                (remove-hook 'TeX-mode-hook 'bib-TeX-mode-hook))
              (add-hook 'TeX-mode-hook 'bib-TeX-mode-hook))))))

;;; <<<This section can be removed if incorporated into auctex
;;; <<<by defining the menus in auc-tex (with auto-loads)

;; --------------------------------------------------------------------------
;; The following routines are also defined in other packages...

;; Must be in sync with function of same name in ff-paths.el
(defun psg-checkfor-file-list (filename list)
  "Check for presence of FILENAME in directory LIST. Return 1st found path."
  ;;USAGE: (psg-checkfor-file-list "gri.cmd" (psg-list-env "PATH"))
  ;;USAGE: (psg-checkfor-file-list "gri-mode.el" load-path)
  ;;USAGE: (psg-checkfor-file-list "gri.cmd" (psg-translate-ff-list "gri.tmp"))
  (let ((the-list list) 
        (filespec))
    (while the-list
      (if (not (car the-list))          ; it is nil
          (setq filespec (concat "~/" filename))
        (setq filespec 
              (concat (file-name-as-directory (car the-list)) filename)))
      (if (file-exists-p filespec)
            (setq the-list nil)
        (setq filespec nil)
        (setq the-list (cdr the-list))))
    filespec))

(or (fboundp 'dired-replace-in-string)
    ;; This code is part of GNU emacs
    (defun dired-replace-in-string (regexp newtext string)
      ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
      ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
      (let ((result "") (start 0) mb me)
        (while (string-match regexp string start)
          (setq mb (match-beginning 0)
                me (match-end 0)
                result (concat result (substring string start mb) newtext)
                start me))
        (concat result (substring string start)))))


;; Could use fset here to equal TeX-split-string to dired-split if only 
;; dired-split is defined.  That would eliminate a check in psg-list-env.
(and (not (fboundp 'TeX-split-string))
     (not (fboundp 'dired-split))
     ;; This code is part of auc-tex
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
         (nreverse result))))

;; Must be in sync with function of same name in ff-paths.el
(defun psg-list-env (env)
  "Return a list of directory elements in ENVIRONMENT variable (w/o leading $)
argument may consist of environment variable plus a trailing directory, e.g.
HOME or HOME/bin (trailing directory not supported in dos or OS/2).

bib-dos-or-os2-variable affects:
  path separator used (: or ;)
  whether backslashes are converted to slashes"
  (let* ((value (if bib-dos-or-os2-variable
                    (dired-replace-in-string "\\\\" "/" (getenv env))
                  (getenv env)))
         (sep-char (or (and bib-dos-or-os2-variable ";") ":"))
         (entries (and value 
                       (or (and (fboundp 'TeX-split-string)
                                (TeX-split-string sep-char value))
                           (dired-split sep-char value))))
         entry
	 answers)
    (while entries
      (setq entry (car entries))
      (setq entries (cdr entries))
      (if (file-directory-p entry)
          (setq answers (cons entry answers))))
    (nreverse answers)))

(provide 'bib-cite)
;;; bib-cite.el ends here
