;;; tex-jp.el - Support for Japanese TeX.

;;; Code:

(require 'latex)

;;; Customization

(setq TeX-format-list
      (append '(("JLATEX" japanese-latex-mode
		 "\\\\\\(\\\\documentstyle[^%\n]*{j\\|\\\\documentclass[^%\n]*{j\\)")
		("JTEX" japanese-plain-tex-mode
		 "-- string likely in Japanese TeX --"))
	      TeX-format-list))

(setq TeX-command-list
      (append (list (list "jTeX" "jtex '\\nonstopmode\\input %t'"
			  'TeX-run-TeX nil t)
		    (list "pTeX" "ptex '\\nonstopmode\\input %t'"
                          'TeX-run-TeX nil t)
		    (list "jBibTeX" "jbibtex %s" 'TeX-run-BibTeX nil nil))
	      TeX-command-list))
       
(setq LaTeX-command-style
      (append (if (string-equal LaTeX-version "2")
		  '(("^ams" "amsjlatex")
		    ("^jslides$" "jslitex")
		    ("^j-?\\(article\\|report\\|book\\)$" "jlatex"))
                '(("^j-\\(article\\|report\\|book\\)$" "jlatex")
                  ("^j\\(article\\|report\\|book\\)$" "platex")
                  ("." "jlatex")))
	      LaTeX-command-style))

(setcdr (assoc "%l" TeX-expand-list)
	(list 'TeX-style-check LaTeX-command-style))

(defvar japanese-TeX-error-messages t
  "If non-nil, explain TeX error messages in Japanese.")

(if (or (boundp 'MULE)
	(featurep 'mule))
    (if (string-match "XEmacs" emacs-version)
	(progn
	  (defvar TeX-japanese-process-input-coding-system 
	    (find-coding-system 'euc-japan)
	    "TeX-process' coding system with standard input.")
	  (defvar TeX-japanese-process-output-coding-system 
	    (find-coding-system 'junet)
	    "TeX-process' coding system with standard output."))
      (progn
	(defvar TeX-japanese-process-input-coding-system *euc-japan*
	  "TeX-process' coding system with standard input.")
	(defvar TeX-japanese-process-output-coding-system *junet*
	  "TeX-process' coding system with standard output."))))

(if (boundp 'NEMACS)
    (defvar TeX-process-kanji-code 2
      "TeX-process' kanji code with standard I/O.
0:No-conversion  1:Shift-JIS  2:JIS  3:EUC/AT&T/DEC"))

(defvar japanese-LaTeX-default-style "j-article"
  "*Default when creating new Japanese documents.")
(make-variable-buffer-local 'japanese-LaTeX-default-style)

(defvar japanese-LaTeX-style-list
  '(("book")
    ("article")
    ("letter")
    ("slides")
    ("report")
    ("jbook")
    ("j-book")
    ("jarticle")
    ("j-article")
    ("jslides")
    ("jreport")
    ("j-report"))
  "*List of Japanese document styles.")
(make-variable-buffer-local 'japanese-LaTeX-style-list)

;;; Coding system

(if (boundp 'MULE)
    (setq TeX-after-start-process-function
	  (function (lambda (process)
		      (set-process-coding-system
		       process
		       TeX-japanese-process-input-coding-system
		       TeX-japanese-process-output-coding-system)))))
(if (boundp 'NEMACS)
    (setq TeX-after-start-process-function
	  (function
	   (lambda (process)
	     (set-process-kanji-code process TeX-process-kanji-code)))))

(if (and (string-match "XEmacs" emacs-version)
	 (featurep 'mule))
    (setq TeX-after-start-process-function
	  (function (lambda (process)
		      (set-process-input-coding-system
		       process
		       TeX-japanese-process-input-coding-system)
		      (set-process-output-coding-system
		       process
		       TeX-japanese-process-output-coding-system)))))

;;; Japanese Parsing

(if (or (boundp 'MULE)
	(featurep 'mule))
(progn

(defconst LaTeX-auto-regexp-list 
  (append
   '(("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?\\[\\([0-9]+\\)\\]\
\\[\\([^\]\\\\\n\r]+\\)\\]"
      (1 3 4) LaTeX-auto-optional)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-arguments)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?" 1 TeX-auto-symbol)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-env-args)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?" 1 LaTeX-auto-environment)
     ("\\\\newtheorem{\\(\\([a-zA-Z]\\|\\cj\\)+\\)}" 1 LaTeX-auto-environment)
     ("\\\\input{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\include{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\usepackage\\(\\[[^\]\\\\]*\\]\\)?\
{\\(\\([^#}\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)}"
      (2) LaTeX-auto-style)
     ("\\\\bibitem{\\(\\([a-zA-Z]\\|\\cj\\)[^, \n\r\t%\"#'()={}]*\\)}" 1 LaTeX-auto-bibitem)
     ("\\\\bibitem\\[[^][\n\r]+\\]{\\(\\([a-zA-Z]\\|\\cj\\)[^, \n\r\t%\"#'()={}]*\\)}"
      1 LaTeX-auto-bibitem)
     ("\\\\bibliography{\\([^#}\\\\\n\r]+\\)}" 1 LaTeX-auto-bibliography))
   LaTeX-auto-label-regexp-list
   LaTeX-auto-minimal-regexp-list)
  "List of regular expression matching common LaTeX macro definitions.")

(defconst plain-TeX-auto-regexp-list
  '(("\\\\def\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\let\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\font\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\chardef\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\new\\(count|dimen|muskip|skip\\)\\\\\\(\\([a-z]\\|\\cj\\)+\\)[^a-zA-Z@]"
     2 TeX-auto-symbol)
    ("\\\\newfont{?\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)}?" 1 TeX-auto-symbol)
    ("\\\\typein\\[\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)\\]" 1 TeX-auto-symbol)
    ("\\\\input +\\(\\.*[^#%\\\\\\.\n\r]+\\)\\(\\.[^#%\\\\\\.\n\r]+\\)?"
     1 TeX-auto-file)
    ("\\\\mathchardef\\\\\\(\\([a-zA-Z]\\|\\cj\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol))
  "List of regular expression matching common LaTeX macro definitions.")

(defconst BibTeX-auto-regexp-list
  '(("@[Ss][Tt][Rr][Ii][Nn][Gg]" 1 ignore)
    ("@[a-zA-Z]+[{(][ \t]*\\(\\([a-zA-Z]\\|\\cj\\)[^, \n\r\t%\"#'()={}]*\\)"
     1 LaTeX-auto-bibitem))
  "List of regexp-list expressions matching BibTeX items.")

))

(if (boundp 'NEMACS)
(progn

(defconst LaTeX-auto-regexp-list 
  (append
   '(("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?\\[\\([0-9]+\\)\\]\
\\[\\([^\]\\\\\n\r]+\\)\\]"
      (1 3 4) LaTeX-auto-optional)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-arguments)
     ("\\\\newcommand{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?" 1 TeX-auto-symbol)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\z\\)+\\)}?\\[\\([0-9]+\\)\\]"
      (1 3) LaTeX-auto-env-args)
     ("\\\\newenvironment{?\\(\\([a-zA-Z]\\|\\z\\)+\\)}?" 1 LaTeX-auto-environment)
     ("\\\\newtheorem{\\(\\([a-zA-Z]\\|\\z\\)+\\)}" 1 LaTeX-auto-environment)
     ("\\\\input{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\include{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\usepackage\\(\\[[^\]\\\\]*\\]\\)?\
{\\(\\([^#}\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)}"
      (2) LaTeX-auto-style)
     ("\\\\bibitem{\\(\\([a-zA-Z]\\|\\z\\)[^, \n\r\t%\"#'()={}]*\\)}" 1 LaTeX-auto-bibitem)
     ("\\\\bibitem\\[[^][\n\r]+\\]{\\(\\([a-zA-Z]\\|\\z\\)[^, \n\r\t%\"#'()={}]*\\)}"
      1 LaTeX-auto-bibitem)
     ("\\\\bibliography{\\([^#}\\\\\n\r]+\\)}" 1 LaTeX-auto-bibliography))
   LaTeX-auto-label-regexp-list
   LaTeX-auto-minimal-regexp-list)
  "List of regular expression matching common LaTeX macro definitions.")

(defconst plain-TeX-auto-regexp-list
  '(("\\\\def\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\let\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol-check)
    ("\\\\font\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\chardef\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1 TeX-auto-symbol)
    ("\\\\new\\(count|dimen|muskip|skip\\)\\\\\\(\\([a-z]\\|\\z\\)+\\)[^a-zA-Z@]"
     2 TeX-auto-symbol)
    ("\\\\newfont{?\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)}?" 1 TeX-auto-symbol)
    ("\\\\typein\\[\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)\\]" 1 TeX-auto-symbol)
    ("\\\\input +\\(\\.*[^#%\\\\\\.\n\r]+\\)\\(\\.[^#%\\\\\\.\n\r]+\\)?"
     1 TeX-auto-file)
    ("\\\\mathchardef\\\\\\(\\([a-zA-Z]\\|\\z\\)+\\)[^a-zA-Z@]" 1
     TeX-auto-symbol))
  "List of regular expression matching common LaTeX macro definitions.")

(defconst BibTeX-auto-regexp-list
  '(("@[Ss][Tt][Rr][Ii][Nn][Gg]" 1 ignore)
    ("@[a-zA-Z]+[{(][ \t]*\\(\\([a-zA-Z]\\|\\z\\)[^, \n\r\t%\"#'()={}]*\\)"
     1 LaTeX-auto-bibitem))
  "List of regexp-list expressions matching BibTeX items.")

))

(defconst TeX-auto-full-regexp-list 
  (append LaTeX-auto-regexp-list plain-TeX-auto-regexp-list)
  "Full list of regular expression matching TeX macro definitions.")

;;; Japanese TeX modes

(defvar japanese-TeX-mode nil
  "Flag to determine if Japanese initialization is needed.")

(add-hook 'plain-TeX-mode-hook 'japanese-plain-tex-mode-initialization)

;;;###autoload
(defun japanese-plain-tex-mode ()
  "Major mode for editing files of input for Japanese plain TeX.
Set japanese-TeX-mode to t, and enters plain-tex-mode."
  (interactive)
  (setq japanese-TeX-mode t)
  (plain-tex-mode))

(defun japanese-plain-tex-mode-initialization ()
  "Japanese plain-TeX specific initializations."
  (if japanese-TeX-mode
      (setq TeX-command-default "jTeX")))

(add-hook 'LaTeX-mode-hook 'japanese-latex-mode-initialization)

;;;###autoload
(defun japanese-latex-mode ()
  "Major mode for editing files of input for Japanese plain TeX.
Set japanese-TeX-mode to t, and enters latex-mode."
  (interactive)
  (setq japanese-TeX-mode t)
  (latex-mode))

(defun japanese-latex-mode-initialization ()
  "Japanese LaTeX specific initializations."
  (if japanese-TeX-mode
      (progn
	(setq LaTeX-default-style japanese-LaTeX-default-style)
	(setq LaTeX-style-list japanese-LaTeX-style-list)
	(setq TeX-command-BibTeX "jBibTeX")
	(setq japanese-TeX-mode nil))))

;;; MULE and NEMACS paragraph filling.

(if (boundp 'MULE)
(if (string-lessp emacs-version "19")
(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "*r\nP")
  (save-restriction
    (goto-char from)
    (skip-chars-forward "\n")
    (LaTeX-indent-line)
    (beginning-of-line)
    (narrow-to-region (point) to)
    (setq from (point))

    ;; Delete whitespace at beginning of line from every line,
    ;; except the first line.
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (delete-horizontal-space)
      (forward-line 1))

    ;; Ignore the handling routine related with `fill-prefix'.

    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.

    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    ;;; patch by S.Tomura 88-Jun-30
    ;;＜統合＞
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;;(while (re-search-forward "[.?!][])""']*$" nil t)
    ;;  (insert ? ))
    (while (re-search-forward "[.?!][])\"']*$" nil t)
      (if (eobp)
	  nil
	;; replace CR by two spaces.
	(delete-char 1)			; delete newline
	(insert "  ")))
    ;; end of patch
    ;; The change all newlines to spaces.
    ;; patched by S.Tomura 87-Dec-7
    ;; bug fixed by S.Tomura 88-May-25
    ;; modified by  S.Tomura 88-Jun-21
    ;;(subst-char-in-region from (point-max) ?\n ?\ )
    ;; modified by K.Handa 92-Mar-2
    ;; Spacing is not necessary for charcters of no word-separater.
    ;; The regexp word-across-newline is used for this check.
    (if (not (stringp word-across-newline))
	(subst-char-in-region from (point-max) ?\n ?\ )
      (goto-char from)
      (end-of-line)
      (while (not (eobp))
	(delete-char 1)
	(if (eobp) nil			; 92.6.30 by K.Handa
	  (if (not (looking-at word-across-newline))
	      (progn
		(forward-char -1)
		(if (and (not (eq (following-char) ? ))
			 (not (looking-at word-across-newline)))
		    (progn
		      (forward-char 1)
		      (insert ? ))
		  (forward-char 1))))
	  (end-of-line))))
    ;; After the following processing, there's two spaces at end of sentence
    ;; and single space at end of line within sentence.
    ;; end of patch
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
    (let ((prefixcol 0)
	  ;; patch by K.Handa 92-Mar-2
	  (re-break-point (concat "[ \t\n]\\|" word-across-newline))
	  ;; end of patch
	  )
      (while (not (eobp))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  ;; patched by S.Tomura 87-Jun-2
	  ;; Big change by K.Handa 92-Mar-2
	  ;; Move back to start of word.
	  ;; (skip-chars-backward "^ \n")
	  ;; (if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	  ;;    ;; Move back over whitespace before the word.
	  ;;    (skip-chars-forward "^ \n")
	  ;;  ;; Normally, move back over the single space between the words.
	  ;;  (forward-char -1))

	  ;; At first, find breaking point at the left of fill-column,
	  ;; but after kinsoku-shori, the point may be right of fill-column.
	  ;; 92.4.15 by K.Handa -- re-search-backward will back to prev line.
	  ;; 92.4.27 by T.Enami -- We might have gone back too much...
	  (let ((p (point)) ch)
	    (re-search-backward re-break-point nil 'mv)
	    (setq ch (following-char))
	    (if (or (= ch ? ) (= ch ?\t))
		(skip-chars-backward " \t")
	      (forward-char 1)
	      (if (<= p (point))
		  (forward-char -1))))
	  (kinsoku-shori)
	  ;; Check if current column is at the right of prefixcol.
	  ;; If not, find break-point at the right of fill-column.
	  ;; This time, force kinsoku-shori-nobashi.
	  (if (>= prefixcol (current-column))
	      (progn
		(move-to-column (1+ fill-column))
		;; 92.4.15 by K.Handa -- suppress error in re-search-forward
		(re-search-forward re-break-point nil t)
		(forward-char -1)
		(kinsoku-shori-nobashi))))
	;; end of patch S.Tomura

	;; Replace all whitespace here with one newline.
	;; Insert before deleting, so we don't forget which side of
	;; the whitespace point or markers used to be on.
	;; patch by S. Tomura 88-Jun-20
	;; 92.4.27 by K.Handa
	(skip-chars-backward " \t")
	(if mc-flag
	    ;; ＜分割＞  WAN means chars which match word-across-newline.
	    ;; (0)     | SPC + SPC* <EOB>	--> NL
	    ;; (1) WAN | SPC + SPC*		--> WAN + SPC + NL
	    ;; (2)     | SPC + SPC* + WAN	--> SPC + NL  + WAN
	    ;; (3) '.' | SPC + nonSPC		--> '.' + SPC + NL + nonSPC
	    ;; (4) '.' | SPC + SPC		--> '.' + NL
	    ;; (5)     | SPC*			--> NL
	    (let ((start (point))	; 92.6.30 by K.Handa
		  (ch (following-char)))
	      (if (and (= ch ? )
		       (progn		; not case (0) -- 92.6.30 by K.Handa
			 (skip-chars-forward " \t")
			 (not (eobp)))
		       (or
			(progn		; case (1)
			  (goto-char start)
			  (forward-char -1)
			  (looking-at word-across-newline))
			(progn		; case (2)
			  (goto-char start)
			  (skip-chars-forward " \t")
			  (and (not (eobp))
			       (looking-at word-across-newline)))
			(progn		; case (3)
			  (goto-char (1+ start))
			  (and (not (eobp))
			       (/= (following-char) ? )
			       (progn
				 (skip-chars-backward " ])\"'")
				 (memq (preceding-char) '(?. ?? ?!)))))))
		  ;; We should keep one SPACE before NEWLINE. (1),(2),(3)
		  (goto-char (1+ start))
		;; We should delete all SPACES around break point. (4),(5)
		(goto-char start))))
	;; end of patch
	(if (equal (preceding-char) ?\\)
	    (insert ? ))
	(insert ?\n)
	(delete-horizontal-space)

	;; Ignore the handling routine related with `fill-prefix'.

	(LaTeX-indent-line)
	(setq prefixcol (current-column))
	;; Justify the line just ended, if desired.
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))
	)
      (goto-char (point-max))
      (delete-horizontal-space))))
(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.\n\
Prefix arg means justify too.\n\
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "*r\nP")
  (save-restriction
    (goto-char from)
    (skip-chars-forward " \n")
    (LaTeX-indent-line)
    (beginning-of-line)
    (narrow-to-region (point) to)
    (setq from (point))
    
    ;; Delete whitespace at beginning of line from every line,
    ;; except the first line.
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (delete-horizontal-space)
      (forward-line 1))

    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.
    
    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    ;; patch by S.Tomura 88-Jun-30
    ;;＜統合＞
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;; (while (re-search-forward "[.?!][])\"']*$" nil t)
    ;;   (insert ? ))
    (while (re-search-forward "[.?!][])}\"']*$" nil t)
      (if (eobp)
	  nil
	;; replace CR by two spaces.
	;; insert before delete to preserve marker.
	(insert "  ")
	;; delete newline
	(delete-char 1)))
    ;; end of patch
    ;; The change all newlines to spaces.
    ;; (subst-char-in-region from (point-max) ?\n ?\ )
    ;; patched by S.Tomura 87-Dec-7
    ;; bug fixed by S.Tomura 88-May-25
    ;; modified by  S.Tomura 88-Jun-21
    ;; modified by K.Handa 92-Mar-2
    ;; Spacing is not necessary for charcters of no word-separater.
    ;; The regexp word-across-newline is used for this check.
    (if (not (stringp word-across-newline))
	(subst-char-in-region from (point-max) ?\n ?\ )
      ;;
      ;; WAN     +NL+WAN       --> WAN            + WAN
      ;; not(WAN)+NL+WAN       --> not(WAN)       + WAN
      ;; WAN     +NL+not(WAN)  --> WAN            + not(WAN)
      ;; SPC     +NL+not(WAN)  --> SPC            + not(WAN)
      ;; not(WAN)+NL+not(WAN)  --> not(WAN) + SPC + not(WAN)
      ;;
      (goto-char from)
      (end-of-line)
      (while (not (eobp))
	;; 92.8.26 , 92.8.30 by S. Tomura
	
	;; Insert SPC only when point is between nonWAN.  Insert
	;; before deleting to preserve marker if possible.
	(if (or (prog2		; check following char.
		    (forward-char)	; skip newline
		    (or (eobp)
			(looking-at word-across-newline))
		  (forward-char -1))
		(prog2		; check previous char.
		    (forward-char -1)
		    (or (eq (following-char) ?\ )
			(looking-at word-across-newline))
		  (forward-char)))
	    nil
	  (insert ?\ ))
	(delete-char 1)		; delete newline
	(end-of-line)))
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
    (let ((prefixcol 0) linebeg
	  ;; patch by K.Handa 92-Mar-2
	  (re-break-point (concat "[ \n]\\|" word-across-newline))
	  ;; end of patch
	  )
      (while (not (eobp))
	(setq linebeg (point))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  ;;(skip-chars-backward "^ \n")
	  (fill-move-backward-to-break-point re-break-point)
	  (if sentence-end-double-space
	      (while (and (> (point) (+ linebeg 2))
			  (eq (preceding-char) ?\ )
			  (not (eq (following-char) ?\ ))
			  (eq (char-after (- (point) 2)) ?\.))
		(forward-char -2)
		(fill-move-backward-to-break-point re-break-point linebeg)))
	  (kinsoku-shori)
	  (if (if (zerop prefixcol)
		  (save-excursion
		    (skip-chars-backward " " linebeg)
		    (bolp))
		(>= prefixcol (current-column)))
	      ;; Keep at least one word even if fill prefix exceeds margin.
	      ;; This handles all but the first line of the paragraph.
	      ;; Meanwhile, don't stop at a period followed by one space.
	      (let ((first t))
		(move-to-column prefixcol)
		(while (and (not (eobp))
			    (or first
				(and (not (bobp))
				     sentence-end-double-space
				     (save-excursion (forward-char -1)
						     (and (looking-at "\\. ")
							  (not (looking-at "\\.  ")))))))
		  (skip-chars-forward " ")
		  ;; (skip-chars-forward "^ \n")
		  (fill-move-forward-to-break-point re-break-point)
		  (setq first nil)))
	    ;; Normally, move back over the single space between the words.
	    (if (eq (preceding-char) ? )
		(forward-char -1))))
	(if mc-flag
	    ;; ＜分割＞  WAN means chars which match word-across-newline.
	    ;; (0)     | SPC + SPC* <EOB>	--> NL
	    ;; (1) WAN | SPC + SPC*		--> WAN + SPC + NL
	    ;; (2)     | SPC + SPC* + WAN	--> SPC + NL  + WAN
	    ;; (3) '.' | SPC + nonSPC		--> '.' + SPC + NL + nonSPC
	    ;; (4) '.' | SPC + SPC		--> '.' + NL
	    ;; (5)     | SPC*			--> NL
	    (let ((start (point))	; 92.6.30 by K.Handa
		  (ch (following-char)))
	      (if (and (= ch ? )
		       (progn		; not case (0) -- 92.6.30 by K.Handa
			 (skip-chars-forward " \t")
			 (not (eobp)))
		       (or
			(progn	; case (1)
			  (goto-char start)
			  (forward-char -1)
			  (looking-at word-across-newline))
			(progn	; case (2)
			  (goto-char start)
			  (skip-chars-forward " \t")
			  (and (not (eobp))
			       (looking-at word-across-newline)
			       ;; never leave space after the end of sentence
			       (not (fill-end-of-sentence-p))))
			(progn	; case (3)
			  (goto-char (1+ start))
			  (and (not (eobp))
			       (/= (following-char) ? )
			       (fill-end-of-sentence-p)))))
		  ;; We should keep one SPACE before NEWLINE. (1),(2),(3)
		  (goto-char (1+ start))
		;; We should delete all SPACES around break point. (4),(5)
		(goto-char start))))
	;; end of patch
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
      (delete-horizontal-space))))))

(if (boundp 'NEMACS)
(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "r\nP")
  (save-restriction
    (goto-char from)
    (skip-chars-forward " \n")
    (LaTeX-indent-line)
    (beginning-of-line)
    (narrow-to-region (point) to)
    (setq from (point))

    ;; Delete whitespace at beginning of line from every line,
    ;; except the first line.
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (delete-horizontal-space)
      (forward-line 1))

    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.

    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    ;;; patch by S.Tomura 88-Jun-30
    ;;＜統合＞
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;;(while (re-search-forward "[.?!][])""']*$" nil t)
    ;;  (insert ? ))
    (while (re-search-forward "[.?!][])""']*$" nil t)
      (if (eobp)
	  nil
      (delete-char 1)
      (insert "  "))) ;; replace CR by two spaces.
    ;; end of patch
    ;; The change all newlines to spaces.
    ;; patched by S.Tomura 87-Dec-7
    ;; bug fixed by S.Tomura 88-May-25
    ;; modified by  S.Tomura 88-Jun-21
    ;;(subst-char-in-region from (point-max) ?\n ?\ )
    ;;日本語の語の後には空白はない。
    (goto-char from)
    (end-of-line)
    (while (not (eobp))
      (delete-char 1)
      (if (and (< ?  (preceding-char)) ;; + SPC + CR + X ==> + SPC + X
	       (< (preceding-char) 128)
	       (<= ?  (following-char))
	       (< (following-char) 128))
	   (insert ?\  ))
      (end-of-line))
    ;; 次の処理で文末にはtwo spacesがあり、それ以外はsingle spaceになっている。
    ;; end of patch
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
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward " " nil t)
      (if (<= 128 (following-char))
	  (let ((dummy 0))
	    (backward-char 1) 
	    (if (<= 128 (preceding-char))
		(delete-char 1))
	    (forward-char 1))))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0))
      (while (not (eobp))
	;; patched by S.Tomura 88-Jun-2
	;;(move-to-column (1+ fill-column))
	(move-to-column fill-column)
	;; end of patch
	;; patched by S.Tomura 88-Jun-16, 89-Oct-2, 89-Oct-19
	;; 漢字コードの場合にはfill-columnより大きくなることがある。
	(or (>= fill-column (current-column)) (backward-char 1))
	;; end of patch
	(if (eobp)
	    nil
	  ;; patched by S.Tomura 87-Jun-2
	  ;;(skip-chars-backward "^ \n")
	  ;;(if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	  ;;    (skip-chars-forward "^ \n")
	  ;;  (forward-char -1)))
	  ;; 原則としてfill-columnより左側に分割点を探す。
	  ;; Find a point to break lines
	     (skip-chars-backward " \t") ;; skip SPC and TAB
	     (if (or (<= 128 (preceding-char))
		     (<= 128 (following-char)) ;; 88-Aug-25
		     (= (following-char) ? )
		     (= (following-char) ?\t))
		 (kinsoku-shori)
	       (if(re-search-backward "[ \t\n]\\|\\z" ;; 89-Nov-17
				      (point-min) (point-min))
		   (forward-char 1))
	       (skip-chars-backward " \t")
	       (kinsoku-shori))
	     ;; prifixcolより右側に分割点を探す。
	     ;; この場合は分割点はfill-columnより右側になる。
	     (if (>= prefixcol (current-column))
		 (progn
		   (move-to-column prefixcol)
		   (if (re-search-forward "[ \t]\\|\\z" ;; 89-Nov-17
					  (point-max) (point-max))
		       (backward-char 1))
		   (skip-chars-backward " \t")
		   (kinsoku-shori)
		   ;; それも駄目なら分割を諦める。
		   (if (>= prefixcol (current-column)) (goto-char (point-max))))))
	;; end of patch S.Tomura
	;; patch by S. Tomura 88-Jun-20
	;;(delete-horizontal-space)
        ;;＜分割＞
        ;; 全角 | SPC + SPC＊   --> 全角 + SPC + CR
	;; | SPC + SPC* + 全角  --> SPC  + CR + 全角
        ;; . | SPC + SPC +      --> . + CR
        ;; . | SPC + nonSPC     --> . + SPC + CR + nonSPC
        ;;
        ;; . | 半角             --> 分割しない
        ;; . | 全角             --> 分割しない
	(if (not kanji-flag) (delete-horizontal-space)
	  (let ((start) (end))
	    (skip-chars-backward " \t")
	    (setq start (point))
	    (skip-chars-forward  " \t")
	    (setq end (point))
	    (delete-region start end)
	    (if (and (not
		      (and (save-excursion
			     (skip-chars-backward " ])\"'")
			     (memq (preceding-char) '(?. ?? ?!)))
			   (= end (+ start 2))))
		     (or (and (or (<= 128 (preceding-char))
				  (<= 128 (following-char)))
			      (< start end)
			      (not (eobp)))
			 (and (memq (preceding-char) '(?. ?? ?!))
			      (= (1+ start) end)
			      (not (eobp)))))
		(insert ?  ))))
	;; end of patch
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
      (delete-horizontal-space)))))

;;; Support for various self-insert-command

(cond ((fboundp 'can-n-egg-self-insert-command)
       (fset 'tex-jp-self-insert-command 'can-n-egg-self-insert-command))
      ((fboundp 'egg-self-insert-command)
       (fset 'tex-jp-self-insert-command 'egg-self-insert-command))
      ((fboundp 'canna-self-insert-command)
       (fset 'tex-jp-self-insert-command 'canna-self-insert-command))
      (t
       (fset 'tex-jp-self-insert-command 'self-insert-command)))

(defun TeX-insert-punctuation ()
  "Insert point or comma, cleaning up preceding space."
  (interactive)
  (if (TeX-looking-at-backward "\\\\/\\(}+\\)" 50)
      (replace-match "\\1" t))
  (call-interactively 'tex-jp-self-insert-command))

;;; Error Messages

(if japanese-TeX-error-messages
    (setq TeX-error-description-list
  '(("Bad \\\\line or \\\\vector argument.*" .
"線の傾きを指定する，\\lineまたは\\vectorの最初の引数が不正です．")

    ("Bad math environment delimiter.*" .
"数式モード中で数式モード開始コマンド\\[または\\(，または，数式モード外で
数式モード終了コマンド\\[または\\(をTeXが見つけました．この問題は，数式モー
ドのデリミタがマッチしていなかったり，括弧のバランスがとれていなかったりす
るために生じます．")

    ("Bad use of \\\\\\\\.*" .
"\\\\コマンドがパラグラフ中にありました．この使いかたは無意味です．
このエラーメッセージは\\\\がcentering環境やflushing環境で使われた
時，あるいはcentering/flushing宣言が有効なところで使われた時に生じます．")

    ("\\\\begin{[^ ]*} ended by \\\\end{[^ ]*}." .
"対応する\\begin命令のない\\end命令をLaTeXが見つけました．\\end命令の環
境名を間違えたか，余分な\\begin命令があるか，\\end命令をわすれたかのいず
れかでしょう．")

    ("Can be used only in preamble." .
"プリアンブルでしか使えない\\documentstyle・\\nofiles・\\includeonly
\\makeindex・\\makeglossaryのうちのいずれかが\\begin{document}よりも
後で使われているのをLaTeXが検出しました．このエラーは\\begin{document}
が余分にあった時にも生じます．")

    ("Command name [^ ]* already used.*" .
"すでに定義されている命令名または環境名に対して\\newcommand・
\\newenvironment・\\newlength・\\newsavebox・\\newtheoremのうちのいず
れかを実行しようとしています(ある環境を定義すると同じ名前の命令が自動
的に定義されるので，既に存在する環境と同名の命令は定義できません)．新
しい名前を考えるか，\\newcommandか\\newenvironmentの場合なら対応する
\\renew...命令を使わなければなりません．")

    ("Counter too large." .
"文字で順序付けされたもの，たぶん番号付けされたリスト環境のラベルが，
26よりも大きい番号を受け取りました．非常に長いリストを使っているか，
カウンタを再設定してしまったかのいずれかでしょう．")

    ("Environment [^ ]* undefined." .
"定義されていない環境に対する\\begin命令をLaTeXが見つけました．おそらく
環境名を間違えたのでしょう．")

    ("Float(s) lost." .
"parboxのなかにfigure環境・table環境または\\marginpar命令がありました
\(なお，parboxはminipage環境か\\parbox命令によって作られるか，脚注や図
などに対してLaTeXが生成するものです\)．これは出力時のエラーなので，原因
となっている環境あるいは命令は，LaTeXが問題を発見した場所よりもだいぶ
ん前にある可能性があります．出力されていない図・表・傍注などがいくつか
あるかもしれませんが，それらが原因であるとは限りません．")

    ("Illegal character in array arg." .
"array環境またはtabular環境の引数，または\\multicolumn命令の第2引数
の中に不正な文字がありました．")

    ("Missing \\\\begin{document}." .
"\\begin{document}命令より前にLaTeXが出力を行なってしまいました．
\\begin{document}命令を忘れたか，プリアンブルに何か間違いがあるのでしょう．
打ち間違いによる文字や，宣言の誤りによる可能性もあります．例えば，引数を
囲む括弧を抜かしたとか，命令名の\\を忘れた場合などです．")

    ("Missing p-arg in array arg.*" .
"array環境・tabular環境の引数，あるいは\\multicolumn命令の第2引数の中に，
括弧に囲まれた表現のついていないpがありました．")

    ("Missing @-exp in array arg." .
"array環境・tabular環境の引数，あるいは\\multicolumn命令の第2引数の中に，
@表現のついていない@がありました．")

    ("No such counter." .
"\\setcounter命令または\\addtocounter命令で，存在しないカウンタが指定され
ました．おそらくただのタイプミスでしょう．ただし，エラーがauxファイルの中
で生じた場合は，\\newcounter命令をプリアンブルの外で使ったのだと思われます．")

    ("Not in outer par mode." .
"figure環境・table環境あるいは\\marginpar命令が数式モードまたはparboxの中
で使われました．")

    ("\\\\pushtabs and \\\\poptabs don't match." .
"\\pushtabsと対応しない\\poptabsがみつかったか，または，対応する\\poptabs
をもたない\\pushtabsがあるのに\\end{tabbing}が現れてしまいました．")

    ("Something's wrong--perhaps a missing \\\\item." .
"リスト環境の中に\\item命令がないのが最もありそうなケースです．
thebibliography環境で引数を忘れた場合にも生じます．")

    ("Tab overflow." .
"\\=が，LaTeXで許されるタブストップの最大数を超えています．")

    ("There's no line here to end." .
"\\newline命令または\\\\命令がパラグラフ間にあります．この使いかたは
無意味です．もし空行をあけたいのでしたら，\\vspaceを使ってください．")

    ("This may be a LaTeX bug." .
"まったくわけがわからなくなってしまいました．たぶんこれ以前に検出された
エラーのせいだと思われます．しかし，LaTeX自体のバグである可能性もあります．
もしこのエラーが入力ファイルに対する最初のエラーであり，何も間違いが見つ
からない場合は，そのファイルを保存して，ローカルガイドに書かれている責任
者に連絡してください．")

    ("Too deeply nested." .
"リスト環境の入れ子が深すぎます．何段階の入れ子が許されるかは使っている
コンピュータに依存しますが，少なくとも4段階までは許されています(普通は
それで十分でしょう)．")

    ("Too many unprocessed floats." .
"このエラーは1ページ中の\\marginpar命令が多すぎるために生じる場合もあ
りますが，もっとありそうなのは，限界を超えて図や表を保存しようとした場
合です．長い文書を組版していくとき，LaTeXは図や表を個々に保存し，ペー
ジの分割を行なう時にそれらを挿入します．このエラーは，ページへの分割が
行なわれる前に，あまりにも多くのfigure環境やtable環境が見つかった場合
に生じます．この問題は環境のうちのいくつかを文書の終わりの方に移動すれ
ば解決できます．また，このエラーは``logjam''によって生じることもありま
す．``logjam''とは，LaTeXが出現順序通りにしか図表を出力できないせいで，
つまっている後ろの図表のために前の図表を出力できなくなることをいいます．
このジャムの原因は，大きすぎて1ページないしは指定された領域に収まらな
いような図や表である可能性があります．これは，引数にpオプションが指定
されていないと起きやすくなります．")

    ("Undefined tab position." .
"\\>・\\+・\\-または\\<命令で，存在しないタブ位置，すなわち\\=命令で定
義されていないタブ位置を指定しようとしています．")

    ("\\\\< in mid line." .
"\\<命令がtabbing環境の行の途中に現れました．この命令は行の先頭になければ
なりません．")

    ("Counter too large." .
"脚注が文字または脚注記号で順序づけされていますが，文字または記号を使い
切ってしまいました．おそらく\\thanks命令の使いすぎです．")

    ("Double subscript." .
"数式中の1つの列に2つの下付き文字がついています．例えばx_{2}_{3}のように．
このような表現は無意味です．")

    ("Double superscript." .
"数式中の1つの列に2つの上付き文字がついています．例えばx^{2}^{3}のように．
このような表現は無意味です．")

    ("Extra alignment tab has been changed to \\\\cr." .
"array環境またはtabular環境の1列中にある項目が多すぎます．言い換えると，
列の終わりまでにある&の数が多すぎます．おそらく前の列の最後に\\\\をつけ
るのを忘れたのでしょう．")

    ("Extra \\}, or forgotten \\$." .
"括弧または数式モードのデリミタが正しく対応していません．おそらく{・\\[・
\\(あるいは$のうちのいずれかを書き忘れたのでしょう．")

    ("Font [^ ]* not loaded: Not enough room left." .
"この文書は限界よりも多くのフォントを使っています．もし文書の部分ごとに
別々のフォントが使われているのなら，分割して処理すれば問題は解決されます．")

    ("I can't find file `.*'." .
"必要なファイルが見つかりませんでした．もし見つからないファイルの拡張子
がtexの場合，あなたが指定したファイル，すなわちメインファイルまたは
\\input命令・\\include命令で挿入されるファイルが見つからないのです．
拡張子がstyであれば，存在しない文書スタイルまたはスタイルオプションを
指定しようとしています．")

    ("Illegal parameter number in definition of .*" .
"これはおそらく，\\newcommand・\\renewcommand・\\newenvironmentまたは
\\renewenvironment命令のなかで#が正しく使われなかったために生じたエラー
です．\\#命令として使われる場合を除けば，#という文字は，例えば2番目の
引数を指定する#2のように，引数パラメータとしてしか使えません．また，
このエラーは，上にあげた4つのコマンドがお互いに入れ子になっている場合
や，\\newenvironment命令・\\renewenvironment命令で#2のようなパラメータ
が最後の引数の中で使われている場合にも生じます．")

    ("Illegal unit of measure ([^ ]* inserted)." .
"もし
      ! Missing number, treated as zero.
というエラーが起きた直後であれば，このエラーの原因もそれと同じです．
そうでない場合は，LaTeXが引数としてlengthを期待しているのにnumberが
現れたことを意味しています．このエラーの最もありがちな原因は長さ0を
表わす0inのような表現の代わりに0とかいてしまうことにあります．ただし，
命令の引数を書き忘れた場合にもこのエラーが生じることがあります．")

    ("Misplaced alignment tab character \\&." .
"arrayまたはtabular環境での項目区切りにのみ使われるべき文字&が普通の文
の中にありました．たぶん\\&と入力したかったのでしょう．")

    ("Missing control sequence inserted." .
"このエラーは，おそらく命令名でないものを\\newcommand・\\renewcommand・
\\newlengthまたは\\newsaveboxの第1引数として使ったために生じたのでしょう．")

    ("Missing number, treated as zero." .
"このエラーはたいてい，引数としてnumberまたはlengthを必要としている命令に
対して引数が与えられなかったために生じます．引数を書き忘れたのか，テキスト
の中の大括弧([])がオプション引数の指定と間違えられてしまったかのどちらかで
しょう．また，数を生成する\\valueのような命令やlength命令の前に\\protectを
置いた場合にもこのエラーは生じます．")

    ("Missing [{}] inserted." .
"TeXは既にわけがわからなくなっています．エラーメッセージによって示されて
いる場所はたぶん入力に間違いがあったところよりも後ろになってしまっている
でしょう．")

    ("Missing \\$ inserted." .
"おそらく，数式モード中でしか使えない命令をTeXが数式モード外で検出した
のだと思われます．特に記述されていない限り，LaTeX Book(Lamport著,訳書
はアスキー出版)の3.3節にある添字・分数・数学記号などのコマンドはすべて
数式モードでしか使えないのだということに注意してください．たとえ命令が
数式環境の中にあったとしても，boxを生成する命令の引数を処理しはじめた
時点では，TeXはまだ数式モードに入っていないのです．また，このエラーは，
数式モード中でTeXが空行を検出した場合にも生じます．")

    ("Not a letter." .
"\\hyphenation命令の引数の中になにか正しくないものがあります．")

    ("Paragraph ended before [^ ]* was complete." .
"命令の引数の中に不正な空行が入ってしまっています．おそらく引数の終わり
に閉じ括弧をつけるのを忘れたのでしょう．")

    ("\\\\[^ ]*font [^ ]* is undefined .*" .
"このエラーはあまり一般的でないフォントが数式モードで使われた時に生じ
ます．例えば，脚注の中の数式で\\sc命令が使われると，footnotesizeの
small capsフォントが呼びだされることになります．この問題は\\load命令を
使えば解決できます．")

    ("Font .* not found." .                                    
"未知のfamily/series/shape/sizeの組み合わせのフォントが指定されました．
このエラーが起きるケースは2つ考えられます．
   1) \\sizeマクロで使えないサイズを選択しようとした．
   2) そうでなければ，管理者のところに行って，フォント選択テーブルが
      腐っていると文句をつけてやりましょう!")

    ("TeX capacity exceeded, sorry .*" .
"TeXがメモリを使いきってしまい，実行を中断しました．しかし，慌てないで
ください．このエラーが生じた原因は，たぶん，TeXにあなたの文書を処理で
きるだけの能力がないからではありません．TeXにメモリを使いきらせた原因
は，おそらく入力したファイルの前の方で生じたエラーです．あなたが本当に
TeXの容量を超えたことをしようとしたのかどうか，そしてその場合どうすれ
ばいいのかを判断する方法を以下に説明します．もし問題が入力ファイル中の
エラーにある場合は，個々のエラーを解決していく方法をとるのがよいでしょ
う．LaTeXが短いファイルでメモリを使いきることはめったにありませんから，
エラーの起きた位置より前に処理したページが数ページしかなければ，まず間
違いなく入力ファイルに問題があるはずです．

エラーメッセージの最後に，TeXが使いきってしまったメモリの種類が示され
ています．それらのうち一般的なものについて，考えられる原因を以下に挙げ
ます．

buffer size
===========
章節・\\caption・\\addcontentslineあるいは\\addtocontents命令の引数と
して与えたテキストが長すぎる場合に生じることがあります．このエラーは
たいてい\\end{document}を処理している時に生じますが，\\tableofcontents・
\\listoffiguresあるいは\\listoftables命令を実行している場合にも起きる
ことがあります．この問題を解決するには，もっと短いテキストをオプション
引数として与えてください．目次や図表一覧を作成しても，見出しが長すぎる
と読みにくくなるはずです．

exception dictionary
====================
TeXが持っている領域以上にハイフネーション情報を与えようとしています．
あまり使わない単語の\\hyphenation命令を取り除いて，代わりに\\-命令を使っ
てください．

hash size 
=========
命令名の定義または相互参照ラベルの定義が多すぎます．

input stack size 
================
このエラーはおそらく命令定義中の誤りによるものです．例えば，次の命令は
再帰的定義となっており，自分自身を使って\\gnuを定義しています．

          \\newcommand{\\gnu}{a \\gnu} % これはだめ

この\\gnu命令を見つけるとTeXは\\gnuが何をうみだすのかを決定しようとし
てその末尾をいつまでも追いつづけ，やがて``input stack''を使いきってし
まいます．

main memory size 
================
これは，TeXが短いファイルを処理している時に使いきる可能性のあるメモリ
のひとつです．main memoryを使いきるのは次の3つの場合のいずれかです．
\(1\)非常に長く複雑な命令を数多く定義した．(2)indexまたはglossaryを作っ
ているとき，1ページ中にあまりにも多くの\\indexまたは\\glossary命令があ
る．(3)生成のための情報をTeXが保持しきれないような，あまりにも複雑なペー
ジを生成しようとした．最初の2つの問題の解決方法は明らかです．命令定義
の数あるいは\\index・\\glossary命令の数を減らすことです．3番目の問題は
ちょっと厄介です．これは，大きなtabbin・tabular・array・picture環境の
せいで生じることがあります．出力位置が決定されるのを待っている図や表で
TeXのメモリがいっぱいになっているのかもしれません．本当にTeXの容量を超
えてしまったのかどうか調べるためには，エラーの起こった場所の直前に
\\clearpage命令を入れてもう一度コンパイルを実行してみてください．もし
それでもメモリが足りなくなるようなら，なんらかの手段を講じる必要があり
ます．TeXがページを切断するかどうか決定するためには段落全体を処理しな
ければならないということを思いだしてください．段落の途中に\\newpage命
令を入れれば，段落の残りを処理する前に今のページをTeXに出力させること
で余裕ができるかもしれません(\\pagebreak命令ではだめです)．もし図や表
が溜まっていることが問題なのならば，図表をもっと後ろの方に移動するとか，
あるいはもっと前の時点で出力されるようにすれば回避できます．もしまだ文
書を作成している途中なら，とりあえず\\clearpage命令を入れておいて，最
終版を作る時までこの問題は棚上げしておきましょう．入力ファイルが変わる
と問題が解消される場合もあるのです．

pool size 
=========
相互参照の\\labelが多すぎるか，命令の定義が多すぎるかのどちらかです．
正確にいえば，定義したラベル名および命令名に使った文字数が多すぎるとい
うことです．ですから，もっと短い名前を使えばこの問題は解決します．ただ
し，このエラーは，\\setcounterなどのカウンタ命令や\\newenvironment・
\\newtheorem命令の引数の終わりを示す右括弧を忘れた場合にも生じます．

save size 
=========
このエラーは，宣言の有効範囲や命令・環境があまりにも深く入れ子になって
いる場合に生じます．たとえば，\\multiput命令の引数にpicture環境があり，
そのなかに\\footnotesize宣言があり，その宣言の有効範囲に\\multiput命令
があって，その引数に... というような場合です．")

    ("Text line contains an invalid character." .
"入力中に不正な文字が含まれています．ファイル作成の誤りによってテキスト
エディタがこの文字を挿入してしまったのでしょう．実際に何が起きたのかは
エディタによります．入力ファイルを調べてみて，指摘された文字が見つから
ない場合にはローカルガイドを見てください．")

    ("Undefined control sequence."   .  
"TeXが未定義の命令名を発見しました．おそらく入力の誤りでしょう．もしこ
のエラーがLaTeX命令の処理中に生じた場合は，その命令は間違った位置に置か
れています．例えば，リスト環境の中でないのに\\item命令が使われた場合など
です．また，\\documentstyle命令がない場合にもこのエラーが生じます．")

    ("Use of [^ ]* doesn't match its definition." .
"おそらく描画のための命令だと思われますが，引数の使いかたが間違ってい
ます．間違っているのが\\@array命令の場合は，array環境かtabular環境での
@表現の引数になにか誤りがあるのでしょう．fragileな命令が\\protectされて
いないのかもしれません．")

    ("You can't use `macro parameter character \\#' in [^ ]* mode." .
"特殊文字#が普通のテキストの中に現れました．おそらく\\#と書きたかった
のでしょう．")

    ("Overfull \\\\hbox .*" .
"行分割のための適切な場所が見つからなかったので，1行に収まるべき分量以上
の出力が行なわれてしまいました．")

    ("Overfull \\\\vbox .*" .
"ページ分割のための適切な場所が見つからなかったので，1ページに収まるべき
分量以上の出力が行なわれてしまいました．")

    ("Underfull \\\\hbox .*" .
"余分な垂直スペースがないかどうか出力を確かめてください．もしあれば，そ
れは\\\\命令または\\newline命令に関係する問題のために生じたものです．例
えば2つの\\\\命令が続いている場合などです．この警告はsloppypar環境や
\\sloppy宣言の使用，あるいは\\linebreak命令の挿入などによる場合もあります．")

    ("Underfull \\\\vbox .*" .
"ページを分割するための適切な場所が見つけられず，十分なテキストのない
ページができてしまいました．")

;; New list items should be placed here 
;; 
;; ("err-regexp" . "context") 
;; 
;; the err-regexp item should match anything

    (".*" . "ごめんなさい．該当するヘルプメッセージがありません．"))))

(provide 'tex-jp)

;;; tex-jp.el ends here
