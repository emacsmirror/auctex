;;; @ tex-jp.el - Support for Japanese TeX.

(provide 'tex-jp)
(require 'tex-init)

;;; @@ Variables

(defvar japanese-LaTeX-style-list
  '(("jbook")
    ("j-book")
    ("jarticle")
    ("j-article")
    ("jslides")
    ("jreport")
    ("j-report"))
  "*List of Japanese document styles.")

;;; @@ Japanese LaTeX initialization

(defun japanese-TeX-initialization ()
  "Initialization for Japanese TeX."
  (if (boundp 'MULE)
      (setq TeX-after-start-process-hook
	    (function (lambda ()
			(set-process-coding-system
			 nil
			 TeX-japanese-process-input-coding-system
			 TeX-japanese-process-output-coding-system)))))
  (if (boundp 'NEMACS)
      (setq TeX-after-start-process-hook
	    (function
	     (lambda ()
	       (set-process-kanji-code nil TeX-process-kanji-code))))))
 

(defun japanese-LaTeX-initialization ()
  "Initialization for Japanese LaTeX."
  (japanese-TeX-initialization)
  (setq LaTeX-style-list japanese-LaTeX-style-list)
  (setq TeX-command-BibTeX "jBibTeX"))

;;; @@ Japanese TeX modes

(defvar japanese-plain-TeX-format-package "JTEX"
  "Name of japanese plain TeX format package (for finding symbols).")

(defun japanese-plain-tex-mode ()
  "Major mode for editing files of input for Japanese plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

See info under AUC TeX for full documentation.

Special commands:
\\{TeX-mode-map}
 
Entering japanese-plain-TeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of plain-TeX-mode-hook."
  (interactive)
  (VirTeX-mode japanese-plain-TeX-format-package))

(defvar japanese-LaTeX-format-package "JLATEX" 
  "Name of LaTeX format package.")

(defun japanese-latex-mode ()
  "Major mode for editing files of input for Japanese LaTeX.

Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.
LFD and TAB indent lines as with programming modes.

See under AUC TeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering japanese-LaTeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of LaTeX-mode-hook."
  (interactive)
  (VirTeX-mode japanese-LaTeX-format-package))

(defvar japanese-SliTeX-format-package "JSLITEX"
  "Name of japanese SliTeX format package.")

(defun japanese-slitex-mode ()
  "Major mode for editing files of input for Japanese SliTeX.

Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.
LFD and TAB indent lines as with programming modes.

See under AUC TeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering japanese-SLiTeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of LaTeX-mode-hook. and then the value of SliTeX-mode-hook."
  (interactive)
  (VirTeX-mode japanese-SliTeX-format-package))

;;; @@ MULE and NEMACS paragraph filling.

(require 'ltx-misc)

(if (boundp 'MULE)

(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "r\nP")
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
    ;;;; patch by S.Tomura 88-Jun-30
    ;;＜統合＞
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;;(while (re-search-forward "[.?!][])""']*$" nil t)
    ;;  (insert ? ))
    (while (re-search-forward "[.?!][])""']*$" nil t)
      (if (eobp)
	  nil
	;; replace CR by two spaces.
	(delete-char 1)			; delete newline
	(insert "  ")))
    ;; end of patch
    ;; The change all newlines to spaces.
    ;;; patched by S.Tomura 87-Dec-7
    ;;; bug fixed by S.Tomura 88-May-25
    ;;; modified by  S.Tomura 88-Jun-21
    ;;;(subst-char-in-region from (point-max) ?\n ?\ )
    ;;; modified by K.Handa 92-Mar-2
    ;;; Spacing is not necessary for charcters of no word-separater.
    ;;; The regexp word-across-newline is used for this check.
    (if (not (stringp word-across-newline))
	(subst-char-in-region from (point-max) ?\n ?\ )
      (goto-char from)
      (end-of-line)
      (while (not (eobp))
	(delete-char 1)
	(if (eobp) nil			; 92.6.30 by K.Handa
	  (if (looking-at word-across-newline)
	      (progn
		(forward-char -1)
		(if (looking-at word-across-newline)
		    (forward-char 1)
		  (forward-char 1)
		  (insert ? )))
	    (insert ? ))
	  (end-of-line))))
    ;;; After the following processing, there's two spaces at end of sentence
    ;;; and single space at end of line within sentence.
    ;;; end of patch
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
	  ;;; patch by K.Handa 92-Mar-2
	  (re-break-point (concat "[ \t\n]\\|" word-across-newline))
	  ;;; end of patch
	  )
      (while (not (eobp))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  ;;; patched by S.Tomura 87-Jun-2
	  ;;; Big change by K.Handa 92-Mar-2
	  ;;; Move back to start of word.
	  ;;; (skip-chars-backward "^ \n")
	  ;;; (if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	  ;;;    ;; Move back over whitespace before the word.
	  ;;;    (skip-chars-forward "^ \n")
	  ;;;  ;; Normally, move back over the single space between the words.
	  ;;;  (forward-char -1))

	  ;;; At first, find breaking point at the left of fill-column,
	  ;;; but after kinsoku-shori, the point may be right of fill-column.
	  ;;; 92.4.15 by K.Handa -- re-search-backward will back to prev line.
	  ;;; 92.4.27 by T.Enami -- We might have gone back too much...
	  (let ((p (point)) ch)
	    (re-search-backward re-break-point nil 'mv)
	    (setq ch (following-char))
	    (if (or (= ch ? ) (= ch ?\t))
		(skip-chars-backward " \t")
	      (forward-char 1)
	      (if (<= p (point))
		  (forward-char -1))))
	  (kinsoku-shori)
	  ;;; Check if current column is at the right of prefixcol.
	  ;;; If not, find break-point at the right of fill-column.
	  ;;; This time, force kinsoku-shori-nobashi.
	  (if (>= prefixcol (current-column))
	      (progn
		(move-to-column (1+ fill-column))
		;; 92.4.15 by K.Handa -- suppress error in re-search-forward
		(re-search-forward re-break-point nil t)
		(forward-char -1)
		(kinsoku-shori-nobashi))))
	;;; end of patch S.Tomura

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
	;;; end of patch
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
      (delete-horizontal-space)))))

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
    ;;;; patch by S.Tomura 88-Jun-30
    ;;＜統合＞
    ;; . + CR             ==> . + SPC + SPC 
    ;; . + SPC + CR +     ==> . + SPC + 
    ;;(while (re-search-forward "[.?!][])""']*$" nil t)
    ;;  (insert ? ))
    (while (re-search-forward "[.?!][])""']*$" nil t)
      (if (eobp)
	  nil
      (delete-char 1)
      (insert "  "))) ;;; replace CR by two spaces.
    ;;; end of patch
    ;; The change all newlines to spaces.
    ;;; patched by S.Tomura 87-Dec-7
    ;;; bug fixed by S.Tomura 88-May-25
    ;;; modified by  S.Tomura 88-Jun-21
    ;;;(subst-char-in-region from (point-max) ?\n ?\ )
    ;;;日本語の語の後には空白はない。
    (goto-char from)
    (end-of-line)
    (while (not (eobp))
      (delete-char 1)
      (if (and (< ?  (preceding-char)) ;;; + SPC + CR + X ==> + SPC + X
	       (< (preceding-char) 128)
	       (<= ?  (following-char))
	       (< (following-char) 128))
	   (insert ?\  ))
      (end-of-line))
    ;;; 次の処理で文末にはtwo spacesがあり、それ以外はsingle spaceになっている。
    ;;; end of patch
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
	;;; patched by S.Tomura 88-Jun-2
	;;;(move-to-column (1+ fill-column))
	(move-to-column fill-column)
	;;; end of patch
	;;; patched by S.Tomura 88-Jun-16, 89-Oct-2, 89-Oct-19
	;;; 漢字コードの場合にはfill-columnより大きくなることがある。
	(or (>= fill-column (current-column)) (backward-char 1))
	;;; end of patch
	(if (eobp)
	    nil
	  ;;; patched by S.Tomura 87-Jun-2
	  ;;;(skip-chars-backward "^ \n")
	  ;;;(if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	  ;;;    (skip-chars-forward "^ \n")
	  ;;;  (forward-char -1)))
	  ;;; 原則としてfill-columnより左側に分割点を探す。
	  ;;; Find a point to break lines
	     (skip-chars-backward " \t") ;; skip SPC and TAB
	     (if (or (<= 128 (preceding-char))
		     (<= 128 (following-char)) ;;; 88-Aug-25
		     (= (following-char) ? )
		     (= (following-char) ?\t))
		 (kinsoku-shori)
	       (if(re-search-backward "[ \t\n]\\|\\z" ;;; 89-Nov-17
				      (point-min) (point-min))
		   (forward-char 1))
	       (skip-chars-backward " \t")
	       (kinsoku-shori))
	     ;;; prifixcolより右側に分割点を探す。
	     ;;; この場合は分割点はfill-columnより右側になる。
	     (if (>= prefixcol (current-column))
		 (progn
		   (move-to-column prefixcol)
		   (if (re-search-forward "[ \t]\\|\\z" ;;; 89-Nov-17
					  (point-max) (point-max))
		       (backward-char 1))
		   (skip-chars-backward " \t")
		   (kinsoku-shori)
		   ;;; それも駄目なら分割を諦める。
		   (if (>= prefixcol (current-column)) (goto-char (point-max))))))
	;;; end of patch S.Tomura
	;;; patch by S. Tomura 88-Jun-20
	;;;(delete-horizontal-space)
        ;;;＜分割＞
        ;;; 全角 | SPC + SPC＊   --> 全角 + SPC + CR
	;;; | SPC + SPC* + 全角  --> SPC  + CR + 全角
        ;;; . | SPC + SPC +      --> . + CR
        ;;; . | SPC + nonSPC     --> . + SPC + CR + nonSPC
        ;;;
        ;;; . | 半角             --> 分割しない
        ;;; . | 全角             --> 分割しない
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
	;;; end of patch
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

;;; @@ Emacs

(run-hooks 'TeX-after-tex-jp-hook)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
