;; This is file alphanum.el, which makes AUC-TeX usable with jura.cls
;; and its style file alphanum.sty.
;;
;; Contributed by Frank Küster (frank@kuesterei.ch). The code for
;; reftex has been written by Carsten Dominik, the maintainer of
;; reftex, but all the errors are mine.

(TeX-add-style-hook "alphanum"
  (lambda ()
		(defun TeX-arg-none (arg) 
    (insert " "))
    (setq LaTeX-largest-level (LaTeX-section-level "chapter"))
		(TeX-add-symbols '("levelup" TeX-arg-none))
    (make-local-variable 'LaTeX-section-list)
    (setq LaTeX-section-list 
					'(("part" 0)
;; the levels don't make sense with alphanum, I randomly chose 0...
						("toc" 0)
						("sub" 0)))
		(setq LaTeX-section-label
			'(("part" . "part:")
				("toc" . "sec:")
				("sub" . "sec:")))
;;
;; ****************** reftex part ******************
;; this won't work in multifile documents, but at least there is
;; something.  
 
		(if (fboundp 'reftex-add-section-levels)
				(reftex-add-section-levels
				 '(("toc" .  reftex-get-section-level-alphanum)
					 ("sub" .  reftex-get-section-level-alphanum))))
		(defun reftex-get-section-level-alphanum ()
			(save-excursion										; preserve position   
				(save-match-data				 ; preserve matching data (important!)
					;; Go back to the beginning of the sectioning command
					(goto-char (match-beginning 0))
					;; Define an initial level number, depending on the current macro.
					(let* ((macro (reftex-match-string 3)) ; "toc" or "sub"
								 (lev (cond ((string= macro "toc") 1)	; min level for "toc"
														((string= macro "sub") 2)	; min level for "sub"
														(t 0)))
								 ;; Make a regular expression which will match sectioning commands
								 ;; and the levelup macro.
								 (re (concat "\\(^[^%]*\\\\levelup\\>\\)"
														 "\\|"
														 "\\(" reftex-section-regexp "\\)")))
						;; Now parse backwards for all sectioning and levelup macros,
						;; and keep track of the relative level changes.
						(while (re-search-backward re nil t)
							(cond
							 ((match-beginning 1)
								;; levelup matched, reduce level counter
								(setq lev (1- lev)))
							 ((string= (reftex-match-string 4) "toc")
								;; a toc entry, nothing changes
								)
							 ((string= (reftex-match-string 4) "sub")
								;; a sub entry, increase level counter
								(setq lev (1+ lev)))))
						;; return the level
						lev))))
))


