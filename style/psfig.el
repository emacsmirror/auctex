;;; psfig.el - Support for the psfig style option.

;; Contributed by Marc Gemis <makke@wins.uia.ac.be>

;;; Code: 

(TeX-add-style-hook "psfig"
 (function
  (lambda ()
	;; probable some of the following symbols may be removed
    (TeX-add-symbols "protect" "figurepath"  "fbox"
		     "other" "letter" "other" "then" "Sine" "Cosine"
		     "psdraft" "psfull" "psscalefirst" "psrotatefirst"
		     "psnodraftbox" "psdraftbox" "pssilent" "psnoisy"
		     "minmaxtest"
     '("psfigurepath" t)
		     )
    (LaTeX-add-environments
     '("psfig" LaTeX-env-psfigure)
     )
    )))

(defun LaTeX-env-psfigure (environment)
  "Create  with \\label and \\caption and \\psfig
commands."
  (let ((float (read-input "Float to: " LaTeX-float))
	(caption (read-input "Caption: "))
	(label (read-input "Label: " LaTeX-figure-label))
        ; gf: ask if this should be centered
	(psfile (read-file-name "PS-file: " "" "" nil))
	(figwidth (read-input "Figure width: "))
	(figheight (read-input "Figure height: "))
	)

    (setq LaTeX-float (if (zerop (length float))
			  LaTeX-float
			float))

    (LaTeX-insert-environment "figure"
			      (concat LaTeX-optop LaTeX-float LaTeX-optcl))

    (if (or (zerop (length label))
	    (and (string= "figure" environment)
		 (equal LaTeX-figure-label label))
	    (and (string= "table" environment)
		 (equal LaTeX-table-label label)))
	()
      (newline-and-indent)
      (insert TeX-esc "label" TeX-grop label TeX-grcl)
      (end-of-line 0)
      (LaTeX-indent-line))

    (if (zerop (length caption))
	()
      (newline-and-indent)
      (insert TeX-esc "caption" TeX-grop caption TeX-grcl))

    (newline-and-indent)
    (insert TeX-esc "centerline" TeX-grop TeX-esc "psfig" TeX-grop
	    "figure=" psfile)
    (if (not (zerop (length figwidth)))
	(insert ",width=" figwidth))
    (if (not (zerop (length figheight)))
	(insert ",width=" figheight))
    (insert TeX-grcl TeX-grcl)
    (forward-line 5)))

;;; psfig.el ends here
