;;; @ slitex.el - Special code for slitex.
;;;
;;; $Id: slitex.el,v 1.4 1993-03-23 11:40:33 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "slitex"
 (function
  (lambda ()
    (or (memq 'LaTeX-slides-hook LaTeX-ducument-style-hook)
	(setq LaTeX-ducument-style-hook
	      (cons 'LaTeX-slides-hook LaTeX-ducument-style-hook)))
    (LaTeX-add-environments '("slide" LaTeX-slide-hook)
			    '("overlay" LaTeX-slide-hook)))))

;;; @@ Help

(defvar LaTeX-slide-color "" 
  "*Default slide color.")

 (make-variable-buffer-local 'LaTeX-slide-color)

(defun LaTeX-slides-hook ()
  "Prompt for and insert SliTeX options."
  (let ((slide-file (read-input "Slide file: "))
	(slide-colors (read-input "Slide colors (comma separetade list): "
				  "black")))
    (save-excursion
      (goto-char (point-min))		; insert before \end{document}
      (if (re-search-forward ".end.document." (point-max) t)
	  (beginning-of-line 1))
      (open-line 2)
      (indent-relative-maybe)
      (if (equal slide-colors "black")
	  (insert TeX-esc "blackandwhite"
		  TeX-grop slide-file TeX-grcl)
	(progn
	  (insert TeX-esc "colors"
		  TeX-grop slide-colors TeX-grcl)
	  (newline-and-indent)
	  (insert TeX-esc "colorslides"
		  TeX-grop slide-file TeX-grcl))))))

(defun LaTeX-slide-hook (environment)
  "Insert ENVIRONMENT and prompt for slide colors."
  (setq LaTeX-slide-color
	(read-input "Slide colors: " LaTeX-slide-color))
  (LaTeX-insert-environment environment
			    (concat TeX-grop LaTeX-slide-color TeX-grcl)))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
