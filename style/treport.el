;;; treport.el - Special code for treport class.

;;; Code:

(TeX-load-style "jreport")
(defvar LaTeX-treport-class-options LaTeX-jreport-class-options
  "Class options for the treport class.")

(TeX-add-style-hook
 "treport"
 (lambda ()
   (TeX-run-style-hooks "jreport" "plext"))
 LaTeX-dialect)

;;; treport.el ends here
