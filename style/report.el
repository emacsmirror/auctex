;;; report.el - Special code for report style.

;;; Code:

(TeX-add-style-hook
 "report"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; report.el ends here
