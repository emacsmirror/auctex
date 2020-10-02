;;; j-report.el - Special code for j-report style.  -*- lexical-binding: t; -*-

;;; Code:

(TeX-add-style-hook
 "j-report"
 (lambda ()
   (LaTeX-largest-level-set "chapter"))
 LaTeX-dialect)

;;; j-report.el ends here
