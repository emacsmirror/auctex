;;; jbook.el - Special code for jbook style.

;;; Code:

(TeX-add-style-hook
 "jbook"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; jbook.el ends here
