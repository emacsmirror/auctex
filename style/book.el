;;; book.el - Special code for book style.

;;; Code:

(TeX-add-style-hook
 "book"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; book.el ends here
