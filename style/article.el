;;; article.el - Special code for article style.

;;; Code:

(TeX-add-style-hook
 "article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; article.el ends here
