;;; tbook.el - Special code for tbook class.

;;; Code:

(TeX-load-style "jbook")
(defvar LaTeX-tbook-class-options LaTeX-jbook-class-options
  "Class options for the tbook class.")

(TeX-add-style-hook
 "tbook"
 (lambda ()
   (TeX-run-style-hooks "jbook" "plext"))
 LaTeX-dialect)

;;; tbook.el ends here
