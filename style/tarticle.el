;;; tarticle.el - Special code for tarticle class.

;;; Code:

(TeX-load-style "jarticle")
(defvar LaTeX-tarticle-class-options LaTeX-jarticle-class-options
  "Class options for the tarticle class.")

(TeX-add-style-hook
 "tarticle"
 (lambda ()
   (TeX-run-style-hooks "jarticle" "plext"))
 LaTeX-dialect)

;;; tarticle.el ends here
