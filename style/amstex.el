;;; amstex.el --- AMS LaTeX support.

;;; Code:

(TeX-add-style-hook "amstex"
 (function
  (lambda ()
    (TeX-add-symbols
     '("eqref" TeX-arg-label)))))

;;; amstex.el ends here.
