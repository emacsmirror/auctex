;;; jarticle.el - Special code for jarticle style.

;; $Id: jarticle.el,v 1.3 2005-02-07 14:14:03 ataka Exp $

;;; Code:

(TeX-add-style-hook "jarticle"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; jarticle.el ends here
