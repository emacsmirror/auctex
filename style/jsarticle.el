;;; jsarticle.el - Special code for jsarticle style.

;; $Id: jsarticle.el,v 1.1 2005-01-31 11:32:12 ataka Exp $

;;; Code:

(TeX-add-style-hook "jsarticle"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; jsarticle.el ends here
