;;; jsbook.el - Special code for jsbook style.

;; $Id: jsbook.el,v 1.1 2005-01-31 11:32:38 ataka Exp $

;;; Code:

(TeX-add-style-hook "jsbook"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; jsbook.el ends here
