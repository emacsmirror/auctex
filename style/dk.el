;;; dk.el - Setup AUC TeX for editing Danish text.

;; $Id: dk.el,v 1.1 1993-09-06 22:28:28 amanda Exp $

;;; Code:

(TeX-add-style-hook "dk"
 (function (lambda ()
   (TeX-ispell-change-dictionary "danish"))))

;;; dk.el ends here
