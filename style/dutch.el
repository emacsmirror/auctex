;;; dutch.el - Setup AUC TeX for editing Dutch text.

;; $Id: dutch.el,v 1.1 1993-09-06 22:28:29 amanda Exp $

;;; Code:

(TeX-add-style-hook "dutch"
 (function (lambda ()
   (TeX-ispell-change-dictionary "dutch"))))

;;; dutch.el ends here
