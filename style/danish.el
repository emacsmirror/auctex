;;; danish.el - Setup AUC TeX for editing Danish text.

;; $Id: danish.el,v 1.1 1997-03-17 12:36:17 abraham Exp $

;;; Code:

(TeX-add-style-hook "danish"
 (function (lambda ()
   (run-hooks 'TeX-language-dk-hook))))

;;; danish.el ends here
