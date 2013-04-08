;;; dk.el - Setup AUCTeX for editing Danish text.

;;; Code:

(TeX-add-style-hook "dk"
 (function (lambda ()
   (run-hooks 'TeX-language-dk-hook))))

;;; dk.el ends here
