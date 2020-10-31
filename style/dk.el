;;; dk.el - Setup AUCTeX for editing Danish text.  -*- lexical-binding: t; -*-

;;; Code:

(TeX-add-style-hook "dk"
 (function (lambda ()
	     (run-hooks 'TeX-language-dk-hook)))
 TeX-dialect)

;;; dk.el ends here
