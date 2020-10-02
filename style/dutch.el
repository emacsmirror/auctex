;;; dutch.el - Setup AUCTeX for editing Dutch text.  -*- lexical-binding: t; -*-

;;; Code:

(TeX-add-style-hook "dutch"
 (function (lambda ()
	     (run-hooks 'TeX-language-nl-hook)))
 LaTeX-dialect)

;;; dutch.el ends here
