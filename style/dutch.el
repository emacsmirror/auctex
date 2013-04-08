;;; dutch.el - Setup AUCTeX for editing Dutch text.

;;; Code:

(TeX-add-style-hook "dutch"
 (function (lambda ()
   (run-hooks 'TeX-language-nl-hook))))

;;; dutch.el ends here
