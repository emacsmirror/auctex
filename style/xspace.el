;;; xspace.el --- AUCTeX style for `xspace.sty'

;; Author: Mads Jensen <mje@inducks.org>
;; Created: 2011-02-01
;; Keywords: tex

;;; Commentary:

;; This file adds support for `xspace.sty'.

;;; Code:

(TeX-add-style-hook
 "xspace"
 (lambda ()
   (TeX-add-symbols 
    '("xspace" TeX-insert-xspace)
    "xspaceaddexception"
    "xspaceremoveexception")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '("xspace") 'function)
     (font-latex-add-keywords '("xspaceaddexception") 'function)
     (font-latex-add-keywords '("xspaceremoveexception") 'function)

     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

(defvar LaTeX-xspace-package-options nil
  "Package options for the alltt package.")

(defun TeX-xspace-insert ()
  "Insert \\xspace without brackets."
  (insert "\\xspace"))

;;; xspace.el ends here
