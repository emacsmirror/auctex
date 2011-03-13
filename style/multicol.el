;;; multicol.el --- AUCTeX style for `multicol.sty'

;; Author: Mads Jensen <mje@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2011-01-24
;; Keywords: tex

;;; Commentary:

;; This file adds support for `multicol.sty'.

;;; Code:

(TeX-add-style-hook
 "multicol"
 (lambda ()
   (LaTeX-add-environments 
    '("multicols" (lambda (env &rest ignore)
                    (LaTeX-insert-environment
                     env (format "{%i}" (read-string "Number of columns: ")))))
    '("multicols*" (lambda (env &rest ignore)
                     (LaTeX-insert-environment
                      env (format "{%i}" (read-string "Number of columns: "))))))

   ;; not sure whether stuff like \columnseprulecolor and \columnseprule
   ;; should be added, and if so, as what? I couldn't find any sensible
   ;; snippet from the other style-files which matched the criteria
   (TeX-add-symbols 
    '("columnseprule" TeX-arg-macro "Length"))

   (add-to-list 'LaTeX-indent-environment-list
		'("multicols*" current-indentation)
                '("multicols" current-indentation))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     ;; Tell font-lock about the update.
     (font-latex-add-keywords '("columnbreak") 'function)
     (font-latex-add-keywords '("columnseprule") 'function)
     (font-latex-add-keywords '("columnseprulecolor") 'function)     
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

;; there weren't any mentioned in the manual; I might have missed them
(defvar LaTeX-multicol-package-options nil
  "Package options for the multicol package.")

;;; multicol.el ends here