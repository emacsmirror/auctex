;;; german.el - Setup AUC TeX for editing German text.

;; $Id: german.el,v 1.4 1993-09-06 22:28:34 amanda Exp $

;;; Commentary:
;;
;; `german.sty' use `"' to give next character an umlaut.

;;; Code:

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(TeX-add-style-hook "german"
 (function (lambda ()
   (TeX-ispell-change-dictionary "german")
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (setq TeX-open-quote "\"")
   (setq TeX-close-quote "\""))))

;;; german.el ends here
