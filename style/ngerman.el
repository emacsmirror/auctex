;;; ngerman.el - Setup AUC TeX for editing German text.

;; $Id: ngerman.el,v 1.2 2003-03-13 22:45:32 dak Exp $

;;; Commentary:
;;
;; `ngerman.sty' use `"' to give next character an umlaut.

;;; Code:

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(defvar LaTeX-german-quote-after-quote t
  "Initial value of `TeX-quote-after-quote' for `german.el'")
(defvar LaTeX-german-open-quote "\"`"
  "Initial value of `TeX-open-quote' for `german.el'")
(defvar LaTeX-german-close-quote "\"'"
  "Initial value of `TeX-close-quote' for `german.el'")

(TeX-add-style-hook "ngerman"
 (function (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (make-local-variable 'TeX-quote-after-quote)
   (setq TeX-quote-after-quote LaTeX-german-quote-after-quote)
   (setq TeX-open-quote LaTeX-german-open-quote)
   (setq TeX-close-quote LaTeX-german-close-quote)
   (run-hooks 'TeX-language-de-hook))))

;;; ngerman.el ends here
