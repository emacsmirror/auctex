;;; @ german.el - Special use of double quotes.
;;;
;;; $Id: german.el,v 1.3 1993-07-27 18:53:36 amanda Exp $

;;; @@ Hook

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(TeX-add-style-hook "german"
 (function (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (setq TeX-open-quote "\"")
   (setq TeX-close-quote "\""))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
