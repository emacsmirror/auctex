;;; @ book.el - Special code for book style.
;;;
;;; $Id: book.el,v 1.1 1993-03-15 18:12:54 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "book"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level ("chapter"))))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
