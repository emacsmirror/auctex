;;; @ book.el - Special code for book style.
;;;
;;; $Id: book.el,v 1.2 1993-03-17 22:11:50 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "book"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
