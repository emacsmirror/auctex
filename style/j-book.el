;;; @ j-book.el - Special code for j-book style.
;;;
;;; $Id: j-book.el,v 1.1 1993-03-27 22:15:45 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "j-book"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
