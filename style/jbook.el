;;; @ jbook.el - Special code for jbook style.
;;;
;;; $Id: jbook.el,v 1.1 1993-03-27 22:15:49 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "jbook"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
