;;; @ jarticle.el - Special code for jarticle style.
;;;
;;; $Id: jarticle.el,v 1.1 1993-03-27 22:15:47 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "jarticle"
 (function (lambda () (setq LaTeX-largest-level 2))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
