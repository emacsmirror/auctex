;;; @ jreport.el - Special code for jreport style.
;;;
;;; $Id: jreport.el,v 1.1 1993-07-07 00:41:46 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "jreport"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
