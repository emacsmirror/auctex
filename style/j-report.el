;;; @ j-report.el - Special code for j-report style.
;;;
;;; $Id: j-report.el,v 1.1 1993-07-07 00:41:45 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "j-report"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
