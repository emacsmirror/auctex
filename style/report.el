;;; @ report.el - Special code for report style.
;;;
;;; $Id: report.el,v 1.1 1993-07-07 00:41:49 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "report"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
