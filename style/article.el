;;; @ article.el - Special code for article style.
;;;
;;; $Id: article.el,v 1.1 1993-03-15 18:12:52 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "article"
 (function (lambda () (setq LaTeX-largest-level 2))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
