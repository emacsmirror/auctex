;;; @ german.el - Special use of double quotes.
;;;
;;; $Id: german.el,v 1.2 1993-07-16 04:34:40 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "german"
 (function (lambda ()
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
