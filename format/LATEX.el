;;; @ LATEX.el - Special code for LaTeX mode.
;;;
;;; $Id: LATEX.el,v 1.1 1993-03-27 22:15:33 amanda Exp $

(require 'tex-init)
(require 'ltx-misc)

;;; @@ HOOK

(TeX-add-style-hook "LATEX"
 (function
  (lambda ()
    (setq mode-name "LaTeX")
    (setq major-mode 'LaTeX-mode)  
    (setq TeX-command-default "LaTeX")
    (LaTeX-mode-initialization)
    (TeX-run-style-hooks "latex"))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
