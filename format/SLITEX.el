;;; @ SLITEX.el - Special code for SliTeX mode.
;;;
;;; $Id: SLITEX.el,v 1.1 1993-03-27 22:15:35 amanda Exp $

(require 'tex-init)
(require 'ltx-misc)

;;; @@ HOOK

(TeX-add-style-hook "SLITEX"
 (function
  (lambda ()
    (setq mode-name "SliTeX")
    (setq major-mode 'SliTeX-mode)  
    (setq TeX-command-default "SliTeX")
    (setq LaTeX-default-style "slides")
    (LaTeX-mode-initialization)
    (run-hooks 'SliTeX-mode-hook)
    (TeX-run-style-hooks "latex" "slitex"))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
