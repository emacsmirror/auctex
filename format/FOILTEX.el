;;; @ FOILTEX.el - Special code for FoilTeX mode.
;;;
;;; $Id: FOILTEX.el,v 1.1 1993-04-01 02:35:06 amanda Exp $

(require 'tex-init)
(require 'ltx-misc)

;;; @@ HOOK

(TeX-add-style-hook "FOILTEX"
 (function
  (lambda ()
    (setq mode-name "FoilTeX")
    (setq major-mode 'FoilTeX-mode)  
    (setq TeX-command-default "FoilTeX")
    (LaTeX-mode-initialization)
    (run-hooks 'FoilTeX-mode-hook)
    (TeX-run-style-hooks "latex" "foiltex"))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
