;;; @ AMSLATEX.el - Special code for AmSLaTeX mode.
;;;
;;; $Id: AMSLATEX.el,v 1.1 1993-07-17 06:27:13 amanda Exp $

(require 'tex-init)
(require 'ltx-misc)

;;; @@ HOOK

(TeX-add-style-hook "AMSLATEX"
 (function
  (lambda ()
    (setq mode-name "AmSLaTeX")
    (setq major-mode 'AmS-LaTeX-mode)  
    (setq TeX-command-default "AmSLaTeX")
    (setq LaTeX-default-style "amsart")
    (LaTeX-mode-initialization)
    (run-hooks 'AmS-TeX-mode-hook 'AmS-LaTeX-mode-hook)
    (TeX-run-style-hooks "latex"))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
