;;; LATEX.el - Special code for LaTeX mode.

;; $Id: LATEX.el,v 1.2 1993-09-06 22:28:09 amanda Exp $

(require 'tex-init)
(require 'ltx-misc)

(TeX-add-style-hook "LATEX"
 (function
  (lambda ()
    (setq mode-name "LaTeX")
    (setq major-mode 'latex-mode)  
    (setq TeX-command-default "LaTeX")
    (LaTeX-mode-initialization)
    (TeX-run-style-hooks "latex"))))

;;; LATEX.el ends here
