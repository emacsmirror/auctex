;;; @ JLATEX.el - Special code for Japanese LaTeX mode.
;;;
;;; $Id: JLATEX.el,v 1.2 1993-04-17 20:29:02 amanda Exp $

(require 'tex-init)
(require 'ltx-misc)
(require 'tex-jp)

;;; @@ HOOK

(TeX-add-style-hook "JLATEX"
 (function
  (lambda ()
    (TeX-run-format-hooks "LATEX")
    (japanese-LaTeX-initialization)
    (setq mode-name "jLaTeX")
    (setq major-mode 'japanese-LaTeX-mode)  
    (setq TeX-command-default "jLaTeX"))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
