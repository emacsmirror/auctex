;;; @ JLATEX.el - Special code for Japanese LaTeX mode.
;;;
;;; $Id: JLATEX.el,v 1.1 1993-03-27 22:15:28 amanda Exp $

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
    (setq TeX-command-default "jLaTeX")
    (setq LaTeX-default-style "j-article"))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
