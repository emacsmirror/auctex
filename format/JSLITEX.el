;;; @ JSLITEX.el - Special code for Japanese SliTeX mode.
;;;
;;; $Id: JSLITEX.el,v 1.1 1993-03-27 22:15:30 amanda Exp $

(require 'tex-init)
(require 'ltx-misc)
(require 'tex-jp)

;;; @@ HOOK

(TeX-add-style-hook "JSLITEX"
 (function
  (lambda ()
    (TeX-run-format-hooks "SLITEX")
    (japanese-LaTeX-initialization)
    (setq mode-name "jSliTeX")
    (setq major-mode 'japanese-SliTeX-mode)
    (setq TeX-command-default "jSliTeX")
    (setq LaTeX-default-style "jslides"))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
