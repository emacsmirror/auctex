;;; @ JTEX.el - Special code for Japanese TeX mode.
;;;
;;; $Id: JTEX.el,v 1.1 1993-03-27 22:15:31 amanda Exp $

(require 'tex-init)
(require 'tex-misc)
(require 'tex-jp)

;;; @@ Hook

(TeX-add-style-hook "JTEX"
 (function
  (lambda ()
    (TeX-run-format-hooks "TEX")
    (setq mode-name "jTeX")
    (setq major-mode 'japanese-plain-TeX-mode)
    (setq TeX-command-default "jTeX")
    (japanese-TeX-initialization))))
    

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
