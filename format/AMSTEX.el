;;; @ AMSTEX.el - Special code for Japanese TeX mode.
;;;
;;; $Id: AMSTEX.el,v 1.1 1993-07-17 06:27:14 amanda Exp $

(require 'tex-init)
(require 'tex-misc)

;;; @@ Hook

(TeX-add-style-hook "AMSTEX"
 (function
  (lambda ()
    (TeX-run-format-hooks "TEX")
    (setq mode-name "AmSTeX")
    (setq major-mode 'AmS-TeX-mode)
    (setq TeX-command-default "AmSTeX")
    (run-hooks 'AmS-TeX-mode-hook))))
    

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
