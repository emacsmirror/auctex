;;; @ german.el - Special use of double quotes.
;;;
;;; $Id: german.el,v 1.1 1993-04-17 04:53:23 amanda Exp $

;;; @@ Hook

(TeX-add-style-hook "german"
 (function (lambda ()
   ;; Only change keymap for this buffer
   (if (eq (current-local-map) LaTeX-mode-map)
       (use-local-map (copy-keymap LaTeX-mode-map)))
   ;; Use " Literally
   (local-set-key "\"" 'self-insert-command))))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
