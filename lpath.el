;;; This file is only used for installing AUC TeX.
;;; It is not a part of AUC TeX itself.

;; This is needed to be able to compile `powerkey.el' on Emacs 18.
(condition-case nil
    (require 'advice)
  (error (provide 'advice)
	 (defmacro defadvice (&rest ignore))))

;; Make sure we get the right files.
(setq load-path (cons "." load-path)
      TeX-lisp-directory "<none>")

