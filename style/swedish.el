;;; swedish.el - Setup AUC TeX for editing Swedishtext.

;; $Id: swedish.el,v 1.1 1996-02-27 18:50:40 abraham Exp $

;;; Commentary:
;;
;; Apparently the Swedes use ''this style'' quotations.

(TeX-add-style-hook "swedish"
 (function (lambda ()
   (make-local-variable 'TeX-open-quote)
   (setq TeX-open-quote "''")
   (run-hooks 'TeX-language-sv-hook))))
