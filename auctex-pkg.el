;; -*- no-byte-compile: t -*-

(define-package "auctex" "12.1.1" "Integrated environment for *TeX*"
  '((emacs "24.1") (cl-lib "0.5"))
  :url "https://www.gnu.org/software/auctex/"
  :keywords '("TeX" "LaTeX" "Texinfo" "ConTeXt" "docTeX"
	      "preview-latex"))

(setq byte-compile-warnings '(not unresolved))
