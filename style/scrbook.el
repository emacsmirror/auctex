;;; -*- emacs-lisp -*-
;;; scrbook.el -- AUC TeX style for scrbook.cls

;; Copyright (C) 2002 Mark Trettin
;; License: GPL, see the file COPYING in the base directory of AUC TeX

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-09-26
;; Version: $Id: scrbook.el,v 1.3 2003-11-17 13:14:30 angeli Exp $
;; Keywords: tex

;;; Commentary: 

;; This file adds support for `scrbook.cls'. This file needs
;; `scrbase.el'.

;; This file is intended to be used with the AUC TeX-Package by Per
;; Abrahamsen. Put this File into your TeX-style-path. You may also
;; byte-compile this file. 

;;; Code:
(TeX-add-style-hook "scrbook"
  (lambda ()
    (setq LaTeX-largest-level (LaTeX-section-level "chapter"))
    ;; load basic definitons
    (TeX-run-style-hooks "scrbase")
    (TeX-add-symbols
     "chapapp"
     "raggeddictum"
     '("chapappifchapterprefix" "Additional text")
     '("setpartpreamble" [ TeX-arg-KOMA-setpreamble ] [ "Width" ] t)
     '("setchapterpreamble" [ TeX-arg-KOMA-setpreamble ] [ "Width" ] t)
     '("dictum" [ "Author" ] t))
    (make-local-variable 'LaTeX-section-list)
    (setq LaTeX-section-list (append
			      LaTeX-section-list
			      '(("addchap" 1))))
    (make-local-variable 'LaTeX-section-label)
    (setq LaTeX-section-label (append
			       LaTeX-section-label
			       '(("addchap" . nil))))
    ;; Definitions for font-latex
    ;; Textual keywords
    (setq font-latex-match-textual-keywords-local
          (append font-latex-match-textual-keywords-local
                  '("addchap"
                    "setpartpreamble"
                    "setchapterpreamble"
                    "dictum")))
    (font-latex-match-textual-make)

    ;; Title keywords
    (add-to-list 'font-latex-match-title-1-keywords-local "addchap")
    (font-latex-match-title-1-make)))

;;; scrbook.el ends here
