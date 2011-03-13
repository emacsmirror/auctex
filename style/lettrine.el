;;; lettrine.el --- AUCTeX style for `lettrine.sty'

;; Author: Mads Jensen <mje@inducks.org>
;; Keywords: tex

;;; Commentary:

;; This file adds support for `lettrine.sty'.

;;; Code:

(defvar LaTeX-lettrine-key-val-options
  '(("lines")
    ("lhang")
    ("loversize")
    ("lraise")
    ("findent")
    ("nindent")
    ("slope")
    ("ante")
    ("image"))
  "Key=value options for \\lettrine")

(TeX-add-style-hook
 "lettrine"
 (lambda ()
   (TeX-add-symbols
    '("lettrine" [ TeX-arg-key-val LaTeX-lettrine-key-val-options ]
      "Letter" "Text")
    '("LettrineImageFalse" 0)
    ;; all of the below can be configured with either \setlength or
    ;; \renewcommand
    "LettrineFont"
    "LettrineFontHook"
    "LettrineText"
    "LettrineWidth"
    "DefaultLhang"
    "DefaultLines"
    "DefaultLoversize"
    "DefaultLraise"
    "DefaultFindent"
    "DefaultNindent"
    "DefaultSlope"
    ;; above settings can also be input a file, and pointed to with
    ;; \renewcommand
    "DefaultOptionsFile")
   
   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("lettrine" "[{{")) 'function)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

(defvar LaTeX-lettrine-package-options nil
  "Package options for the lettrine package.")

;;; lettrine.el ends here