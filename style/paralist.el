;;; -*- emacs-lisp -*-
;;; paralist.el -- AUCTeX style for paralist.sty

;; License:  GPL, see the file COPYING in the base directory of AUCTeX
;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Created:  2003-10-22
;; Keywords: tex

;;; Commentary:

;; This file adds support for `paralist.sty'.

;; This file is intended to be used with the AUCTeX package. Put this
;; File into your TeX-style-path. You may also byte-compile this file.

;;; Code:

;; Insert an itemize-ish environment and ask for an optional label
(defun pl-LaTeX-env-item-opt-label (env &rest ignore)
  (LaTeX-insert-environment
   env
   (let ((label (read-string "(Optional) Label: ")))
     (concat (unless (zerop (length label))
               (format "[%s]" label)))))
  (LaTeX-find-matching-begin)
  (end-of-line 1)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(TeX-add-style-hook
 "paralist"
 (lambda ()

   ;; Add compactdesc to the list of environments which have an optional
   ;; argument for each item.
   (add-to-list 'LaTeX-item-list '("compactdesc" . LaTeX-item-argument))

   ;; New symbols
   (TeX-add-symbols
    '("pointedenum")
    '("pointlessenum")
    '("paradescriptionlabel")
    '("setdefaultitem" "First level" "Second level" "Third level"
      "Fourth level")
    '("setdefaultenum" "First level" "Second level" "Third level"
      "Fourth level")
    '("setdefaultleftmargin" "First level" "Second level" "Third level"
      "Fourth level" "Fifth level" "Sixth level"))

   ;; New environments
   (LaTeX-add-environments
    '("asparaenum" pl-LaTeX-env-item-opt-label)
    '("inparaenum" pl-LaTeX-env-item-opt-label)
    '("compactenum" pl-LaTeX-env-item-opt-label)
    '("asparaitem" pl-LaTeX-env-item-opt-label)
    '("inparaitem" pl-LaTeX-env-item-opt-label)
    '("compactitem" pl-LaTeX-env-item-opt-label)
    '("compactdesc" LaTeX-env-item)
    ;; Only defined when package is loaded with option `defblank':
    '("asparablank" LaTeX-env-item)
    '("inparablank" LaTeX-env-item))

   ;; Fontification
   (setq font-latex-match-variable-keywords-local
         (append font-latex-match-variable-keywords-local
                 '("setdefaultitem"
                   "setdefaultenum"
                   "setdefaultleftmargin")))
   (font-latex-match-variable-make)))


;;; paralist.el ends here