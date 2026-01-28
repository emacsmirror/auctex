;;; babel-german.el --- AUCTeX style for `babel-german.def' (v2.99)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2026-01-31
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Starting with `babel-german' v2.99, the organization of file and
;; language names has changed.  New language names are introduced which
;; all set some options and then load the file `babel-german.def'.
;; AUCTeX now follows this change and let language files load this style
;; which does the real work., i.e., special quote and hyphen strings or
;; that `"' makes the following letter an umlaut.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-quotes "font-latex" (quotes))
(declare-function font-latex-add-to-syntax-alist "font-latex" (list))
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(defun LaTeX-babel-german-key-val-options ()
  "Return key=val options for the \\germansetup macro."
  (append
   (when (seq-intersection '("german-ch"      "german-switzerland"
                             "german-ch-1901" "german-switzerland-1901")
                           (TeX-style-list))
     '(("toss" ("true" "false"))))
   '(("capsz"   ("true" "false"))
     ("nocapsz" ("true" "false"))
     ("glottomyms" ("auto" "contemporary" "legacy"))
     ("hyphenrules" ("latest" "legacy")))))

(defvar LaTeX-babel-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `babel-german'.")

(modify-syntax-entry ?\" "w" LaTeX-babel-german-mode-syntax-table)

(TeX-add-style-hook
 "babel-german"
 (lambda ()
   (TeX-add-symbols
    '("germansetup"
      (TeX-arg-key-val (LaTeX-babel-german-key-val-options))))

   (set-syntax-table LaTeX-babel-german-mode-syntax-table)
   ;; Fontification
   (when (and (eq TeX-install-font-lock 'font-latex-setup)
              (featurep 'font-latex))
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\">" "\"<" german))
     ;; Prevent "| from leading to color bleed.
     (font-latex-add-to-syntax-alist (list (cons ?\" "\\")))
     (font-latex-add-keywords '(("germansetup" "{"))
                              'function))
   (run-hooks 'TeX-language-de-hook))
 TeX-dialect)

;;; babel-german.el ends here
