;;; lua-unicode-math.el --- AUCTeX style for `lua-unicode-math' v0.7  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2026-02-04
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

;; This file adds support for `lua-unicode-math.sty' (v0.7) from
;; 2026-02-03.  `lua-unicode-math.sty' is part of TeXlive.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(TeX-add-style-hook
 "lua-unicode-math"
 (lambda ()

   ;; Run only with luatex:
   (TeX-check-engine-add-engines 'luatex)

   (TeX-add-symbols
    '("setmathfont"
      (TeX-arg-completing-read ("Latin Modern Math"
                                "New Computer Modern Math"
                                "New Computer Modern Sans Math"
                                "STIX2"
                                "XITS"
                                "TeX Gyre Pagella Math"
                                "TeX Gyre DejaVu Math"
                                "TeX Gyre Bonum Math"
                                "TeX Gyre Schola Math"
                                "TeX Gyre Termes Math"
                                "Fira Math"
                                "GFS Neohellenic Math"
                                "Erewhon Math"
                                "XCharter Math"
                                "Concrete Math")
                               "Math font name")
      (TeX-arg-conditional (member "fontspec" (TeX-style-list))
          ([TeX-arg-key-val (LaTeX-fontspec-font-features) "Font features"])
        (["Font features"]))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("setmathfont" "[{["))
                              'function)))
 TeX-dialect)

(defvar LaTeX-lua-unicode-math-package-options nil
  "Package options for the lua-unicode-math package.")

;;; lua-unicode-math.el ends here
