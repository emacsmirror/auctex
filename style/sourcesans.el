;;; sourcesans.el --- AUCTeX style for `sourcesans.sty' (v3.0)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2026-03-08
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

;; This file adds support for `sourcesans.sty' (v3.0) from 2026-03-01.
;; `sourcesans.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(TeX-add-style-hook
 "sourcesans"
 (lambda ()

   ;; Load "fontspec" with package options "opentype" or "otf":
   (when (or (LaTeX-provided-package-options-member "sourcesans" "opentype")
             (LaTeX-provided-package-options-member "sourcesans" "opentype=true")
             (LaTeX-provided-package-options-member "sourcesans" "otf")
             (LaTeX-provided-package-options-member "sourcesans" "otf=true"))
     (TeX-run-style-hooks "fontspec"))

   ;; The next set of macros is only available when package "fontspec"
   ;; is loaded, by this style or by user.  We just check against
   ;; "fontspec" and do not go through a check of `TeX-engine':
   (when (member "fontspec" (TeX-style-list))
     (TeX-add-symbols
      '("sourcesans"        -1)
      '("sourcesansmedium"  -1)
      '("sourcesanslight"   -1)
      '("sourcesansextreme" -1)
      '("sourcesanslf"      -1)))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup)
              (member "fontspec" (TeX-style-list)))
     (font-latex-add-keywords '(("sourcesans"        "")
                                ("sourcesansmedium"  "")
                                ("sourcesanslight"   "")
                                ("sourcesansextreme" "")
                                ("sourcesanslf"      ""))
                              'type-declaration)))
 TeX-dialect)

(defvar LaTeX-sourcesans-package-options
  '("oldstyle" "osf"
    "lining" "nf" "lf"
    "tabular" "proportional"
    "black" "semibold" "bold"
    "light" "extralight"
    "regular" "medium"
    "scale" "scaled"
    "default" "nosfdefault" "sfdefault"
    "type1" "t1"
    "opentype" "otf")
  "Prompt for package options for the sourcesans package.")

;;; sourcesans.el ends here
