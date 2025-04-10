;;; geometry.el --- AUCTeX style for `geometry.sty' (v5.6)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-02-21
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

;; This file adds support for `geometry.sty' (v5.6) from 2010/09/12.
;; `geometry.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-geometry-always-key-val-options
  '(("layout" ("a0paper" "a1paper" "a2paper" "a3paper" "a4paper"
               "a5paper" "a6paper" "b0paper" "b1paper" "b2paper"
               "b3paper" "b4paper" "b5paper" "b6paper" "c0paper"
               "c1paper" "c2paper" "c3paper" "c4paper" "c5paper"
               "c6paper" "b0j" "b1j" "b2j" "b3j" "b4j" "b5j" "b6j"
               "ansiapaper" "ansibpaper" "ansicpaper" "ansidpaper"
               "ansiepaper"))
    ("layoutwidth") ("layoutheight") ("layoutsize")
    ("layouthoffset") ("layoutvoffset") ("layoutoffset") ("hscale")
    ("vscale") ("scale") ("width") ("totalwidth") ("height") ("totalheight")
    ("total") ("textwidth") ("textheight") ("text") ("body") ("lines")
    ("includehead") ("includefoot") ("includeheadfoot") ("includemp")
    ("includeall") ("ignorehead") ("ignorefoot") ("ignoreheadfoot")
    ("ignoremp") ("ignoreall") ("heightrounded") ("hdivide") ("vdivide")
    ("divide") ("left") ("lmargin") ("inner") ("right") ("rmargin")
    ("outer") ("top") ("tmargin") ("bottom") ("bmargin") ("hmargin")
    ("vmargin") ("margin") ("hmarginratio") ("vmarginratio") ("marginratio")
    ("ratio") ("hcentering") ("vcentering") ("centering") ("twoside")
    ("asymmetric") ("bindingoffset") ("hdivide") ("vdivide") ("divide")
    ("headheight") ("head") ("headsep") ("footskip") ("foot") ("nohead")
    ("nofoot") ("noheadfoot") ("footnotesep") ("marginparwidth") ("marginpar")
    ("marginparsep") ("nomarginpar") ("columnsep") ("hoffset") ("voffset")
    ("offset") ("twocolumn") ("onecolumn") ("twoside") ("textwidth")
    ("textheight") ("reversemp") ("reversemarginpar"))
  "Key=value options always available for geometry macros.")

(defvar LaTeX-geometry-preamble-key-val-options
  '(("paper" ("a0paper" "a1paper" "a2paper" "a3paper" "a4paper"
              "a5paper" "a6paper" "b0paper" "b1paper" "b2paper"
              "b3paper" "b4paper" "b5paper" "b6paper" "c0paper"
              "c1paper" "c2paper" "c3paper" "c4paper" "c5paper"
              "c6paper" "b0j" "b1j" "b2j" "b3j" "b4j" "b5j" "b6j"
              "ansiapaper" "ansibpaper" "ansicpaper" "ansidpaper"
              "ansiepaper"))
    ("papername" ("a0paper" "a1paper" "a2paper" "a3paper" "a4paper"
                  "a5paper" "a6paper" "b0paper" "b1paper" "b2paper"
                  "b3paper" "b4paper" "b5paper" "b6paper" "c0paper"
                  "c1paper" "c2paper" "c3paper" "c4paper" "c5paper"
                  "c6paper" "b0j" "b1j" "b2j" "b3j" "b4j" "b5j" "b6j"
                  "ansiapaper" "ansibpaper" "ansicpaper" "ansidpaper"
                  "ansiepaper"))
    ("a0paper") ("a1paper") ("a2paper") ("a3paper") ("a4paper") ("a5paper")
    ("a6paper") ("b0paper") ("b1paper") ("b2paper") ("b3paper") ("b4paper")
    ("b5paper") ("b6paper") ("c0paper") ("c1paper") ("c2paper") ("c3paper")
    ("c4paper") ("c5paper") ("c6paper") ("b0j") ("b1j") ("b2j") ("b3j")
    ("b4j") ("b5j") ("b6j") ("ansiapaper") ("ansibpaper") ("ansicpaper")
    ("ansidpaper") ("ansiepaper") ("screen") ("paperwidth") ("paperheight")
    ("papersize") ("landscape") ("portrait")
    ("driver" ("dvips" "dvipdfm" "dvipdfmx" "xdvipdfmx"
               "pdftex" "luatex" "vtex" "xetex" "auto" "none"))
    ("dvips") ("dvipdfm") ("dvipdfmx") ("xdvipdfmx") ("pdftex") ("luatex")
    ("xetex") ("vtex") ("verbose") ("reset")
    ("mag") ("truedimen") ("pass") ("showframe") ("showcrop"))
  "Key=value options allowed only in the preamble for geometry macros.")

;; Setup for \savegeometry:
(TeX-auto-add-type "geometry-savegeometry" "LaTeX" "geometry-savegeometries")

(defvar LaTeX-geometry-savegeometry-regexp
  '("\\\\savegeometry{\\([^}]+\\)}"
    1 LaTeX-auto-geometry-savegeometry)
  "Matches the argument of `\\savegeometry' from `geometry'
package.")

(defun LaTeX-geometry-auto-prepare ()
  "Clear `LaTeX-auto-geometry-savegeometry' before parsing."
  (setq LaTeX-auto-geometry-savegeometry nil))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-geometry-auto-prepare t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "geometry"
 (lambda ()

   ;; Add geometry to the parser.
   (TeX-auto-add-regexp LaTeX-geometry-savegeometry-regexp)

   ;; geometry commands:
   (TeX-add-symbols
    `("geometry"
      (TeX-arg-key-val ,(append LaTeX-geometry-preamble-key-val-options
                                LaTeX-geometry-always-key-val-options)))
    '("newgeometry"
      (TeX-arg-key-val LaTeX-geometry-always-key-val-options))

    '("restoregeometry" 0)

    `("savegeometry"
      ,(lambda (optional)
         (let ((name (TeX-read-string
                      (TeX-argument-prompt optional nil "Name"))))
           (LaTeX-add-geometry-savegeometries name)
           (TeX-argument-insert name optional))))

    '("loadgeometry"
      (TeX-arg-completing-read (LaTeX-geometry-savegeometry-list)
                               "Name")))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("geometry"      "{")
                                ("newgeometry"   "{")
                                ("restoregeometry" "")
                                ("savegeometry"  "{")
                                ("loadgeometry"  "{"))
                              'function))

   ;; Option management
   (if (and (LaTeX-provided-package-options-member "geometry" "dvipdfmx")
            (not (eq TeX-engine 'xetex)))
       (setq TeX-PDF-from-DVI "Dvipdfmx")))
 TeX-dialect)

(defvar LaTeX-geometry-package-options-list
  (append LaTeX-geometry-preamble-key-val-options
          LaTeX-geometry-always-key-val-options)
  "Package options for the geometry package.")

(defun LaTeX-geometry-package-options ()
  "Prompt for package options for the geometry package."
  (TeX-read-key-val t LaTeX-geometry-package-options-list))

;;; geometry.el ends here
