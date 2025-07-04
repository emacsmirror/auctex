;;; dcolumn.el --- AUCTeX style for `dcolumn.sty' (v1.06)  -*- lexical-binding: t; -*-

;; Copyright (C) 2016--2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2016-12-18
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

;; This file adds support for `dcolumn.sty' (v1.06) from 2014/10/28.
;; `dcolumn.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "dcolumn"
 (lambda ()

   ;; `dcolumn.sty' adds one new column specification letter:
   (setq-local LaTeX-array-column-letters
               (concat LaTeX-array-column-letters "D"))

   ;; Also run style hook for `array':
   (TeX-run-style-hooks "array"))
 TeX-dialect)

(defvar LaTeX-dcolumn-package-options nil
  "Package options for the dcolumn package.")

;;; dcolumn.el ends here
