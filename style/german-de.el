;;; german-de.el --- AUCTeX style for `german-de.ldf' (v2.99)  -*- lexical-binding: t; -*-

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

;; This file contains specific code for `german-de' language option
;; provided by `babel-german' bundle.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "german-de"
 (lambda ()
   (TeX-run-style-hooks "babel-german")
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language '("german-de" "\"`" "\"'" t)))
   (setq LaTeX-babel-hyphen-language "german-de"))
 TeX-dialect)

;;; german-de.el ends here
